{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import Control.Monad.Logger                 (liftLoc, runLoggingT, LoggingT(..))
#if defined(USE_SQLITE)
import Database.Persist.Sqlite              (createSqlitePool, runSqlPool,
                                             sqlDatabase, sqlPoolSize)
#elif defined(USE_MYSQL)
import Database.Persist.MySQL               (createMySQLPool, myConnInfo,
                                             myPoolSize, runSqlPool)
#elif defined(USE_POSTGRESQL)
import Database.Persist.Postgresql          (createPostgresqlPool, pgConnStr,
                                             pgPoolSize, runSqlPool)
#else
#error "unknown/unsupported database backend"
#endif

import Import hiding (Proxy, (</>))
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)
import Control.Concurrent.Async             (async, race, Async)
import Control.Concurrent                   (threadDelay)

import qualified Data.Map.Strict            as Map
import qualified Data.Acid                  as A
import Yesod.Helpers.Acid
import Yesod.Helpers.Logger                 ( logFuncByHandlerV, newLogHandlerV
#if DEVELOPMENT
#else
                                            , chooseYesodLoggerBySrcLV
#endif
                                            )

import Filesystem.Path.CurrentOS            ((</>))
import Yesod.Core.Types                     (loggerDate)

import WeiXin.PublicPlatform.Yesod.Site.Function
-- import WeiXin.PublicPlatform.Acid
import WeiXin.PublicPlatform.CS
import WeiXin.PublicPlatform.BgWork
import WeiXin.PublicPlatform.Menu
import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.Misc

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Home

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp



-- | This function allocates resources (such as a database connection pool),
-- performs initialization and return a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appLogHandlerV <- newLogHandlerV (loggerDate appLogger) (appLoggerConfig appSettings)
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    appBgThreadShutdown <- newEmptyMVar

    appDownloadMediaChan <- newChan
    appSendOutMsgsChan <- newChan

    appForwardUrlMap <- newMVar def

    appLastMsgHandlers <- liftM Map.fromList $
        forM (Map.keys $ appWxppAppConfigMap appSettings) $ \k -> (k,) <$> newIORef Nothing

    let logFunc = logFuncByHandlerV appLogHandlerV

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool appAcid = do
            let tf = App {..}
            return tf

    {-
    tempFoundation <- mkFoundation (error "connPool forced in tempFoundation")
                            (error "appAcid forced in tempFoundation")
    let logFunc = messageLoggerSource tempFoundation appLogger
    --}

    -- Create the database connection pool
#if defined(USE_SQLITE)
    pool <- flip runLoggingT logFunc $ createSqlitePool
        (sqlDatabase $ appDatabaseConf appSettings)
        (sqlPoolSize $ appDatabaseConf appSettings)
#elif defined(USE_MYSQL)
    pool <- flip runLoggingT logFunc $ createMySQLPool
        (myConnInfo $ appDatabaseConf appSettings)
        (myPoolSize $ appDatabaseConf appSettings)
#elif defined(USE_POSTGRESQL)
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)
#else
#error "unknown/unsupported database backend"
#endif

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    acid <- flip runLoggingT logFunc $ do
                acidOpenByConfig def (appAcidConfig appSettings) (5 * 1000 * 1000)
                    >>= maybe (throwM $ userError "cannot connect/open acid state") return

    -- Return the foundation
    mkFoundation pool acid

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applyng some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    let def_logger = appLogger foundation
#if DEVELOPMENT
    let logger = def_logger
#else
    logger <- liftM (fromMaybe def_logger) $
                    chooseYesodLoggerBySrcLV (appLogHandlerV foundation) "http"
#endif
    logWare <- mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet logger
        }

    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e -> do
        let logFunc = logFuncByHandlerV (appLogHandlerV foundation)
        when (defaultShouldDisplayException e) $
            logFunc
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

initAppBgWorks :: App -> IO [Async ()]
initAppBgWorks foundation = do
    a1 <- if atk_check_interval > 0
            then do
                ay_list <- forM (Map.elems $ appWxppAppConfigMap settings) $ \wac -> do
                        async $ runAppLoggingT foundation $
                                loopRefreshAccessToken chk_abort
                                    atk_check_interval
                                    wac
                                    cache
                                    (fromIntegral (60 * 5 :: Int))
                                            -- 接近过期 5 分钟内会更新 access token
                                            --
                ay2 <- async $ loopRunBgJob chk_abort (60 * 10) $ do
                                A.createCheckpoint acid
                                A.createArchive acid

                return $ ay2 : ay_list
                
            else return []

    a2 <- case acidServerByConfig acid_config of
        Nothing -> return []
        Just f  -> do
                    ay <- async $ f skipAuthenticationCheck acid
                    return [ay]

    let read_chan_and_down = do
            if_done_or <- liftIO $ race chk_abort (readChan down_chan)
            case if_done_or of
                Left True -> return ()
                Left False -> read_chan_and_down
                Right ch_msg@(app_id, (msg_id, media_id)) -> do
                    m_atk <- wxppGetUsableAccessToken cache app_id
                    case m_atk of
                        Nothing   -> do
                            -- 没有可用的 AccessToken
                            -- 这可能是个暂时的困难，可以稍等之后重试
                            -- TODO: 但又不能长时间地重试，以免积压过多的请求
                            $(logError) $
                              "Cannot download media because of no AccessToken available"
                            liftIO $ do
                              threadDelay $ 1000 * 1000 * 15
                              writeChan down_chan ch_msg

                        Just (atk, _) -> do
                            runAppMainDB foundation $
                                downloadSaveMediaToDB atk msg_id media_id

                    read_chan_and_down

    a3 <- async $ runAppLoggingT foundation $
                logWxppWsExcAndRetryLoop "read_chan_and_down" $ read_chan_and_down

    let read_chan_and_send = do
            if_done_or <- liftIO $ race chk_abort (readChan send_chan)
            case if_done_or of
                Left True -> return ()
                Left False -> read_chan_and_send
                Right (app_id, out_msg_e_lst) -> do
                    m_atk <- wxppGetUsableAccessToken cache app_id
                    case m_atk of
                          Nothing -> do
                              $(logError) $ "cannot send outgoing messages through CS: "
                                  <> "no access token available"
                          Just (atk, _) -> do
                              $logDebug $ fromString $
                                show (length out_msg_e_lst)
                                    ++ " messages to send in background."
                              forM_ out_msg_e_lst $ \msg -> do
                                  logWxppWsExcThen "wxppCsSendOutMsg"
                                      (const $ return ()) (const $ return ())
                                      (wxppCsSendOutMsg atk Nothing msg)

                    read_chan_and_send

    a4 <- async $ runAppLoggingT foundation $
                  logWxppWsExcAndRetryLoop "read_chan_and_send" $ read_chan_and_send

    a5_list <- forM (Map.elems $ appWxppAppConfigMap settings) $ \wac -> do
            async $ runAppLoggingT foundation $ do
                    -- XXX: 延迟一点，等待 access token 加载完成
                    -- 如果不能解决问题，可以等 access token 确认已取得后，重启程序
                    liftIO $ threadDelay $ 2 * 1000 * 1000

                    logWxppWsExcAndRetryLoop "wxppWatchMenuYaml" $ wxppWatchMenuYaml
                        (fmap (fmap fst) $ wxppGetUsableAccessToken cache $ wxppAppConfigAppID wac)
                        (void chk_abort)
                        (wxppAppConfigDataDir wac </> "menu.yml")

    a6 <- async $ runAppLoggingT foundation $ do
            loopCleanupTimedOutForwardUrl
                  (fmap (fmap fst) . wxppGetUsableAccessToken cache)
                  (appForwardUrlMap foundation)

    return $ a6 : a5_list <> [ a4 ] <> [ a3 ] <> a1 <> a2
    where
        acid = appAcid foundation
        acid_config = appAcidConfig $ appSettings foundation
        chk_abort = readMVar (appBgThreadShutdown foundation) >> return True
        down_chan = appDownloadMediaChan foundation
        send_chan = appSendOutMsgsChan foundation
        settings = appSettings foundation
        cache = appWxppCacheBackend foundation
        atk_check_interval = appAccessTokenCheckInterval settings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    _ <- initAppBgWorks foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadAppSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadAppSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    _ <- initAppBgWorks foundation

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    _ <- initAppBgWorks foundation
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp = void . flip tryPutMVar () . appBgThreadShutdown


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB

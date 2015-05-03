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
import Database.Persist.Sqlite              (createSqlitePool, runSqlPool,
                                             sqlDatabase, sqlPoolSize)
import Import hiding (Proxy)
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
import Control.Concurrent.Async             (async, Async)
import Control.Concurrent                   (threadDelay)
import Filesystem.Path.CurrentOS            (fromText, encodeString)
import Data.Proxy

import qualified Data.Acid                  as A
import Yesod.Helpers.Acid

import WeiXin.PublicPlatform.Yesod.Site
import WeiXin.PublicPlatform.Yesod.Site.Function
import WeiXin.PublicPlatform.Acid
import WeiXin.PublicPlatform.CS
import WeiXin.PublicPlatform.BgWork
import WeiXin.PublicPlatform.InMsgHandler
import WeiXin.PublicPlatform.Menu

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Home

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp


-- | 因下面的 WxppInMsgDispatchHandler 也要一个 [WxppInMsgHandlerPrototype m]
-- 所以做了这个不带 WxppInMsgDispatchHandler
allWxppInMsgHandlerPrototypes' :: forall m.
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m, MonadBaseControl IO m ) =>
    App
    -> [WxppInMsgHandlerPrototype m]
allWxppInMsgHandlerPrototypes' foundation =
    WxppInMsgProcessorPrototype
        (Proxy :: Proxy (StoreInMsgToDB m))
        ( WxppSubDBActionRunner $ runAppMainDB foundation
        , \x y -> liftIO $ writeChan (appDownloadMediaChan foundation) (x, y)
        )
    : allBasicWxppInMsgHandlerPrototypes (appWxppDataDir settings </> "out-msg")
    where
        settings = appSettings foundation


allWxppInMsgHandlerPrototypes :: forall m.
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m, MonadBaseControl IO m ) =>
    App
    -> [WxppInMsgHandlerPrototype m]
allWxppInMsgHandlerPrototypes foundation =
    WxppInMsgProcessorPrototype
        (Proxy :: Proxy (WxppInMsgDispatchHandler m))
        ( allBasicWxppInMsgPredictorPrototypes
        , allWxppInMsgHandlerPrototypes' foundation
        )
    : allWxppInMsgHandlerPrototypes' foundation


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
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    appBgThreadShutdown <- newEmptyMVar

    appDownloadMediaChan <- newChan
    appSendOutMsgsChan <- newChan

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool appAcid = do
            let get_access_token = wxppAcidGetUsableAccessToken appAcid
            let wxpp_config = appWxppAppConfig appSettings
                data_dir = appWxppDataDir appSettings
                handle_msg bs ime = runAppLoggingT tf $ do
                                    in_msg_handlers <- liftIO $
                                            readWxppInMsgHandlers
                                                (allWxppInMsgHandlerPrototypes tf)
                                                (encodeString $ data_dir </> fromText "msg-handlers.yml")
                                            >>= either (throwM . userError . show) return
                                    tryEveryInMsgHandler'
                                            appAcid
                                            (liftIO get_access_token)
                                            in_msg_handlers
                                            bs ime

                send_msg = writeChan appSendOutMsgsChan

                appWxppSub  = WxppSub wxpp_config get_access_token send_msg
                                data_dir
                                handle_msg (runAppLoggingT tf)
                tf = App {..}
            return tf

    tempFoundation <- mkFoundation (error "connPool forced in tempFoundation")
                            (error "appAcid forced in tempFoundation")
    let logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createSqlitePool
        (sqlDatabase $ appDatabaseConf appSettings)
        (sqlPoolSize $ appDatabaseConf appSettings)

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
    logWare <- mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

initAppBgWorks :: App -> IO [Async ()]
initAppBgWorks foundation = do
    a1 <- if acidStateConfigLocal acid_config
            then do
                -- this process is the 'real' acid-state opener (local, not remote)
                ay <- async $ runAppLoggingT foundation $
                                loopRefreshAccessToken chk_abort
                                    1       -- 每秒检查一次
                                    wac
                                    acid
                                    (fromIntegral (60 * 5 :: Int))
                                            -- 接近过期 5 分钟内会更新 access token
                ay2 <- async $ loopRunBgJob chk_abort (60 * 10) (A.createCheckpoint acid)
                return [ay, ay2]
                
            else return []

    a2 <- case acidServerByConfig acid_config of
        Nothing -> return []
        Just f  -> do
                    ay <- async $ f skipAuthenticationCheck acid
                    return [ay]

    let read_chan_and_down = do
            if_done <- chk_abort
            if if_done
                then return ()
                else do
                    (msg_id, media_id) <- readChan down_chan
                    m_atk <- wxppAcidGetUsableAccessToken acid
                    runAppLoggingT foundation $ do
                      case m_atk of
                          Nothing   -> do
                              -- 没有可用的 AccessToken
                              -- 这可能是个暂时的困难，可以稍等之后重试
                              -- TODO: 但又不能长时间地重试，以免积压过多的请求
                              $(logError) $
                                "Cannot download media because of no AccessToken available"
                              liftIO $ do
                                threadDelay $ 1000 * 1000 * 15
                                writeChan down_chan (msg_id, media_id)

                          Just atk  -> do
                              runAppMainDB foundation $
                                  downloadSaveMediaToDB atk msg_id media_id
                    read_chan_and_down

    a3 <- async $ read_chan_and_down

    let read_chan_and_send = do
            if_done <- chk_abort
            if if_done
                then return ()
                else do
                    out_msg_e_lst <- readChan send_chan
                    m_atk <- wxppAcidGetUsableAccessToken acid
                    runAppLoggingT foundation $ do
                      case m_atk of
                            Nothing -> do
                                $(logError) $ "cannot send outgoing messages through CS: "
                                    <> "no access token available"
                            Just atk ->
                                mapM_ (wxppCsSendOutMsg atk Nothing) out_msg_e_lst

    a4 <- async $ read_chan_and_send

    a5 <- async $ runAppLoggingT foundation $ do
                    -- XXX: 延迟一点，等待 access token 加载完成
                    -- 如果不能解决问题，可以等 access token 确认已取得后，重启程序
                    liftIO $ threadDelay $ 2 * 1000 * 1000

                    wxppWatchMenuYaml
                        (wxppAcidGetUsableAccessToken acid)
                        (void chk_abort)
                        (appWxppDataDir settings </> "menu.yml")

    return $ a5 : a4 : a3 : a1 <> a2
    where
        acid = appAcid foundation
        wac     = appWxppAppConfig $ appSettings foundation
        acid_config = appAcidConfig $ appSettings foundation
        chk_abort = readMVar (appBgThreadShutdown foundation) >> return True
        down_chan = appDownloadMediaChan foundation
        send_chan = appSendOutMsgsChan foundation
        settings = appSettings foundation

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

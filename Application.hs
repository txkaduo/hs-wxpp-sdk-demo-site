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
import Import
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

import qualified Data.Acid                  as A
import Yesod.Helpers.Acid

import WeiXin.PublicPlatform.Yesod.Site
import WeiXin.PublicPlatform.Acid
import WeiXin.PublicPlatform.BgWork
import WeiXin.PublicPlatform.InMsgHandler

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Home

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp


allWxppInMsgHandlersWHNF ::
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m ) =>
    [SomeWxppInMsgHandler m]
allWxppInMsgHandlersWHNF = allBasicWxppInMsgHandlersWHNF

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

    (in_msg_handlers :: [SomeWxppInMsgHandler (LoggingT IO)])
        <- readWxppInMsgHandlers allWxppInMsgHandlersWHNF "config/msg-handlers.yml"
                            >>= either (throwM . userError . show) return

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool appAcid = do
            let get_access_token = wxppAcidGetUsableAccessToken appAcid
            let wxpp_config = appWxppAppConfig appSettings
                handle_msg  = runAppLoggingT tf .
                                tryEveryInMsgHandler'
                                        appAcid
                                        (liftIO get_access_token)
                                        in_msg_handlers

                appWxppSub  = WxppSub wxpp_config get_access_token handle_msg
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
                ay2 <- async $ A.createCheckpoint acid
                return [ay, ay2]
                
            else return []

    a2 <- case acidServerByConfig acid_config of
        Nothing -> return []
        Just f  -> do
                    ay <- async $ f skipAuthenticationCheck acid
                    return [ay]

    return $ a1 <> a2
    where
        acid = appAcid foundation
        wac     = appWxppAppConfig $ appSettings foundation
        acid_config = appAcidConfig $ appSettings foundation
        chk_abort = readMVar (appBgThreadShutdown foundation) >> return True

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

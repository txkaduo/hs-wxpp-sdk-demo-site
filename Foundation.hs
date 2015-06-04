{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Foundation where

import Import.NoFoundation hiding (Proxy, FilePath, (</>))
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Filesystem.Path.CurrentOS            ((</>))
import Data.Proxy
import Control.Monad.Logger                 (LoggingT)
import qualified Data.Map.Strict            as Map
import qualified Data.Conduit.List          as CL
import Yesod.Helpers.Acid
import Yesod.Helpers.Persist                (DBActionRunner(..))
import Yesod.Helpers.Logger                 (LoggingTRunner(..))
import WeiXin.PublicPlatform.Yesod.Site
import WeiXin.PublicPlatform.Acid
import WeiXin.PublicPlatform.Types
import WeiXin.PublicPlatform.InMsgHandler
import WeiXin.PublicPlatform.Yesod.Site.Function
import WeiXin.PublicPlatform.Misc

import Yesod.Helpers.Logger                 ( LogHandlerV
                                            , defaultYesodLoggerHandlerV
                                            , defaultShouldLogLV
                                            )

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger  -- ^ 现在这个 Logger 已基本弃用
                                -- 仅用作配置失败时的后备
    , appLogHandlerV :: LogHandlerV
    , appBgThreadShutdown   :: MVar ()
        -- ^ put value into this MVar will make backgroup threads exit
    , appAcid               :: AcidState WxppAcidState
    , appDownloadMediaChan  :: Chan (WxppAppID, (WxppInMsgRecordId, WxppMediaID))
    , appSendOutMsgsChan    :: Chan (WxppAppID, [WxppOutMsgEntity])
    , appForwardUrlMap      :: MVar ForwardUrlMap
    , appLastMsgHandlers    :: Map WxppAppID (IORef (Maybe [SomeWxppInMsgHandler (LoggingT IO)]))
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

instance LoggingTRunner App where
    runLoggingTWith foundation = runLoggingTWith (appLogHandlerV foundation)

instance DBActionRunner App where
    type DBAction App = SqlPersistT
    runDBWith = runAppMainDB

runAppLoggingT :: App -> LoggingT m a -> m a
runAppLoggingT = runLoggingTWith

runAppMainDB :: MonadBaseControl IO m =>
    App
    -> SqlPersistT m a
    -> m a
runAppMainDB foundation op = runSqlPool op (appConnPool foundation)

runAppMainDBLogged :: MonadBaseControl IO m =>
    App
    -> SqlPersistT (LoggingT m) a
    -> m a
runAppMainDBLogged foundation = runAppLoggingT foundation . runAppMainDB foundation

-- | 因下面的 WxppInMsgDispatchHandler 也要一个 [WxppInMsgHandlerPrototype m]
-- 所以做了这个不带 WxppInMsgDispatchHandler
allWxppInMsgHandlerPrototypes' :: forall m.
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m, MonadBaseControl IO m ) =>
    App
    -> WxppAppID
    -> [WxppInMsgHandlerPrototype m]
allWxppInMsgHandlerPrototypes' foundation app_id =
    case Map.lookup app_id app_conf_map of
        Nothing -> []
        Just wac ->
            WxppInMsgProcessorPrototype
                (Proxy :: Proxy (StoreInMsgToDB m))
                ( app_id
                , sub_db_runner
                , \x y -> liftIO $ writeChan (appDownloadMediaChan foundation) (app_id, (x, y))
                )
            : WxppInMsgProcessorPrototype
                (Proxy :: Proxy (CacheAppOpenIdToUnionId m))
                ( app_id, sub_db_runner )
            : allBasicWxppInMsgHandlerPrototypes
                app_id
                (wxppAppConfigDataDir wac </> "out-msg")
                (appForwardUrlMap foundation)
    where
        sub_db_runner = WxppSubDBActionRunner $ runAppMainDB foundation
        app_conf_map = appWxppAppConfigMap settings
        settings = appSettings foundation


allWxppInMsgHandlerPrototypes :: forall m.
    ( MonadIO m, MonadLogger m, MonadThrow m, MonadCatch m, MonadBaseControl IO m ) =>
    App
    -> WxppAppID
    -> [WxppInMsgHandlerPrototype m]
allWxppInMsgHandlerPrototypes foundation app_id =
    all_protos
    where
        proto_dispatch =
            WxppInMsgProcessorPrototype
                (Proxy :: Proxy (WxppInMsgDispatchHandler m))
                (allBasicWxppInMsgPredictorPrototypes, all_protos)

        -- CAUTION: this value call itself recurisively
        all_protos = proto_dispatch :
                        allWxppInMsgHandlerPrototypes' foundation app_id


appWxppCacheBackend :: App -> SomeWxppCacheBackend
appWxppCacheBackend foundation = SomeWxppCacheBackend $
#if 0
        (WxppCacheByAcid $ appAcid foundation)
#else
        (WxppSubDBActionRunner $ runDBWith foundation :: WxppSubDBActionRunner IO)
#endif

appGetWxppSub :: App -> WxppAppID -> MaybeWxppSub
appGetWxppSub foundation =
    mkMaybeWxppSub
        foundation
        (appWxppCacheBackend foundation)
        (flip Map.lookup $ appLastMsgHandlers foundation)
        (appWxppAppConfigMap $ appSettings foundation)
        (allWxppInMsgHandlerPrototypes foundation)
        (appSendOutMsgsChan foundation)
        (appDownloadMediaChan foundation)
        opts
    where
        opts = appWxppSubsiteOpts settings
        settings = appSettings foundation

appGetWxppSubNoApp :: App -> WxppSubNoApp
appGetWxppSubNoApp foundation =
    WxppSubNoApp
        get_openid_by_union_id
        (runAppLoggingT foundation)
        (wxppSubTrustedWaiReq opts)
    where
        opts = appWxppSubsiteOpts settings
        settings = appSettings foundation

        get_openid_by_union_id union_id = runAppMainDBLogged foundation $ runResourceT $ do
            selectSource [ WxppUserCachedInfoUnionId ==. Just union_id ] []
                =$= CL.map ((wxppUserCachedInfoOpenId &&& wxppUserCachedInfoApp) . entityVal)
                $$ CL.consume


-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    {-
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError
            --}

    shouldLogIO foundation source level = do
        defaultShouldLogLV (appLogHandlerV foundation) source level

    makeLogger foundation =
        -- return . appLogger
        return $ fromMaybe def_logger $
                defaultYesodLoggerHandlerV (appLogHandlerV foundation)
        where
            def_logger = appLogger foundation

#if !defined(DEVELOPMENT) && defined(OFFLOAD_STATIC_SITE)
    urlRenderOverride master    (StaticR s) =
        Just $ uncurry (joinPath master OFFLOAD_STATIC_SITE) $ renderRoute s
    urlRenderOverride _         _           = Nothing
#endif

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert User
                    { userIdent = credsIdent creds
                    , userPassword = Nothing
                    }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId def]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

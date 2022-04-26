module Api.Article where

-- import Control.Monad.Catch
-- import Control.Monad.Writer
-- import Crypto.Hash qualified as Crypto


-- import HKD.HKD

-- import Extended.Text (Text)
-- import Extended.Text qualified as T
-- import Entity.Author
-- import Entity.Internal
-- import App.Router
-- import App.Internal
-- import App.Types
-- import App.Result
-- import Entity.Article

-- import Database.Database qualified as Database
-- import Database.Database (Database)
-- import Logger qualified
-- import Logger ((.<))
-- import Data.Coerce
-- import Data.String (IsString(fromString))

-- getArticles :: forall m. 
--     ( Application m
--     , Gettable m Article (Front Display)
--     , Database.ToRowOf (Database m) [Page]
--     ) => Endpoint m
-- getArticles _ = do
--     Logger.info "Attempt to get articles"
--     page <- getPage
--     json =<< Database.getManyEntitiesWith @Article @(Front Display) 
--         (<> " WHERE published = true")
--         page

-- getArticle :: forall m. 
--     ( Application m
--     , Gettable m Article (Front Display)
--     , Database.ToRowOf (Database m) (ID (Article (Front Display)))
--     ) => Endpoint m
-- getArticle [aID] = do
--     Logger.info "Attempt to get articles"
--     json =<< (Database.getEntitiesWith @Article @(Front Display) 
--         (coerce @_ @(ID (Article (Front Display))) aID) 
--         (<> " WHERE published = true AND id = ?") 
--         >>= Database.getSingle )
-- getArticle _ = entityIDArityMissmatch "get article api"



-- postDraft :: forall m.
--     ( Application m
--     , Database.GettableFrom (Database m) (Entity Author) Display
--     , Database.PostableTo   (Database m) Article
--     , Database.ToRowOf (Database m) [Token]
--     , Database.ToRowOf (Database m) (Article Create)
--     , Database.FromRowOf (Database m) (ID (Article Create))
--     ) => Endpoint m
-- postDraft _ = do
--     Logger.info "Attempt to post draft"
--     token      <- getToken
--     now        <- getCurrentDate 
--     Entity{..} <- Database.getEntitiesWith @(Entity Author) @Display @m [token]
--         (<> " WHERE token = ? ") >>= Database.getSingle
--     Article{..} <- decodedBody @(Article (Front Create))
--     let article = Article
--             { created = now
--             , author = coerce entityID
--             , category = coerce category 
--             , tags = coerce tags
--             , ..}
--     articleID <- Database.postEntityWith id article
--     text articleID

-- putDraft :: forall m.
--     ( Application m
--     , Puttable m (Entity Article) (Front Update)
--     ) => Endpoint m
-- putDraft [dID] = do
--     a@Article{..} <- decodedBody @(Article (Front Update))
--     Database.putEntity $ Entity (coerce dID) a
--     text "Successfuly updated."
-- putDraft _ = entityIDArityMissmatch "put draft"

-- publishDraft :: forall m.
--     ( Application m
--     , Database.GettableFrom (Database m) (Entity Author) Display
--     , Database.ToRowOf   (Database m) [Token]
--     , Database.ToRowOf   (Database m) (Entity Article Publish)
--     , Database.FromRowOf (Database m) (ID (Entity Article Publish))
--     ) => Endpoint m
-- publishDraft [draftID]= do
--     token <- getToken
--     now <- getCurrentDate
--     Entity{..} <- Database.getEntitiesWith @(Entity Author) @Display @m [token]
--         (<> " WHERE token = ? ") >>= Database.getSingle
--     let q = fromString publishQuery
--     text <=< Database.publish @(Entity Article) @m @Publish q $
--         Entity (coerce draftID) Article
--             { author   = coerce entityID
--             , created  = now 
--             , title    = Nothing 
--             , content  = Nothing 
--             , category = Nothing 
--             , tags     = Nothing 
--             }
-- publishDraft _ = entityIDArityMissmatch "publish draft"



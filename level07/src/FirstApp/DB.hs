{-# LANGUAGE OverloadedStrings #-}
module FirstApp.DB
  ( Table (..)
  , FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)

import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.DB.Types                  (FirstAppDB (FirstAppDB, dbConn),
                                                     Table (Table, getTableName))
import           FirstApp.Error                     (Error (DBError))
import           FirstApp.Types                     (Comment, CommentText,
                                                     Topic, fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           FirstApp.AppM                      (AppM, envDb, throwL)

-- Doctest setup section
-- $setup
-- >>> :set -XOverloadedStrings

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDb
  :: ( Sql.Connection -> IO a )
  -> AppM a
runDb a = do
  db <- asks ( dbConn . envDb )
  -- We use the liftDb function to take the IO (Either SQLiteResponse a) and
  -- convert it into an `m (Either Error a)` so that it matches the requirements
  -- to be in our AppM, we then lean on the ExceptT functionality and use our
  -- helper to either `throwError` with any DB errors that have made it this far
  -- or return the desired value.
  liftDb db >>= throwL
  -- Or alternatively, if you hate variables...
  -- asks (dbConn.envDb) >>= ( liftDb >=> throwL )
  where
    -- The first function here is from the Data.Bifunctor module and lets us run
    -- functions on the left side of a Bifunctor:
    -- first :: ( a -> b ) -> p a c -> p b c
    -- Where `p` is our Bifunctor: Either
    liftDb conn = liftIO $ first DBError
      <$> Sql.runDBAction (a conn)

faQuery
  :: ( ToRow q
     , FromRow r
     )
  => Query
  -> q
  -> Connection
  -> IO [r]
faQuery q p c =
  Sql.query c q p

faQuery_
  :: FromRow r
  => Query
  -> Connection
  -> IO [r]
faQuery_ q c =
  Sql.query_ c q

faExecute
  :: ( ToRow q
     )
  => Query
  -> q
  -> Connection
  -> IO ()
faExecute q p c =
  Sql.execute c q p

getComments
  :: Topic
  -> AppM [Comment]
getComments t = do
  -- Write the query with an icky string and remember your placeholders!
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- To be doubly and triply sure we've no garbage in our response, we take care
  -- to convert our DB storage type into something we're going to share with the
  -- outside world. Checking again for things like empty Topic or CommentText values.
  res <- runDb $ faQuery q [ getTopic t ]
  throwL $ traverse fromDbComment res

addCommentToTopic
  :: Topic
  -> CommentText
  -> AppM ()
addCommentToTopic t c = do
  -- Record the time this comment was created.
  nowish <- liftIO getCurrentTime
  -- Note the triple, matching the number of values we're trying to insert, plus
  -- one for the table name.
  --
  -- Remember that the '?' are order dependent so if you get your input
  -- parameters in the wrong order, the types won't save you here. More on that
  -- sort of goodness later.
  let q = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  -- We use the execute function this time as we don't care about anything
  -- that is returned. The execute function will still return the number of rows
  -- affected by the query, which in our case should always be 1.
  runDb $ faExecute q (getTopic t, getCommentText c, nowish)
  -- An alternative is to write a returning query to get the Id of the DbComment
  -- we've created. We're being lazy (hah!) for now, so assume awesome and move on.

getTopics
  :: AppM [Topic]
getTopics = do
  let q = "SELECT DISTINCT topic FROM comments"
  res <- runDb $ faQuery_ q
  throwL $ traverse ( mkTopic . Sql.fromOnly ) res

deleteTopic
  :: Topic
  -> AppM ()
deleteTopic t = do
  let q = "DELETE FROM comments WHERE topic = ?"
  runDb $ faExecute q [getTopic t]

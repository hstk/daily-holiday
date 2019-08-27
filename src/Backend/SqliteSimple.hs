{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE QuasiQuotes        #-}

module Backend.SqliteSimple where

import           Prelude hiding (Word)

import           Control.Applicative
import           Data.Functor ((<&>))
import           Data.Monoid
import           Data.Maybe (fromJust)
import           Data.Foldable

import           Data.Int
import           Data.List (nub, sort)
import           Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Text(Text(..))
import           Data.ByteString.Lazy(ByteString(..))
import qualified Data.ByteString.Lazy as BS

import           GHC.Generics
import           Data.Aeson

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Ok
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Time
import           Database.SQLite.Simple.QQ

-- https://hackage.haskell.org/package/sqlite-simple-0.4.16.0/docs/Database-SQLite-Simple.html
-- dbs : https://www.reddit.com/r/haskell/comments/7tx0o4/haskell_3_sql/
-- representing sum types : https://www.parsonsmatt.org/2019/03/19/sum_types_in_sql.html

data Word = Word
  { wordId  :: Maybe WordId
  , word    :: Text
  , addedBy :: Maybe UserId
  , banned  :: Bool
  , addedOn :: UTCTime
  } deriving (Eq, Show, Generic)

newtype WordId = WordId { unWordId :: Int64 }
  deriving (Eq, Show, Ord, Bounded)

instance ToField WordId where
  toField = toField . unWordId

instance FromField WordId where
  fromField = fromField

instance ToRow Word where
  toRow (Word {..}) = toRow (wordId, word, addedBy, banned, addedOn)

instance FromRow Word where
  fromRow = Word <$> field <*> field <*> field <*> field <*> field

newtype Noun = Noun { unNoun :: Word }
  deriving (Eq, Show, Generic)

instance ToRow Noun where
  toRow = toRow . unNoun
instance FromRow Noun where
  fromRow = Noun <$> fromRow

newtype Adjective = Adjective { unAdjective :: Word }
  deriving (Eq, Show, Generic)

instance ToRow Adjective where
  toRow = toRow . unAdjective
instance FromRow Adjective where
  fromRow = Adjective <$> fromRow

data UserStatus = Banned Text | Unconfirmed | Confirmed | Admin
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON UserStatus where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON UserStatus

instance ToField UserStatus where
  toField a = toField . encode $ a

instance FromField UserStatus where
  fromField f = do 
    bs <- fromField f
    case decode bs of
      Just status -> Ok status
      Nothing     -> returnError ConversionFailed f "Couldn't decode user status"

newtype UserId = UserId { unUserId :: Int64 }
  deriving (Eq, Show, Ord)

instance ToField UserId where
  toField = toField . unUserId

instance FromField UserId where
  fromField f = UserId <$> fromField f

data User = User
  { userId     :: Maybe UserId
  , userName   :: Text
  , userEmail  :: Text
  , joinDate   :: UTCTime
  , holidays   :: Int
  , userStatus :: UserStatus
  } deriving (Eq, Show, Generic)

instance ToRow User where
  toRow (User {..}) = 
    toRow (userId, userName, userEmail, joinDate, holidays, userStatus)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

-- have to batch these out, can't create multiple tables in one statement
executeMany_ :: Connection -> [Query] -> IO ()
executeMany_ conn xs = traverse_ (execute_ conn) xs

createTables :: Connection -> IO ()
createTables conn = executeMany_ conn [
  [sql| CREATE TABLE IF NOT EXISTS 
    Noun(
      id      INTEGER PRIMARY KEY,
      word    TEXT NOT NULL UNIQUE,
      addedBy INTEGER,
      banned  BOOL,
      addedOn DATETIME NOT NULL
    ); |],
  [sql| CREATE TABLE IF NOT EXISTS 
    Adjective(
      id      INTEGER PRIMARY KEY,
      word    TEXT NOT NULL UNIQUE,
      addedBy INTEGER,
      banned  BOOL,
      addedOn DATETIME NOT NULL
    ); |],
  [sql| CREATE TABLE IF NOT EXISTS 
    User(
      id         INTEGER PRIMARY KEY,
      userName   TEXT,
      userEmail  TEXT NOT NULL,
      joinDate   DATETIME NOT NULL,
      holidays   INT,
      userStatus BLOB
    ); |]
  ]

refreshTestDb :: IO ()
refreshTestDb = do
  conn <- open "data/simple-test.db"
  dropTables conn
  _ <- createTables conn
  now <- getCurrentTime
  _ <- traverse_ (insertUser conn) (sampleUsers now)
  _ <- insertWords conn
  close conn

prepCorpus :: [Text] -> [Text]
prepCorpus = sort . nub

insertWords :: Connection -> IO ()
insertWords conn = do
  adjWord  <- prepCorpus <$> T.lines <$> TIO.readFile "data/adjectives.txt"
  nounWord <- prepCorpus <$> T.lines <$> TIO.readFile "data/nouns.txt"
  now <- getCurrentTime

  let insertNounStatement = [sql|
        INSERT INTO Noun(id, word, addedBy, banned, addedOn)
        VALUES (?, ?, ?, ?, ?)|]

  let insertAdjStatement = [sql|
        INSERT INTO Adjective(id, word, addedBy, banned, addedOn) 
        VALUES (?, ?, ?, ?, ?)|]

  let toWordModel x = Word 
        { wordId = Nothing
        , word = x
        , addedBy = Nothing
        , banned = False
        , addedOn = now }
  
  let adjectives = Adjective . toWordModel <$> adjWord
  let nouns = Noun . toWordModel <$> nounWord

  executeMany conn insertNounStatement nouns
  executeMany conn insertAdjStatement adjectives

sampleUsers :: UTCTime -> [User]
sampleUsers now = 
  [ User (Just $ UserId 0) "hank"  "hstk@hstk.dev"  now 50 Admin
  , User (Just $ UserId 1) "scott" "scott@bofh.net" now 10 (Banned "gefuzzled is not a word")
  ]

dropTables :: Connection -> IO ()
dropTables conn = executeMany_ conn 
  [ "DROP TABLE IF EXISTS Noun;"
  , "DROP TABLE IF EXISTS Adjective;"
  , "DROP TABLE IF EXISTS User;"
  ]

-- inserts
insertUser :: Connection -> User -> IO ()
insertUser conn u = execute conn 
  [sql|
  INSERT INTO 
  User(userName, userEmail, joinDate, holidays, userStatus) 
  VALUES (?, ?, ?, ?, ?)
  |] u

insertNoun :: Connection -> Noun -> IO ()
insertNoun conn n = execute conn
  [sql|
  INSERT INTO 
  Noun(id, word, addedBy, banned, addedOn) 
  VALUES (?, ?, ?, ?, ?)
  |] n

insertAdjective :: Connection -> Adjective -> IO ()
insertAdjective conn a = execute conn
  [sql|
  INSERT INTO 
  Adjective(id, word, addedBy, banned, addedOn)
  VALUES (?, ?, ?, ?)
  |] a

-- selects
getUserById :: UserId -> Connection -> IO [User]
getUserById (UserId x) conn = query conn 
  "SELECT * FROM User WHERE id = ?" $ Only x

getAdjectivesByUser :: UserId -> Connection -> IO [Adjective]
getAdjectivesByUser (UserId x) conn = query conn
  "SELECT * FROM Adjective where addedBy = ?" $ Only x

getNounsByUser :: UserId -> Connection -> IO [Noun]
getNounsByUser (UserId x) conn = query conn
  "SELECT * FROM Noun where addedBy = ?" $ Only x

getRandomPhrase :: Connection -> IO (Adjective, Noun)
getRandomPhrase conn = do
  adj  <- query_ conn "SELECT * FROM Adjective ORDER BY RANDOM() LIMIT 1;" :: IO [Adjective]
  noun <- query_ conn "SELECT * FROM Noun ORDER BY RANDOM() LIMIT 1;" :: IO [Noun]
  pure $ (head adj, head noun)

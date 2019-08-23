{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE QuasiQuotes        #-}


module SQLiteSimpleBackend where

import           Control.Applicative
import           Data.Monoid
import           Data.Maybe (fromJust)
import           Data.Foldable

import           Data.Int
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
  { wordId  :: Int32
  , word    :: Text
  , addedBy :: Maybe (UserId)
  , banned  :: Bool
  , addedOn :: UTCTime
  }

newtype Noun = Noun Word
  deriving (Eq, Show, Generic)

newtype Adjective = Adjective Word
  deriving (Eq, Show, Generic)

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

newtype UserId = UserId { unUserId :: Int32 }
  deriving (Eq, Show, Ord)

instance ToField UserId where
  toField = toField . unUserId

instance FromField UserId where
  fromField = fromField

data User = User
  { userId     :: UserId
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

setupTestDb = do
  conn <- open "data/test.db"
  _ <- createTables conn
  now <- getCurrentTime


  traverse_ (insertUser conn) [hank, scott]

  close conn

sampleUsers :: [User]
sampleUsers = 
  [ User (UserId 0) "hank"  "hstk@hstk.dev"  now 50 Admin
  , User (UserId 1) "scott" "scott@bofh.net" now 10 (Banned "gefuzzled is not a word")
  ]

dropTables :: Connection -> IO ()
dropTables conn = execute_ conn 
  [sql|
  DROP TABLE Noun;
  DROP TABLE Adjective;
  DROP TABLE User;
  |]

-- have to batch these out, can't create multiple tables in one statement
createTables :: Connection -> IO ()
createTables conn = traverse_ (execute_ conn) [
  [sql| CREATE TABLE IF NOT EXISTS Noun(
    id      INTEGER PRIMARY KEY,
    word    TEXT NOT NULL UNIQUE,
    addedBy INTEGER,
    banned  BOOL,
    addedOn DATETIME NOT NULL
  ); |],
  [sql| CREATE TABLE IF NOT EXISTS Adjective(
    id      INTEGER PRIMARY KEY,
    word    TEXT NOT NULL UNIQUE,
    addedBy INTEGER,
    banned  BOOL,
    addedOn DATETIME NOT NULL
  ); |],
  [sql| CREATE TABLE IF NOT EXISTS User(
    id         INTEGER PRIMARY KEY,
    userName   TEXT,
    userEmail  TEXT NOT NULL,
    joinDate   DATETIME NOT NULL,
    holidays   INT,
    userStatus BLOB
  ); |]
  ]

-- insertUser :: 
insertUser :: Connection -> User -> IO ()
insertUser conn (User {..}) = execute conn 
  [sql|
  INSERT INTO 
  User(userName, userEmail, joinDate, holidays, userStatus) 
  VALUES (?, ?, ?, ?, ?)
  |]
  (userName, userEmail, joinDate, holidays, userStatus)

insertNoun :: Connection -> Noun -> IO ()
insertNoun conn noun = do
  [sql|
  INSERT INTO 
  Noun(userName, userEmail, joinDate, holidays, userStatus) 
  VALUES (?, ?, ?, ?, ?)
  |]
  (userName, userEmail, joinDate, holidays, userStatus)

insertAdjective :: Connection -> Adjective -> IO ()
insertAdjective conn = do
  undefined

insertWords :: Connection -> IO Text
insertWords conn = do
  adjectives <- T.lines <$> TIO.readFile "data/adjectives.txt"
  nouns      <- T.lines <$> TIO.readFile "data/nouns.txt"
  executeMany insertAdjective adjectives
  executeMany insertNoun nouns

readWords :: FilePath -> IO [Text]
readWords path = fmap T.lines $ T.IO.readFile path

getWord :: FilePath -> IO Text
getWord path = readWords path >>= chooseWord

chooseWord :: [Text] -> IO Text
chooseWord xs = do
  index <- randomRIO (0, length xs - 1)
  return $ xs !! index

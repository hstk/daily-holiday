{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE QuasiQuotes    #-}


module SQLiteSimpleBackend where

import           Control.Applicative
-- import           Data.Monoid
import           Data.Maybe (fromJust)

import           Data.Int
import           Data.Time
import qualified Data.Text as T
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
  } deriving (Eq, Show, Generic)

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

zerp = do
  conn <- open ":memory:"

  close conn

createTables :: Query
createTables = [sql|
  CREATE TABLE Noun(
    wordId  INTEGER PRIMARY KEY,
    word    TEXT NOT NULL UNIQUE,
    addedBy INTEGER,
    banned  BOOL,
    addedOn DATETIME NOT NULL
  );

  CREATE TABLE Adjective(
    wordId  INTEGER PRIMARY KEY,
    word    TEXT NOT NULL UNIQUE,
    addedBy INTEGER,
    banned  BOOL,
    addedOn DATETIME NOT NULL
  );
  
  CREATE TABLE User(
    userId     INTEGER PRIMARY KEY,
    userName   TEXT,
    userEmail  TEXT NOT NULL,
    joinDate   DATETIME NOT NULL,
    holidays   INT,
    userStatus BLOB
  );
  |]

-- data HolidayDb f = HolidayDb 
--   { nouns      :: f (TableEntity WordT) 
--   , adjectives :: f (TableEntity WordT)
--   , users      :: f (TableEntity UserT) }
--     deriving (Generic, Database be)

-- populateDb :: IO ()
-- populateDb = do
--   db <- open "/data/holiday.db"
--   now <- zonedTimeToLocalTime <$> getZonedTime

--   runBeamSqliteDebug putStrLn db $ runInsert $ insert (users holidayDb) $ insertValues 
--     [ User "Hank"    "hstk@hstk.hstk"  now 50 Admin
--     , User "DarkArc" "wac@test.test"   now 5  Confirmed
--     , User "scott"   "scott@bofh.test" now 20 (Banned "For general scumbaggery")
--     ]

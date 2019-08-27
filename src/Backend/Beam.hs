{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Backend.Beam where

import           Prelude hiding (Word)

import           Database.SQLite.Simple
import           Database.SQLite.Simple.QQ

import           Database.Beam
import           Database.Beam.Sqlite
import           Database.Beam.Backend.SQL

import           Data.Foldable
import           Data.Int
import           Data.Text (Text(..), unpack)
import           Data.Time

data WordT f = Word
  { wordId  :: C f WordId
  , word    :: C f Text
  , addedBy :: C f (Maybe UserId)
  , banned  :: C f Bool
  , addedOn :: C f LocalTime
  } deriving (Generic, Beamable)

newtype WordId = WordId { unWordId :: Int64 }
  deriving (Eq, Show, Ord, Bounded)

instance Table WordT where
  data PrimaryKey WordT f = WordPk (C f WordId) deriving (Generic, Beamable)
  primaryKey = WordPk . wordId -- we're calling an associated constructor on a field of WordT here, the one that we defined above

type Word = WordT Identity
deriving instance Show Word
deriving instance Eq Word

data UserStatus = Banned Text | Unconfirmed | Confirmed | Admin
  deriving (Show, Read, Eq, Ord)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be UserStatus where
  sqlValueSyntax = autoSqlValueSyntax
instance FromBackendRow Sqlite UserStatus where
  fromBackendRow = read . unpack <$> fromBackendRow

data UserT f = User
  { userId       :: C f UserId
  , userName     :: C f Text
  , userEmail    :: C f Text
  , userJoinDate :: C f LocalTime
  , userHolidays :: C f Int
  , userStatus   :: C f UserStatus
  } deriving (Generic, Beamable)

newtype UserId = UserId { unUserId :: Int64 }
  deriving (Eq, Show, Ord)

-- this is the equivalent of ToField and FromField
-- http://hackage.haskell.org/package/beam-core-0.8.0.0/docs/src/Database.Beam.Backend.SQL.SQL92.html#sqlValueSyntax
instance HasSqlValueSyntax be Int64 => HasSqlValueSyntax be UserId where
  sqlValueSyntax = sqlValueSyntax . unUserId
instance FromBackendRow Sqlite UserId where
  fromBackendRow = UserId <$> fromBackendRow

instance Table UserT where
  data PrimaryKey UserT f = UserPk (C f UserId) deriving (Generic, Beamable)
  primaryKey = UserPk . userId

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

data HolidayDb f = HolidayDb 
  { noun      :: f (TableEntity WordT) 
  , adjective :: f (TableEntity WordT)
  , user      :: f (TableEntity UserT) }
    deriving (Generic, Database be)

holidayDb :: DatabaseSettings be HolidayDb
holidayDb = defaultDbSettings

populateDb :: IO ()
populateDb = do
  conn <- open "data/beam-test.db"
  createDb conn
  now <- zonedTimeToLocalTime <$> getZonedTime

  runBeamSqliteDebug putStrLn conn $ runInsert $ insert (user holidayDb) $ insertValues 
    [ User (UserId 1) "Hank"    "hstk@hstk.hstk"  now 50 Admin
    , User (UserId 2) "DarkArc" "wac@test.test"   now 5  Confirmed
    , User (UserId 3) "scott"   "scott@bofh.test" now 20 (Banned "Gefuzzled is not a word")
    ]

stage :: IO ()
stage = do 
  conn <- open "data/beam-test.db"
  createDb conn
  close conn

createDb :: Connection -> IO ()
createDb conn = traverse_ (execute_ conn) 
  [ "DROP TABLE IF EXISTS Noun;"
  , "DROP TABLE IF EXISTS Adjective;"
  , "DROP TABLE IF EXISTS User;"
  , [sql| CREATE TABLE IF NOT EXISTS 
    Noun(
      id      INTEGER PRIMARY KEY,
      word    TEXT NOT NULL UNIQUE,
      addedBy INTEGER,
      banned  BOOL,
      addedOn DATETIME NOT NULL
    ); |]
  , [sql| CREATE TABLE IF NOT EXISTS 
    Adjective(
      id      INTEGER PRIMARY KEY,
      word    TEXT NOT NULL UNIQUE,
      addedBy INTEGER,
      banned  BOOL,
      addedOn DATETIME NOT NULL
    ); |]
  , [sql| CREATE TABLE IF NOT EXISTS 
    User(
      id         INTEGER PRIMARY KEY,
      name       TEXT,
      email      TEXT NOT NULL,
      join_date  DATETIME NOT NULL,
      holidays   INT,
      status     BLOB
    ); |]
  ]

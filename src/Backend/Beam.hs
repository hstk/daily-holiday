{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Backend.Beam () where

import Prelude hiding (Word)

import Database.SQLite.Simple

import Database.Beam
import Database.Beam.Sqlite
import Database.Beam.Backend.SQL

import Data.Text (Text, unpack)
import Data.Time

-- https://tathougies.github.io/beam/tutorials/tutorial1/

data WordT f = Word
  { word    :: C f Text
  , addedBy :: C f Text
  , banned  :: C f Bool
  , addedOn :: C f LocalTime
  } deriving (Generic, Beamable)

instance Table WordT where
  data PrimaryKey WordT f = WordId (C f Text) deriving (Generic, Beamable)
  primaryKey = WordId . word

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
  { userName  :: C f Text
  , userEmail :: C f Text
  , joinDate  :: C f LocalTime
  , holidays  :: C f Int
  , userType  :: C f UserStatus
  } deriving (Generic, Beamable)

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f Text) deriving (Generic, Beamable)
  primaryKey = UserId . userEmail

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

data HolidayDb f = HolidayDb 
  { nouns      :: f (TableEntity WordT) 
  , adjectives :: f (TableEntity WordT)
  , users      :: f (TableEntity UserT) }
    deriving (Generic, Database be)

holidayDb :: DatabaseSettings be HolidayDb
holidayDb = defaultDbSettings

populateDb :: IO ()
populateDb = do
  db <- open "/data/holiday.db"
  now <- zonedTimeToLocalTime <$> getZonedTime

  runBeamSqliteDebug putStrLn db $ runInsert $ insert (users holidayDb) $ insertValues 
    [ User "Hank"    "hstk@hstk.hstk"  now 50 Admin
    , User "DarkArc" "wac@test.test"   now 5  Confirmed
    , User "scott"   "scott@bofh.test" now 20 (Banned "For general scumbaggery")
    ]

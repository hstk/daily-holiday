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

module Persistence where

import Prelude hiding (Word)

import Database.SQLite.Simple

import Database.Beam
import Database.Beam.Sqlite

import Data.Text (Text)
import Data.Time (LocalTime)

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

data HolidayDb f = HolidayDb 
  { nouns      :: f (TableEntity WordT) 
  , adjectives :: f (TableEntity WordT) }
    deriving (Generic, Database be)

holidayDb :: DatabaseSettings be HolidayDb
holidayDb = defaultDbSettings
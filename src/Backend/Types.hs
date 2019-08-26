{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}

module Backend.Types where

import           Prelude hiding (Word)

import           GHC.Generics

import           Data.Int
import           Data.Time
import qualified Data.Text as T
import           Data.Text (Text(..))
-- import qualified Data.Text.IO as TIO

data Word = Word
  { wordId  :: WordId
  , word    :: Text
  , addedBy :: Maybe (UserId)
  , banned  :: Bool
  , addedOn :: UTCTime
  } deriving (Eq, Show, Generic)

newtype WordId = WordId { unWordId :: Int32 }
  deriving (Eq, Show, Ord, Bounded)

newtype Noun = Noun Word
  deriving (Eq, Show, Generic)

newtype Adjective = Adjective Word
  deriving (Eq, Show, Generic)

data User = User
  { userId     :: UserId
  , userName   :: Text
  , userEmail  :: Text
  , joinDate   :: UTCTime
  , holidays   :: Int
  , userStatus :: UserStatus
  } deriving (Eq, Show, Generic)
  
newtype UserId = UserId { unUserId :: Int32 }
  deriving (Eq, Show, Ord, Bounded)

data UserStatus = Banned Text | Unconfirmed | Confirmed | Admin
  deriving (Show, Read, Eq, Ord, Generic)
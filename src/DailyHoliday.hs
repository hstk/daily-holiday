{-# LANGUAGE OverloadedStrings #-}

module DailyHoliday (getHoliday) where

import           Control.Monad
import           Data.Text     (Text)
import qualified Data.Text     as T
import qualified Data.Text.IO  as T.IO
import           System.Random (randomRIO)

getHoliday :: IO Text
getHoliday = do
  a <- getWord "data/adjectives.txt"
  n <- getWord "data/nouns.txt"
  return $ T.concat [(T.toTitle a), " ", (T.toTitle n), " Day!"]

readWords :: FilePath -> IO [Text]
readWords path = fmap T.lines $ T.IO.readFile path

chooseWord :: [Text] -> IO Text
chooseWord xs = do
  index <- randomRIO (0, length xs - 1)
  return $ xs !! index

getWord :: FilePath -> IO Text
getWord path = readWords path >>= chooseWord
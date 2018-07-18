{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import DailyHoliday (getHoliday)
import Web.Scotty

import Data.Text (Text)
import qualified Data.Text.Lazy as L

main :: IO ()
main = do
  putStrLn "Starting server..."
  scotty 3000 $ do
    get "/" $ do
      holiday <- liftIO $ fmap L.fromStrict getHoliday
      html holiday
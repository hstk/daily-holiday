{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           DailyHoliday (getHoliday)
import           Data.Text (Text)
import qualified Data.Text.Lazy as L
import           Layout (templateHoliday)
import           Web.Scotty

main :: IO ()
main = do
  putStrLn "Starting server at http://localhost:3000..."
  scotty 3000 $ do
    get "/" $ do
      holiday <- liftIO $ fmap L.fromStrict getHoliday
      html $ templateHoliday holiday
    get "/html" $ do
      holiday <- liftIO $ fmap L.fromStrict getHoliday
      html holiday
    get "/milligram.css" $ file "./web/css/milligram.min.css"

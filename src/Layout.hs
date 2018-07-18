{-# LANGUAGE OverloadedStrings #-}

module Layout where

import           Lucid
import           Data.Text.Lazy (Text) 
-- import qualified Data.Text as T
--base, bootstrap, html5

--https://chrisdone.com/posts/lucid2
templateHoliday content =
  renderText $ html_ $ do

    head_ $ do
      title_ "Daily Holiday!"
      link_ [ rel_  "stylesheet"
            , type_ "text/css"
            , href_ "/milligram.css" ]

    body_ $ do
      h3_ "Daily Holiday"
      p_ (toHtml content)
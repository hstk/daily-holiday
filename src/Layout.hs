{-# LANGUAGE OverloadedStrings #-}

module Layout where

import Lucid
import Data.Text.Lazy (Text)
import Data.Monoid ((<>))
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
      div_ [ class_ "container" ] $ do
        div_ [ class_ "row" ] $ do
          div_ [ class_ "column" ] $ do
            p_ ("Your holiday is... " <> (toHtml content))
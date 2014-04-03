
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Mime (
  Mime, defaultMimes
  ) where

import qualified Data.Text as T
import Data.Word

type Mime = T.Text

defaultMimes :: [(Word16, Mime)]
defaultMimes =
  [ ( 44, "application/octet-stream")
  , ( 91, "application/xml")
  , ( 94, "application/zip")
  , (384, "application/x-sh")
  , (388, "application/x-tar")
  , (447, "image/gif")
  , (449, "image/jpeg")
  , (452, "image/png")
  , (476, "image/x-icon")
  , (528, "text/css")
  , (533, "text/html")
  , (537, "text/plain")
  , (576, "text/x-perl")
  ]

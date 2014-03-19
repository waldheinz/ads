
{-# LANGUAGE OverloadedStrings #-}

module Freenet.Mime (
  Mime, defaultMimes
  ) where

import qualified Data.Text as T
import Data.Word

type Mime = T.Text

defaultMimes :: [(Word16, Mime)]
defaultMimes =
  [ (449, "image/jpeg")
  ]

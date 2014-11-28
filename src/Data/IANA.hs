{-# LANGUAGE TemplateHaskell #-}
module Data.IANA (
    module X,
    fileDate,
    languageTags
) where

import Data.IANAQ      (loadRegistry)
import Data.IANAQ as X (LanguageTag (..), Type (..))

loadRegistry

-- fileDate :: Day

-- languageTags :: [LanguageTag]

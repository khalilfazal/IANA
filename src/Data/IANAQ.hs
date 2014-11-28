{-# Language QuasiQuotes, TemplateHaskell #-}
module Data.IANAQ (
    loadRegistry,
    LanguageTag (..),
) where

import Control.Applicative        ((<*))
import Control.Error.Safe         (rightMay)
import Control.Monad              (void)
import Data.Composition           ((.:))
import Data.FileQ                 (f)
import Data.Functor.Bind          (($>))
import Data.Maybe                 (fromJust)
import Data.Time.Calendar         (Day, fromGregorian)
import Language.Haskell.TH        (DecsQ)
import Language.Haskell.TH.Lift   (deriveLift)
import Language.Haskell.TH.Syntax (lift)
import Text.Parsec                ((<|>), Parsec, anyChar, char, choice, digit, eof, letter, newline, parse, string, try)
import Text.Parsec.Combinator     (lookAhead, many1, manyTill)
import Text.Parsec.Language       (haskell)
import Text.Parsec.Perm           ((<$$>), (<$?>), (<|?>), permute)
import Text.Parsec.Token          (decimal)

-- instance Day
deriveLift ''Day

-- decimal parser
pDecimal :: Parsec String () Integer
pDecimal = decimal haskell

-- Date parser
pDate :: Parsec String () Day
pDate = do
    y <- pDecimal
    m <- char '-' >> fmap fromInteger pDecimal
    d <- char '-' >> fmap fromInteger pDecimal
    return $ fromGregorian y m d

data Type = Language | ExtendedLang | Script | Region | Variant | Grandfathered | Redundant
    deriving Show

deriveLift ''Type

-- Type parser
pType :: Parsec String () Type
pType = choice
    [string "language"      $> Language,
     string "extlang"       $> ExtendedLang,
     string "script"        $> Script,
     try (string "region"   $> Region),
     string "variant"       $> Variant,
     string "grandfathered" $> Grandfathered,
     string "redundant"     $> Redundant]

data LanguageTag = LanguageTag {
    ty             :: Type,
    tag            :: Maybe String,
    subTag         :: Maybe String,
    description    :: [String],
    added          :: Day,
    deprecated     :: Maybe Day,
    suppressScript :: Maybe String,
    scope          :: Maybe String,
    macrolanguage  :: Maybe String,
    preferredValue :: Maybe String,
    comments       :: Maybe String,
    prefix         :: Maybe [String]
} deriving Show

deriveLift ''LanguageTag

-- Charactor groups
alnum :: Parsec String () Char
alnum = letter <|> digit

eolEof :: Parsec String () ()
eolEof = void newline <|> eof

hyphened :: Parsec String () Char
hyphened = letter <|> char '-'

-- parser for optional key values
pOption :: String -> Parsec String () a -> (Maybe a, Parsec String () (Maybe a))
pOption = toMaybe .: pKeyValue

toMaybe :: Functor f => f b -> (Maybe a, f (Maybe b))
toMaybe = (,) Nothing . fmap Just

pKeyValue :: String -> Parsec String () a -> Parsec String () a
pKeyValue x = try . (string x >>) . (<* eolEof)

-- parser for LanguageTag
pLanguageTag :: Parsec String () LanguageTag
pLanguageTag = do
    t <- string "%%\nType: " >> pType <* newline
    (tg, sTag) <- permute $ (,)
        <$?> pOption "Tag: "    (many1 (alnum <|> char '-'))
        <|?> pOption "Subtag: " (many1 (alnum <|> char '.'))
    desc <- many1 (string "Description: " >> manyTill anyChar newline)
    (addDate, depDate, sscript, s, m, v, c, p) <- permute $ (,,,,,,,)
        <$$> try (string "Added: " >> pDate <* eolEof)
        <|?> pOption "Deprecated: "      pDate
        <|?> pOption "Suppress-Script: " (many1 letter)
        <|?> pOption "Scope: "           (many1 hyphened)
        <|?> pOption "Macrolanguage: "   (many1 letter)
        <|?> pOption "Preferred-Value: " (many1 (alnum <|> char '-'))
        <|?> pOption "Comments: "        (manyTill anyChar (lookAhead eolEof))
        <|?> ((toMaybe . many1) .: pKeyValue) "Prefix: " (many1 hyphened)
    return $ LanguageTag t tg sTag desc addDate depDate sscript s m v c p

data Registry = Registry {
    fileDate  :: Day,
    languageTags :: [LanguageTag]
} deriving Show

-- parser for Registry
pRegistry :: Parsec String () Registry
pRegistry = do
    date  <- string "File-Date: " >> pDate <* newline
    langs <- many1 pLanguageTag
    return $ Registry date langs

loadRegistry :: DecsQ
loadRegistry =
    [d| fileDate :: Day
        fileDate = $(lift date)

        languageTags :: [LanguageTag]
        languageTags = $(lift langs) |]
    where
        registry = (fromJust . rightMay . parse pRegistry "") [f|registry.txt|]
        date     = fileDate registry
        langs    = languageTags registry

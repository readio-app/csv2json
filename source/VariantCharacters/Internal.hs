{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module VariantCharacters.Internal where

import           Data.ByteString    (ByteString)
import           Data.FileEmbed     (embedFile)
import           Data.Map.Strict    (Map)
import           Data.Maybe         (catMaybes)
import           Data.Text          (Text)
import           Prelude            hiding (lines)

import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

makeVariantCharacterMap :: ByteString -> Map Char Char
makeVariantCharacterMap rawData =
        M.fromList
            $ catMaybes
            $ fmap parseLine
            $ T.splitOn "\r\n"
            $ T.decodeUtf8 rawData
    where
        parseLine :: Text -> Maybe (Char, Char)
        parseLine t = case T.unpack t of
            [a, b] -> Just (a, b)
            _      -> Nothing

rawVariantCharacterDatabase :: ByteString
rawVariantCharacterDatabase = $(embedFile "data/variant-characters")

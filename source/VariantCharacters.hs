{-# LANGUAGE TemplateHaskell #-}

module VariantCharacters
    ( replaceVariantCharacter
    , replaceVariantCharacters
    ) where

import           Data.Map.Strict            (Map)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (lift)
import           VariantCharacters.Internal

import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T

variantCharacterMap :: Map Char Char
variantCharacterMap = $(lift $ makeVariantCharacterMap rawVariantCharacterDatabase)

replaceVariantCharacter :: Char -> Char
replaceVariantCharacter a = fromMaybe a $ M.lookup a variantCharacterMap

replaceVariantCharacters :: Text -> Text
replaceVariantCharacters = T.map replaceVariantCharacter

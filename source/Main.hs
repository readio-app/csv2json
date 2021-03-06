{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (mzero)
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Function              (on)
import           Data.List                  (filter, groupBy)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, pack)
import           Data.Text.Encoding         (decodeUtf8)
import           GHC.Generics               (Generic)
import           Prelude                    hiding (id)
import           VariantCharacters          (replaceVariantCharacters)

import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as BC8
import qualified Data.ByteString.Lazy.Char8  as BL
import qualified Data.ByteString.Lazy.Search as BLS
import qualified Data.Csv                    as Csv
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import qualified System.Environment          as E
import qualified System.Exit                 as E

csvToJson :: CsvData -> JsonData
csvToJson = makeDays

makeDays :: [CsvRow] -> Days
makeDays rows = Days $ map makeDay $ groupByDate rows where
    groupByDate = groupBy ((==) `on` cDate)

makeDay :: [CsvRow] -> Day
makeDay rows = Day (cDate $ head rows) $
    makeHalfDay Morning rows <>
    makeHalfDay Evening rows

makeHalfDay :: Time -> [CsvRow] -> [Quotation]
makeHalfDay t rs = f <$> zip is ss
    where
        is = [1 .. 9] :: [Integer]
        ss = filter ((==) t . cTime) rs
        u = case t of
            Morning -> "0"
            Evening -> "1"
        f :: (Integer, CsvRow) -> Quotation
        f (i, r) = Quotation
            { qId         = unpackDate (cDate r) <> u <> pack (show i)
            , qContent    = cContent    r
            , qAuthor     = cAuthor     r
            , qSource     = cSource     r
            , qLink       = cLink       r
            , qBackground = cBackground r
            , qBanner     = cBanner     r
            , qHasVoice   = cHasVoice   r
            }

-- CSV data types

type CsvData = [CsvRow]

data CsvRow = CsvRow
    { cDate       :: Date
    , cTime       :: Time
    , cContent    :: Text -- need to replace variant characters
    , cAuthor     :: Text -- need to replace variant characters
    , cSource     :: Text -- need to replace variant characters
    , cLink       :: Text
    , cBackground :: Text
    , cBanner     :: Text
    , cHasVoice   :: HasVoice
    } deriving (Generic, Show)

newtype Date = Date Text deriving (Eq, Show)

data Time = Morning | Evening deriving (Eq, Show)

newtype HasVoice = HasVoice Bool deriving (Eq, Show)

unpackDate :: Date -> Text
unpackDate (Date d) = d

hasVoice :: HasVoice -> Bool
hasVoice (HasVoice x) = x

instance Csv.FromRecord CsvRow

instance Csv.FromField Date where
    parseField x =
        if B.null x
            then error "Date missing from one or more records."
            else pure $ Date $ decodeUtf8 x

instance Csv.FromField Time where
    parseField x
        | x == "m"  = pure Morning
        | x == "e"  = pure Evening
        | otherwise = mzero

instance Csv.FromField HasVoice where
    parseField = pure . HasVoice <$> \case
        "TRUE"  -> True
        "FALSE" -> False
        ""      -> False
        _       -> error "Unexpected value in voice column."

-- JSON data types

type JsonData = Days

newtype Days = Days [Day] deriving (Generic, Show)

data Day = Day Date [Quotation] deriving (Generic, Show)

data Quotation = Quotation
    { qId         :: Text
    , qContent    :: Text
    , qAuthor     :: Text
    , qSource     :: Text
    , qLink       :: Text
    , qBackground :: Text
    , qBanner     :: Text
    , qHasVoice   :: HasVoice
    } deriving (Generic, Show)

instance ToJSON Days where
    toJSON (Days ds) = object
            [ "quotes" .= ds ]

instance ToJSON Day where
    toJSON (Day d qs) = object
            [ "date" .= unpackDate d
            , "data" .= qs ]

instance ToJSON Quotation where
    toJSON q = object $
            [ "id"       .=                           qId         q
            , "quote"    .= replaceVariantCharacters (qContent    q)
            , "author"   .= replaceVariantCharacters (qAuthor     q)
            , "from"     .= replaceVariantCharacters (qSource     q)
            , "link"     .=                           qLink       q
            , "bg"       .=                           qBackground q ]
        <>
            (if T.null $ qBanner q
               then [ {- don't output empty values -} ]
               else [ "bannerid" .= ("@" <> qBanner q) ])
        <>
            (if hasVoice (qHasVoice q)
               then [ "voice" .= True ]
               else [ {- don't output false values -} ])

-- Command line interface

usage :: String
usage = "usage: csv2json <template-json-file> <string-to-be-replaced> <input-csv-file> <output-json-file>"

main :: IO ()
main = do
    arguments <- E.getArgs
    case arguments of
        [t, s, c, j] -> transformCsvToJson t s c j
        _            -> exitWithUsageError

transformCsvToJson :: FilePath -> String -> FilePath -> FilePath -> IO ()
transformCsvToJson templateJsonFilePath stringToBeReplaced csvFilePath jsonFilePath = do
    templateJson <- BL.readFile templateJsonFilePath
    csvData <- BL.readFile csvFilePath
    case Csv.decode Csv.HasHeader csvData of
        Left  e -> putStrLn e
        Right r -> BL.writeFile jsonFilePath
            (BLS.replace
                (BC8.pack stringToBeReplaced)
                -- This is a horribly inefficient hack, but we don't
                -- care, as these files are generally quite small.
                (BL.reverse $ BL.drop 1 $ BL.reverse $ BL.drop 1
                    $ encodePretty $ csvToJson $ V.toList r)
            templateJson)

exitWithUsageError :: IO ()
exitWithUsageError = do
    putStrLn usage
    E.exitWith $ E.ExitFailure 1


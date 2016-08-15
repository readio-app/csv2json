{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (mzero, when)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Function (on)
import Data.List (filter, groupBy)
import Data.Monoid ((<>))
import Data.Text (Text, append, pack)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Prelude hiding (id)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified System.Environment as E
import qualified System.Exit as E

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
        is = [1 .. 9]
        ss = filter ((==) t . cTime) rs
        u = case t of
            Morning -> "0"
            Evening -> "1"
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

data Time = Morning | Evening deriving (Eq, Show)

data Date = Date Text deriving (Eq, Show)

unpackDate :: Date -> Text
unpackDate (Date d) = d

data CsvRow = CsvRow
    { cDate       :: Date
    , cTime       :: Time
    , cContent    :: Text
    , cAuthor     :: Text
    , cSource     :: Text
    , cLink       :: Text
    , cBackground :: Text
    , cBanner     :: Text
    , cHasVoice   :: Bool
    } deriving (Generic, Show)

instance Csv.FromRecord CsvRow

instance Csv.FromField Bool where
    parseField "TRUE"  = pure True
    parseField "FALSE" = pure False
    parseField ""      = pure False
    parseField _       = error "Unexpected value in voice column."

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

-- JSON data types

type JsonData = Days

data Days = Days [Day] deriving (Generic, Show)

data Day = Day Date [Quotation] deriving (Generic, Show)

data Quotation = Quotation
    { qId         :: Text
    , qContent    :: Text
    , qAuthor     :: Text
    , qSource     :: Text
    , qLink       :: Text
    , qBackground :: Text
    , qBanner     :: Text
    , qHasVoice   :: Bool
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
            [ "id"       .= qId         q
            , "quote"    .= qContent    q
            , "author"   .= qAuthor     q
            , "from"     .= qSource     q
            , "link"     .= qLink       q
            , "bg"       .= qBackground q ]
        <>
            (if T.null $ qBanner q
               then []
               else [ "bannerid" .= ("@" <> qBanner q) ])
        <>
            (if qHasVoice q
               then [ "voice" .= True ]
               else [])

-- Command line interface

usage = "usage: csv2json <path-to-csv-file> <path-to-json-file>"

main :: IO ()
main = do
    arguments <- E.getArgs
    case arguments of
        [c, j] -> transformCsvToJson c j
        _      -> exitWithUsageError

transformCsvToJson csvFilePath jsonFilePath = do
    csvData <- BL.readFile csvFilePath
    case Csv.decode Csv.HasHeader csvData of
        Left  e -> putStrLn e
        Right r -> BL.writeFile jsonFilePath $
            encodePretty $ csvToJson $ V.toList r

exitWithUsageError = do
    putStrLn usage
    E.exitWith $ E.ExitFailure 1


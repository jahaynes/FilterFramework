{-# LANGUAGE OverloadedStrings #-}

module Data.FilterFramework.WarcHelper where

import           Control.Monad.Trans.Resource
import           Data.Attoparsec.ByteString     -- (Result(..), parse)
import           Data.ByteString.Char8          as C8
import           Data.Warc.Body.Body
import           Data.Warc.Header.Header
import           Data.Warc.Header.HeaderLine    as HL
import           Data.Warc.Header.Key           as K   
import           Data.Warc.Header.Value         as V  
import           Data.Warc.WarcEntry                as W
import           Data.Conduit
import           Data.Conduit.Binary              (sourceFile, sinkFile)
import qualified Data.Conduit.List       as CL
import qualified Data.Conduit.Attoparsec as CA
import           Data.Maybe                       (mapMaybe)
import           Data.FilterFramework.Serialisation
import           Data.FilterFramework.Types

warcFileSource :: FilePath -> Source (ResourceT IO) WarcEntry 
warcFileSource wf = sourceFile wf $= byteStringToWarcEntries

    where
    byteStringToWarcEntries :: Filter ByteString WarcEntry
    byteStringToWarcEntries = CA.conduitParser warcEntry $= CL.map snd

warcToFiltered :: WarcEntry -> FilterDocument
warcToFiltered (WarcEntry _ (CompressedBody _)) = error "not implemented"
warcToFiltered (WarcEntry (WarcHeader _ headerLines) (UncompressedBody body)) =
    FilterDocument (mapMaybe warcHeaderToMetaDatum headerLines) (Pristine body) (Current body)

    where
    warcHeaderToMetaDatum :: HeaderLine -> Maybe Metadatum
    warcHeaderToMetaDatum (HeaderLine k v) = do
        let kbs = K.toByteString k
            vbs = V.toByteString v
            bs = C8.concat [kbs, "=", vbs, "\n"]
        case parse metaParser bs of
            (Done _ m@(Metadatum _ _)) -> Just m
            _ -> Nothing

warcFileSink :: FilePath -> Sink WarcEntry (ResourceT IO) ()
warcFileSink wf = CL.map W.toByteString $= sinkFile wf

filteredToWarc :: FilterDocument -> WarcEntry
filteredToWarc (FilterDocument meta _ (Current current)) = do
    let headers = mapMaybe metaDatumToWarcHeaderLine meta
    WarcEntry (WarcHeader (WarcVersion "1.0") (fixLength headers)) (UncompressedBody current)

    where
    fixLength :: [HeaderLine] -> [HeaderLine]
    fixLength [] = []
    fixLength ((HeaderLine (MandatoryKey ContentLength) _):xs) = (HeaderLine (MandatoryKey ContentLength) (IntValue (C8.length current))):xs
    fixLength (x:xs) = x : fixLength xs
        
    metaDatumToWarcHeaderLine :: Metadatum -> Maybe HeaderLine
    metaDatumToWarcHeaderLine (Metadatum (Key mk) (Value mv)) =
        case parse headerLine (C8.concat [mk, ":", mv, "\r\n"]) of
            (Done _ hl) -> Just hl
            _ -> Nothing

getUri :: WarcEntry -> ByteString
getUri (WarcEntry (WarcHeader _ xs) _) = 
    case getUri' xs of
        Just x -> x
        Nothing -> error "WarcEntry did not have a Record ID"

    where
    getUri' :: [HeaderLine] -> Maybe ByteString
    getUri' [] = Nothing
    getUri' (HeaderLine (MandatoryKey WarcRecordId) (StringValue s):_) = Just s
    getUri' (_:hs) = getUri' hs

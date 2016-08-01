{-# LANGUAGE OverloadedStrings #-}

module Data.FilterFramework.Serialisation where

import Data.Attoparsec.ByteString.Char8 as A        (takeWhile1, isSpace, skipSpace, char)
import Data.Attoparsec.ByteString.Lazy  as A        (Parser, string, take)
import Data.ByteString                              (ByteString)
import qualified Data.ByteString.Char8  as C8

import Data.FilterFramework.Types

filteredParser :: Parser FilterDocument
filteredParser = do

    nMeta <- string "Meta" *> skipSpace *> int <* skipSpace
    metas <- takeNThings nMeta metaParser

    szPristine <- string "Pristine" *> skipSpace *> int <* skipSpace
    pristine <- Pristine <$> A.take szPristine <* char '\n'

    szCurrent <- string "Current" *> skipSpace *> int <* skipSpace
    current <- Current <$> A.take szCurrent <* skipSpace

    pure (FilterDocument metas pristine current)

metaParser :: Parser Metadatum
metaParser = do
    _ <- skipSpace
    k <- Key <$> (skipSpace *> takeTill1 (=='=') <* char '=' <* skipSpace)
    v <- Value <$> takeTill1 isSpace
    pure (Metadatum k v)
    
takeNThings :: Int -> Parser a -> Parser [a]
takeNThings n p = mapM (\_ -> p) [1..n]

takeTill1 :: (Char -> Bool) -> Parser ByteString
takeTill1 f = takeWhile1 $ not . f

int :: Parser Int
int = do
    x <- takeTill1 isSpace
    case C8.readInt x of
        Just (i,_) -> pure i
        Nothing    -> fail ("Error.  Expected int instead of: " ++ show x)

filteredSerialiser :: FilterDocument -> ByteString
filteredSerialiser (FilterDocument meta (Pristine pristine) (Current current)) =
    let a = C8.concat ["Meta ",     C8.pack . show . length    $ meta]
        b = C8.concat ["Pristine ", C8.pack . show . C8.length $ pristine]
        c = C8.concat ["Current ",  C8.pack . show . C8.length $ current]
    in C8.intercalate "\n" [a, metadataToByteString meta, b, pristine, c, current]
    where
    metadataToByteString :: [Metadatum] -> ByteString
    metadataToByteString = C8.intercalate "\n" . map metadatumToByteString

metadatumToByteString :: Metadatum -> ByteString
metadatumToByteString (Metadatum (Key k) (Value v)) = C8.concat [k, "= ", v]
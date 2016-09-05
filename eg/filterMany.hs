{-# LANGUAGE OverloadedStrings #-}

import Data.FilterFramework.Types
import Data.FilterFramework.WarcHelper
import Data.Ord
import Data.List

main :: IO ()
main = filterManyMany aFilterOfManyThings

aFilterOfManyThings :: [FilterDocument] -> IO [FilterDocument]
aFilterOfManyThings xs = do

    return . sortBy (comparing getDocId) $ xs

    where
    getDocId = head
             . map (\(Metadatum _ (Value v)) -> v)
             . filter (\(Metadatum (Key k) _) -> k == warcRecordId)
             . getMeta

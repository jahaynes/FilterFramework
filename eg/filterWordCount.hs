{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8  as C8
import           Data.FilterFramework.Types
import           Data.FilterFramework.WarcHelper

main :: IO ()
main = filterDocumentViaWarcViaStdIo wordCountFilter

wordCountFilter :: FilterDocument -> [FilterDocument]
wordCountFilter (FilterDocument metadata pristine (Current current)) =

    let wc = Metadatum
                 (Key "Word-Count")
                 (Value (C8.pack . show . length . C8.words $ current))

    in
    [FilterDocument (wc:metadata) pristine (Current current)]

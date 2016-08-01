import Data.FilterFramework.Types
import Data.FilterFramework.WarcHelper

import qualified Data.ByteString.Char8 as C8
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.List as CL

import System.Environment (getArgs)

{- Load a warc file from the first argument,
   Convert each warc entry to a 'filter document'
   Run a function on it
   convert it back to warc entry
   write it back to a warc file (second argument) -}
main :: IO ()
main = do

    (a:b:[]) <- getArgs

    runResourceT $  warcFileSource a
                 $= CL.map warcToFiltered
                 $= metaDataIntoBody
                 $= CL.map filteredToWarc
                 $$ warcFileSink b

metaDataIntoBody :: Filter FilterDocument FilterDocument
metaDataIntoBody = awaitForever $ \(FilterDocument meta pristine _) -> do
    let newBs = C8.pack . concatMap show $ meta
    liftIO $ mapM_ print meta
    yield (FilterDocument meta pristine (Current newBs))
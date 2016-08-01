module Data.FilterFramework.FilterChain where

import           Control.Monad.Trans.Resource     (runResourceT)
import           Data.ByteString                  (ByteString)
import           Data.Conduit
import           Data.Conduit.Binary              (sourceHandle, sinkHandle)
import qualified Data.Conduit.List       as CL
import qualified Data.Conduit.Attoparsec as CA
import System.IO                                  (stdin, stdout)

import Data.FilterFramework.Serialisation
import Data.FilterFramework.Types

filteredToByteString :: Filter FilterDocument ByteString
filteredToByteString = CL.map filteredSerialiser

byteStringToFiltered :: Filter ByteString FilterDocument
byteStringToFiltered = CA.conduitParser filteredParser $= CL.map snd

runFilterChain :: Filter FilterDocument FilterDocument -> IO ()
runFilterChain chain = runResourceT $  sourceHandle stdin
                                    $= byteStringToFiltered
                                    $= chain
                                    $= filteredToByteString
                                    $$ sinkHandle stdout

function :: (a -> a) -> Filter a a
function = CL.map

method f = awaitForever $ \a -> (f a >>= yield)
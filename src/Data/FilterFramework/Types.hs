module Data.FilterFramework.Types where

import Control.Monad.Trans.Resource     (ResourceT)
import Data.ByteString                  (ByteString)
import Data.Conduit                     (Conduit)
import Data.Warc.WarcEntry              (WarcEntry)

newtype Key = Key ByteString deriving Show

newtype Value = Value ByteString deriving Show

data Metadatum = Metadatum Key Value deriving Show

newtype Pristine = Pristine ByteString deriving Show

newtype Current = Current ByteString deriving Show

data FilterDocument = FilterDocument
                    { getMeta :: [Metadatum]
                    , getPristine :: Pristine
                    , getCurrent :: Current } deriving Show
                    
type Filter a b = Conduit a (ResourceT IO) b

type FilterInput = Filter () FilterDocument

type FilterOutput = Filter WarcEntry ()
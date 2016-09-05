module Data.FilterFramework.Types where

import Control.Monad.Trans.Resource     (ResourceT)
import Data.ByteString                  (ByteString)
import Data.Conduit                     (Conduit)

newtype Key = Key ByteString deriving (Eq, Ord, Show)

newtype Value = Value ByteString deriving (Eq, Ord, Show)

data Metadatum = Metadatum Key Value deriving (Eq, Ord, Show)

newtype Pristine = Pristine ByteString deriving (Eq, Ord, Show)

newtype Current = Current ByteString deriving (Eq, Ord, Show)

data FilterDocument = FilterDocument
                    { getMeta :: [Metadatum]
                    , getPristine :: Pristine
                    , getCurrent :: Current } deriving Show
                    
type Filter a b = Conduit a (ResourceT IO) b

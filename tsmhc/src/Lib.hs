module Lib
  ( Hashconsed(..)
  , HashconsedTable
  , newEmptyTable
  , hashcons
  ) where

import Data.Hashable
import qualified Data.HashMap.Strict as HashMap

-- | Data structure for hash consed values.
data Hashconsed a = Hashconsed
  { value :: a      -- ^ Original value
  } deriving (Eq, Show)

-- | Type alias for a hash consed table, mapping Int hash keys to `Hashconsed a` values.
type HashconsedTable a = HashMap.HashMap a (Hashconsed a)

-- | Create a new, empty `HashconsedTable`.
newEmptyTable :: HashconsedTable a
newEmptyTable = HashMap.empty

{-|
   Perform a lookup or insert operation on a `HashconsedTable`.

   If `val` is already in the table, return the existing `Hashconsed` value and the same table.
   If `val` is not in the table, insert it, return the new `Hashconsed` value and the modified table.
-}
hashcons :: Hashable a => a -> HashconsedTable a -> (Hashconsed a, HashconsedTable a)
hashcons val table =
  case HashMap.lookup val table of
    Just hc -> (hc, table)  -- Value found, return existing Hashconsed and original table
    Nothing -> 
      let hc = Hashconsed val
      in (newHc, HashMap.insert val hc table)  -- Value not found, insert and return new table

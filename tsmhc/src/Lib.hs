module Lib
  ( Hashconsed(..)
  , HashconsedTable
  , newEmptyTable
  , construct
  , lookupOrInsert
  , mapTable
  , clearTable
  , iterateTable
  ) where

import Data.Hashable
import qualified Data.HashMap.Strict as HashMap

-- | Data structure for hash consed values.
data Hashconsed a = Hashconsed
  { value :: a      -- ^ Original value
  , hashKey :: Int  -- ^ Hash key of the value
  } deriving (Eq, Show)

-- | Type alias for a hash consed table, mapping Int hash keys to `Hashconsed a` values.
type HashconsedTable a = HashMap.HashMap Int (Hashconsed a)

-- | Create a new, empty `HashconsedTable`.
newEmptyTable :: HashconsedTable a
newEmptyTable = HashMap.empty

-- | Construct a `Hashconsed` value, given an original value.
construct :: Hashable a => a -> Hashconsed a
construct val = Hashconsed val (hash val)  -- Use hash function from Data.Hashable

{-|
   Perform a lookup or insert operation on a `HashconsedTable`.

   If `val` is already in the table, return the existing `Hashconsed` value and the same table.
   If `val` is not in the table, insert it, return the new `Hashconsed` value and the modified table.
-}
lookupOrInsert :: (Eq a, Hashable a) => a -> HashconsedTable a -> (Hashconsed a, HashconsedTable a)
lookupOrInsert val table =
  let hashedVal = hash val  -- Compute hash once
  in case HashMap.lookup hashedVal table of
    Just hc -> (hc, table)  -- Value found, return existing Hashconsed and original table
    Nothing -> 
      let newHc = construct val
      in (newHc, HashMap.insert hashedVal newHc table)  -- Value not found, insert and return new table

{-|
   Map a function over all `Hashconsed` values in the table.
-}
mapTable :: Hashable b => (a -> b) -> HashconsedTable a -> HashconsedTable b
mapTable f = HashMap.map (construct . f . value)  -- Apply function, then rehash values

-- | Return an empty table, effectively clearing the given table.
clearTable :: HashconsedTable a -> HashconsedTable a
clearTable _ = HashMap.empty


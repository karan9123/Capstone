{-# LANGUAGE GADTs #-}
module LibPure(
    HC,
    hashconsPure
) where

import System.IO.Unsafe
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import System.Mem.Weak (Weak, mkWeakPtr, deRefWeak)
import Control.Monad (forM_)
import Control.Concurrent.MVar

-- A newtype for the hash-consed value
{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}
data HC a where
  HC :: (Eq a, Hashable a) => {
    value :: !a
  } -> HC a


-- | Type alias for a hash consed table, mapping Int hash keys to `HConsed a` values.
type PureHCTable a = HashMap.HashMap a (HC a)

-- | Create a new, empty `HashconsedTable`.
newEmptyPureHCTable :: PureHCTable a
newEmptyPureHCTable = HashMap.empty

{-|
   Perform a lookup or insert operation on a `HashconsedTable`.

   If `val` is already in the table, return the existing `Hashconsed` value and the same table.
   If `val` is not in the table, insert it, return the new `Hashconsed` value and the modified table.
-}
hashconsPure :: Hashable a => a -> PureHCTable a -> (HC a, PureHCTable a)
hashconsPure val table =
  case HashMap.lookup val table of
    Just hConsed -> (hConsed, table)  -- Value found, return existing Hashconsed and original table
    Nothing -> 
      let newHC = HC val
      in (newHC, HashMap.insert val newHC table)  -- Value not found, insert and return new table
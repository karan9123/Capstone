{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, GADTs #-}

module Lib(
    HC, -- Data Type of HashConsed Value
    HashCons, -- Name of the class
    hashCons, -- Function which creates and returns HC value while updating table
    getValue, -- Get the value from HashConsed 
    getTable, -- Get the table in which the value resides
    printTable -- Print the table in which the value resides
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

-- Eq logic of HC
instance Eq (HC a) where
    (HC x) == (HC y) = x == y
  
-- Hash logic of HC based on its value
instance Hashable (HC a) where
    hashWithSalt s (HC x) = hashWithSalt s x

-- Show logic of HC 
instance Show a => Show (HC a) where
    show (HC x) = "HC: " ++ show x

-- Get the raw value from the HC
getValue :: HC a -> a
getValue (HC val) = val
                  
-- Table type which holds weak references of HC 
-- as values and the value corresponding to the HC as keys
type HCTable a = MVar (HashMap.HashMap a (Weak (HC a)))

-- Create a new empty hash-consing table
newEmptyHCTable :: IO (HCTable a)
newEmptyHCTable = newMVar HashMap.empty


-- Function to either insert a new HC value or retrieve an existing one
addOrRetrieve :: Hashable a => a -> HCTable a -> IO (HC a)
addOrRetrieve val tableRef = do
  table <- readMVar tableRef
  -- Attempt to retrieve the value from the table
  case HashMap.lookup val table of
    Just weakHC -> do
      -- Dereference the weak pointer to get the actual HC value
      maybeHC <- deRefWeak weakHC
      case maybeHC of
        Just hc -> return hc
        Nothing -> helper val tableRef  -- If value is GC'ed, insert a new one
    Nothing -> helper val tableRef     -- Value not found, insert a new one
    where
      helper valu hcTable = do
        table <- takeMVar hcTable
        -- Create a weak reference to the value, with a finalizer to remove it from the table when GC'ed
        weakRef <- mkWeakPtr (HC valu) (Just $ finalizer valu hcTable)
        putMVar hcTable (HashMap.insert valu weakRef table)
        tableRead <- readMVar hcTable
        -- Ensure the value was properly inserted/retrieved
        case HashMap.lookup valu tableRead of
          Just weakHC -> do
            maybeHC <- deRefWeak weakHC
            case maybeHC of
              Just hc -> return hc
              Nothing -> return (HC valu)
          Nothing -> return (HC valu)

-- Finalizer to remove a value from the table when it's garbage collected
finalizer :: Hashable a => a -> HCTable a -> IO ()
finalizer key tableRef = do
  table <- takeMVar tableRef
  putMVar tableRef (HashMap.delete key table)

-- Typeclass for hash-consable values
class (Eq a, Hashable a) => HashCons a where
  hashConsedTable :: HCTable a
  hashConsedTable = unsafePerformIO newEmptyHCTable
  {-# NOINLINE hashConsedTable #-}

-- Function to create or retrieve a hash-consed value
hashCons ::  HashCons a  => a -> HC a
hashCons val = unsafePerformIO $ addOrRetrieve val hashConsedTable

-- Retrieve the hash-consing table (not dependent on the given value)
getTable :: HashCons a => HC a -> HCTable a
getTable _ = hashConsedTable

-- Print the contents of the hash-consing table
printTable :: Show a => HCTable a -> IO ()
printTable hcTable = do
    table <- readMVar hcTable 
    forM_ (HashMap.toList table) $ \(key, weakVal) -> do
        maybeHC <- deRefWeak weakVal
        case maybeHC of
            Just hConsed -> putStrLn $ "Key: " ++ show key ++ " Value: " ++ show hConsed
            Nothing -> putStrLn $ "Reference not found for Key: " ++ show key

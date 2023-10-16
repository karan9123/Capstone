{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, GADTs #-}

module Lib(
    Hashconsed,
    HCTable,
    newEmptyHCTable,
    hashCons,
    printTable,
    finalizer,
    HashConsedClass,
    hc,
    getValue, 
    getTable
    ) where

import System.IO.Unsafe
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import System.Mem.Weak (Weak, mkWeakPtr, deRefWeak)
import Control.Monad (forM_)
import Control.Concurrent.MVar

-- A newtype for the hash-consed value
{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}
data Hashconsed a where
  Hashconsed :: (Eq a, Hashable a) => {
    value :: !a
  } -> Hashconsed a

-- Define the hash logic of the hash-consed value
instance Eq (Hashconsed a) where
    (Hashconsed x) == (Hashconsed y) = x == y
  
-- Define the hash logic of the hash-consed value
instance Hashable (Hashconsed a) where
    hashWithSalt s (Hashconsed x) = hashWithSalt s x

instance Show a => Show (Hashconsed a) where
    show (Hashconsed x) = "HC: " ++ show x

getValue :: Hashconsed a -> a
getValue (Hashconsed val) = val
                  
-- Define the table type which holds weak references of hash-consed values
type HCTable a = MVar (HashMap.HashMap a (Weak (Hashconsed a)))

newEmptyHCTable :: IO (HCTable a)
newEmptyHCTable = newMVar HashMap.empty

-- Function to insert or retrieve hash-consed values from the table
hashCons :: Hashable a => a -> HCTable a -> IO (Hashconsed a)
hashCons val tableRef = do
  table <- readMVar tableRef
  -- Lookup value from table
  case HashMap.lookup val table of
    Just weakHC -> do
      -- Get HashConsed Value from Weak Pointer
      maybeHashconsed <- deRefWeak weakHC
      case maybeHashconsed of
        Just hConsed -> return hConsed
        Nothing -> helper val tableRef 
    Nothing -> helper val tableRef
    where
      helper valu hcTable = do
        table <- takeMVar hcTable
        weakRef <- mkWeakPtr (Hashconsed valu) (Just $ finalizer valu hcTable)
        putMVar hcTable (HashMap.insert valu weakRef table)
        tableRead <- readMVar hcTable
        case HashMap.lookup valu tableRead of
          Just weakHC -> do
            maybeHashconsed <- deRefWeak weakHC
            case maybeHashconsed of
              Just hconsed -> return hconsed
              Nothing -> return (Hashconsed valu)
          Nothing -> return (Hashconsed valu)


finalizer :: Hashable a => a -> HCTable a -> IO ()
finalizer key tableRef = do
  putStrLn "removing value from table"
  table <- takeMVar tableRef
  putMVar tableRef (HashMap.delete key table)

class (Eq a, Hashable a) => HashConsedClass a where
  hashConsedTable :: HCTable a
  hashConsedTable = unsafePerformIO newEmptyHCTable
  {-# NOINLINE hashConsedTable #-}

-- | Make a hash-consed value.
hc ::  HashConsedClass a  => a -> Hashconsed a
hc val = unsafePerformIO $ hashCons val hashConsedTable

getTable :: HashConsedClass a => Hashconsed a -> HCTable a
getTable _ = hashConsedTable

printTable :: Show a => HCTable a -> IO ()
printTable hcTable = do
    table <- readMVar hcTable 
    forM_ (HashMap.toList table) $ \(key, weakVal) -> do
        maybeHC <- deRefWeak weakVal
        case maybeHC of
            Just hConsed -> putStrLn $ "Key: " ++ show key ++ " Value: " ++ show hConsed
            Nothing -> putStrLn $ "Reference not found for Key: " ++ show key


-- | Type alias for a hash consed table, mapping Int hash keys to `Hashconsed a` values.
type PureHashConsedTable a = HashMap.HashMap a (Hashconsed a)

-- | Create a new, empty `HashconsedTable`.
newEmptyPureHCTable :: PureHashConsedTable a
newEmptyPureHCTable = HashMap.empty

{-|
   Perform a lookup or insert operation on a `HashconsedTable`.

   If `val` is already in the table, return the existing `Hashconsed` value and the same table.
   If `val` is not in the table, insert it, return the new `Hashconsed` value and the modified table.
-}
hashconsPure :: Hashable a => a -> PureHashConsedTable a -> (Hashconsed a, PureHashConsedTable a)
hashconsPure val table =
  case HashMap.lookup val table of
    Just hConsed -> (hConsed, table)  -- Value found, return existing Hashconsed and original table
    Nothing -> 
      let newHC = Hashconsed val
      in (newHC, HashMap.insert val newHC table)  -- Value not found, insert and return new table
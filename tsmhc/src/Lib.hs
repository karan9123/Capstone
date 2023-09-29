-- | Lib.hs
module Lib
    ( Hashconsed(..)       -- Exporting the Hashconsed data type and its constructors
    , HashconsTable        -- Exporting the HashconsTable type
    , newEmptyTable        -- Exporting the newEmptyTable function
    , clearTable           -- Exporting the clearTable function
    , lookupOrInsert       -- Exporting the lookupOrInsert function
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import System.Mem.StableName
import Data.Hashable
import Data.IORef

-- | The 'Hashconsed' data type represents a hash-consed value.
-- It contains the original value, a stable name, and a hash key of the original value.
data Hashconsed a = Hashconsed
    { node :: a            -- Original value
    , stableName :: Int    -- Stable name of the value
    , hkey :: Int          -- Hash key of the value
    } deriving (Show)

-- | 'Hashable' instance for 'Hashconsed' to allow it to be used as a key in 'HashMap'.
instance Hashable (Hashconsed a) where
    hashWithSalt salt hc = hashWithSalt salt (hkey hc)

-- | 'Eq' instance for 'Hashconsed' to compare based on stable names.
instance Eq (Hashconsed a) where
    hc1 == hc2 = stableName hc1 == stableName hc2

-- | The 'HashconsTable' type represents a mutable hash-consing table.
newtype HashconsTable a = HashconsTable (IORef (HashMap Int (Hashconsed a)))

-- | 'newEmptyTable' creates a new empty 'HashconsTable'.
newEmptyTable :: IO (HashconsTable a)
newEmptyTable = HashconsTable <$> newIORef HashMap.empty

-- | 'clearTable' clears all entries in the given 'HashconsTable'.
clearTable :: HashconsTable a -> IO()
clearTable (HashconsTable tableRef) = modifyIORef' tableRef (const HashMap.empty)

-- | 'lookupOrInsert' looks up or inserts a value in the 'HashconsTable'.
-- It returns the hash-consed value from the table.
lookupOrInsert :: Hashable a => a -> HashconsTable a -> IO (Hashconsed a)
lookupOrInsert val (HashconsTable tableRef) = do
    stableName <- makeStableName val
    let tag = hashStableName stableName
    hm <- readIORef tableRef
    case HashMap.lookup tag hm of
        Just hc -> return hc  -- Return existing hash-consed value if found
        Nothing -> do         -- Otherwise, insert new hash-consed value and return it
            let hc = Hashconsed val tag (hash val)
            writeIORef tableRef (HashMap.insert tag hc hm)
            return hc

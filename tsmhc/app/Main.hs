{-# LANGUAGE DeriveAnyClass, PatternSynonyms, ViewPatterns, BangPatterns #-}
-- import Lib
import Lib (HashConsedClass, Hashconsed, hc, getValue, printTable, getTable)
import Data.Hashable
import Data.Bits (xor)
import System.Mem
import Control.Exception


-- data List a = Nil | Cons a (List a) deriving (Eq, Show)
-- instance Hashable a => Hashable (List a) where
--   hashWithSalt _ Nil = 0
--   hashWithSalt val (Cons a l) = hashWithSalt val a `xor` hashWithSalt val l
--   hash Nil = 0
--   hash (Cons a l) = hash a `xor` hash l

-- testList :: IO ()
-- testList = do
--     print "testing List: "
--     let table = newEmptyTable
--     let list1 = Cons "1" (Cons "2" Nil)
--     let list2 = Cons "1" (Cons "2" Nil)
--     let list3 = Cons "1" (Cons "3" Nil)
--     let (hc1, table1) = lookupOrInsert list1 table 
--     let (hc2, table2) = lookupOrInsert list2 table1
--     let (hc3, _) = lookupOrInsert list3 table2
--     print $ hc1 == hc2 -- true
--     print $ hc1 == hc3 -- false


-- type Id = Hashconsed String

type BoolFormula = Hashconsed BoolFormula'

data BoolFormula' = 
    Var' String 
  | And' BoolFormula BoolFormula 
  | Or' BoolFormula BoolFormula 
  deriving (Eq, Show, HashConsedClass)

instance Hashable BoolFormula' where
  hashWithSalt val (Var' a) = hashWithSalt val a
  hashWithSalt val (And' a1 a2) = (hashWithSalt val a1 `xor` hashWithSalt val a2)`xor` hashWithSalt (val + 10003344) a2
  hashWithSalt val (Or' a1 a2) = hashWithSalt val a1 `xor` hashWithSalt val a2

pattern Var :: String -> BoolFormula
pattern Var str <- (getValue -> Var' str)
  where
    Var str = hc (Var' str)
                                          
pattern And :: BoolFormula -> BoolFormula -> BoolFormula
pattern And x y <- (getValue -> (And' x y))
  where
    And x y = hc (And' x y)

pattern Or :: BoolFormula -> BoolFormula -> BoolFormula
pattern Or x y <- (getValue -> (Or' x y))
  where
    Or x y = hc (Or' x y)

-- -- Sample test cases for BoolFormula
-- boolFormulaTest :: IO ()
-- boolFormulaTest = do
--     print "testing boolFormula: "
--     -- let table  = newEmptyTableW
--     let varP = Var "p"
--     let varPP = Var "p"
--     let varQ = Var "q"
--     let hc1 = And varP varQ
--     let hc2 = And varP varQ
--     let hc3 = Or varP varQ
--     print hc1
--     -- let hc2 = And varP varQ
--     -- let hc3 = Or varP varQ
--     -- -- let (hc1, table1) = lookupOrInsertW formula1 table 
--     -- -- let (hc2, table2) = lookupOrInsertW formula2 table1 
--     -- -- let (hc3, _) = lookupOrInsert formula3 table2 
--     print $ hc1 == hc2  -- Should print True, demonstrating memory saving
--     print $ hc1 == hc3  -- Should print False
--     print $ varP == varPP  -- Should print False


-- Main Function to run the tests
main :: IO ()
main = do
  print "testing boolFormulaTest"
  let !varP = Var "p"
  let varPP = Var "p"
  let varQ = Var "q"
 

  -- printTable varQ
  -- performMajorGC
  let hc1 = And varP varQ
  let hc2 = And varP varQ
  let hc3 = Or varP varQ
  -- print $ hc1 == hc2
  printTable(getTable varQ)
  print "Table printed"
  -- printTable varP
  performMajorGC
  print "Starting Table print"
  printTable(getTable varQ)
  performGC
  -- _ <- return ()
  -- print "Starting Table print"
  -- printTable(getTable varQ)
  -- print "Table printed"
  print "testing bool"

  -- performMajorGC
  -- putStrLn "Garbage collection performed"

  -- putStrLn ""
  -- putStrLn "Table:"
  -- _ <- printTable table
  -- putStrLn ""

  -- _ <- clearTable table
  -- putStrLn "Clearing Table"

  -- print $ hc1 == hc2
  -- print hc1
  -- printTable table
  -- putStrLn "Table End"



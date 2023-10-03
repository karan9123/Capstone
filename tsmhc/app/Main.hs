import Lib
import Data.Hashable
import Data.Bits (xor)  




data BoolFormula = Var String | And BoolFormula BoolFormula | Or BoolFormula BoolFormula deriving (Eq, Show)
instance Hashable BoolFormula where
  hashWithSalt val (Var a) = hashWithSalt val a
  hashWithSalt val (And a1 a2) = (hashWithSalt val a1 `xor` hashWithSalt val a2)`xor` hashWithSalt val a2
  hashWithSalt val (Or a1 a2) = hashWithSalt val a1 `xor` hashWithSalt val a2


data List a = Nil | Cons a (List a) deriving (Eq, Show)


instance Hashable a => Hashable (List a) where
  hashWithSalt _ Nil = 0
  hashWithSalt val (Cons a l) = hashWithSalt val a `xor` hashWithSalt val l
  hash Nil = 0
  hash (Cons a l) = hash a `xor` hash l



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

-- Sample test cases for BoolFormula
boolFormulaTest :: IO ()
boolFormulaTest = do
    -- print "testing boolFormula: "
    let table  = newEmptyTable
    let formula1 = And (Var "p") (Var "q")
    let formula2 = And (Var "p") (Var "q")
    let formula3 = Or (Var "p") (Var "q")
    let (hc1, table1) = lookupOrInsert formula1 table 
    let (hc2, table2) = lookupOrInsert formula2 table1 
    let (hc3, _) = lookupOrInsert formula3 table2 
    print $ hc1 == hc2  -- Should print True, demonstrating memory saving
    print $ hc1 == hc3  -- Should print False


-- Main Function to run the tests
main :: IO ()
main = do
        testList
        boolFormulaTest
  -- _ <- runTestTT listTest
  -- boolFormulaTest


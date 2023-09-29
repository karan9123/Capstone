-- Main.hs
import Lib
import Data.Hashable (Hashable, hashWithSalt)  -- Import the Hashable type class
import Data.Bits (xor)           -- Import xor for hashing

-- data List a = Nil | Cons a (List a) deriving (Eq, Show)


-- instance (Eq a, Hashable a) => Hashable (List a) where
--   hashWithSalt _ Nil = 0
--   hashWithSalt val (Cons a l) = hashWithSalt val a `xor` hashWithSalt val l
-- --   hash Nil = 0
-- --   hash (Cons a l) = hash a `xor` hash l


-- testList :: IO ()
-- testList = do
--     table <- newEmptyTable
--     let list1 = Cons "1" (Cons "2" Nil)
--     let list2 = Cons "1" (Cons "2" Nil)
--     let list3 = Cons "1" (Cons "3" Nil)
--     hc1 <- lookupOrInsert list1 table 
--     hc2 <- lookupOrInsert list2 table
--     hc3 <- lookupOrInsert list3 table
--     print $ hc1 == hc2  
--     print $ hc1 == hc3

-- main :: IO ()
-- main = testList






data BoolFormula = Var String | And BoolFormula BoolFormula | Or BoolFormula BoolFormula deriving (Eq, Show)

instance Hashable BoolFormula where
  hashWithSalt val (Var a) = hashWithSalt val a
  hashWithSalt val (And a1 a2) = hashWithSalt val a1 `xor` hashWithSalt val a2
  hashWithSalt val (Or a1 a2) = hashWithSalt val a1 `xor` hashWithSalt val a2
  
--   hash (Var a) = hash a
--   hash (And a1 a2) = hash a1 `xor` hash a2
--   hash (Or a1 a2) = hash a1 `xor` hash a2


testBoolFormula :: IO ()
testBoolFormula = do
    table <- newEmptyTable
    let formula1 = And (Var "p") (Var "q")
    let formula2 = And (Var "p") (Var "q")
    let formula3 = Or (Var "p") (Var "q")
    hc1 <- lookupOrInsert formula1 table 
    hc2 <- lookupOrInsert formula2 table 
    hc3 <- lookupOrInsert formula3 table 
    print $ hc1 == hc2  -- Should print True, demonstrating memory saving
    print $ hc1 == hc3  -- Should print False

main :: IO ()
main = testBoolFormula


  
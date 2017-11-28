module Data.URM.Prog
(
  -- | type (fully exported)
  Prog

  -- | query
, len
)
where

  import qualified Data.List as L

  import qualified Data.URM.Instruction as URM.Instruction

  -- | program data type definition
  type Prog = [URM.Instruction.Instruction]

  {-|
    The 'len' function returns the number of intruction in a program.
   -}
  len :: Prog -> Int
  len = L.length

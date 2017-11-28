module Data.URM.Reg
(
-- | types
  RegVal
, Reg(..)

 -- * constructing
, mk
, mkVal
)
where

  -- | register value type definition
  type RegVal = Int

  -- | register type definition
  newtype Reg = Reg { getVal :: RegVal}

  -- | Reg type Show type class instance
  instance Show Reg where
    show = show . getVal

  -- | Reg type Eq type class instance
  instance Eq Reg where
    r == r' = getVal r == getVal r'

  {-|
    The 'mk' function return a register with value 0.
  -}
  mk :: Reg
  mk = Reg { getVal = 0 }

  {-|
    The 'mk' x' function return a register with value 'x'.
  -}
  mkVal :: RegVal -> Reg
  mkVal x = Reg { getVal = x }

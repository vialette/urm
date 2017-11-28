module Data.URM.Instruction
(
  -- * type (fully exported)
  Instruction(..)
, IInstruction

  -- * Constructing
, mkZero
, mkSucc
, mkCopy
, mkJmp
)
where

  import qualified Data.URM.RegIndex as URM.RegIndex

  -- | instruction data type
  --
  -- ZERO i : Zero i : Replace the value in register Ri by 0
  --
  -- SUCC i : Add 1 to the value stored in Ri
  --
  -- COPY i j : Copy the value of register Rj in register Ri
  -- leaving the one in Ri as it was)
  --
  -- JMP i j k : If the values of registers Ri and Rj are equal, then go
  -- to instruction number k, otherwise go to the next instruction
  data Instruction = Zero URM.RegIndex.RegIndex                           -- ^ ZERO instruction
                   | Succ URM.RegIndex.RegIndex                           -- ^ SUCC instruction
                   | Copy URM.RegIndex.RegIndex URM.RegIndex.RegIndex     -- ^ COPY instruction
                   | Jmp  URM.RegIndex.RegIndex URM.RegIndex.RegIndex Int -- ^ JMP  instruction
                   deriving (Show, Eq)

  -- | indexed instruction type definition
  type IInstruction = (Int, Instruction)

  -- | Make a ZERO instruction with non-negative register index.
  mkZero :: URM.RegIndex.RegIndex -> Maybe Instruction
  mkZero i
    | i < 0     = Nothing
    | otherwise = Just $ Zero i

  -- | Make a SUCC instruction with non-negative register index.
  mkSucc :: URM.RegIndex.RegIndex -> Maybe Instruction
  mkSucc i
    | i < 0     = Nothing
    | otherwise = Just $ Succ i

  -- | Make a COPY instruction with non-negative register indices.
  mkCopy :: URM.RegIndex.RegIndex -> URM.RegIndex.RegIndex -> Maybe Instruction
  mkCopy i j
    | i < 0 || j < 0 = Nothing
    | otherwise      = Just $ Copy i j

  -- | Make a JUMP instruction with non-negative register indices and
  -- non-negative intruction pointer.
  mkJmp :: URM.RegIndex.RegIndex -> URM.RegIndex.RegIndex -> Int -> Maybe Instruction
  mkJmp i j l
    | i < 0 || j < 0 || l < 0 = Nothing
    | otherwise               = Just $ Jmp i j l

module Data.URM.StateView
(
  StateView(..)

  -- * Constructing
, mk
, mk'
)
where

  import qualified Data.URM.Instruction as URM.Instruction
  import qualified Data.URM.Reg         as URM.Reg

  -- | The state view data type
  data StateView = StateView { regs         :: [(Int, URM.Reg.Reg)]
                             , iInstruction :: Maybe URM.Instruction.IInstruction
                             } deriving (Show, Eq)

  mk :: [(Int, URM.Reg.Reg)] -> Maybe URM.Instruction.IInstruction -> StateView
  mk rs c = StateView { regs = rs, iInstruction = c }

  mk' :: [(Int, URM.Reg.Reg)] -> URM.Instruction.IInstruction -> StateView
  mk' rs c = StateView { regs = rs, iInstruction = Just c }

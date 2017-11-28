module Data.URM.State
(
  State

, iPtrForward
, iPtrSet
, iPtr

, getReg
, updateReg

, instruction

, stateView
)
where

  import qualified Data.List       as L
  import qualified Data.Tuple      as T
  import qualified Data.Map.Strict as M

  import Data.URM.RegIndex
  import qualified Data.URM.Instruction  as URM.Instruction
  import qualified Data.URM.Reg          as URM.Reg
  import qualified Data.URM.Prog         as URM.Prog
  import qualified Data.URM.StateView    as URM.StatView
  import qualified Data.URM.Utils.Zipper as URM.Zipper

  data State = State { zipper :: URM.Zipper.Zipper URM.Instruction.IInstruction
                     , iRegs   :: M.Map RegIndex URM.Reg.Reg
                     } deriving (Show, Eq)

  {-|
  -}
  mk :: [URM.Prog.Prog] -> [(Int, URM.Reg.Reg)] -> State
  mk is irs = State { zipper = URM.Zipper.mk (L.zip [0..] is)
                    , iRegs  = M.fromList irs }

  {-|
    The 'iPtrForward' function moves forward the instruction program pointer
    and return a new state.
    It returns Nothing in case the instruction pointer program is on a
    non-valid instruction.
  -}
  iPtrForward :: State -> Maybe State
  iPtrForward s = Zipper.stepForward (zipper s) >>= go
    where
      go z = Just $ s { zipper = z }

  {-|
    The 'iPtrSet' function moves the intruction program pointer.
    It returns Nothing in case the instruction pointer program is on an
    invalid instruction.
  -}
  iPtrSet :: Int -> State -> Maybe State
  iPtrSet l s = iPtr s                                                    >>=
                (\ ptr -> if l > ptr then Just (l-ptr) else Just (ptr-l)) >>=
                flip Zipper.move (zipper s)                               >>=
                go
    where
      go z  = Just $ s { zipper = z }

  -- The 'iPtr' function returns the current intruction pointer.
  -- It returns Nothing in case the instruction pointer is on an
  -- invalid instruction.
  iPtr :: State -> Maybe Int
  iPtr s = URM.Zipper.focus (zipper s) >>= (Just . T.fst)

  -- Return the current instruction.
  instruction :: State -> Maybe URM.Instruction.Intruction
  instruction s = URM.Zipper.focus (zipper s) >>= (Just . T.snd)

  -- Return the register at index i together with a new state.
  -- It register at index i does not exist, create a zero value register.
  getReg :: Int -> State -> (State, Reg.Reg.Reg)
  getReg i s = aux M.lookup i (iRegs s)
    where
      aux Nothing = (s { iRegs = M.insert i r }, r)
        where
          r = URM.Reg.mk
      aux (Just r) = (s, r)

  -- Update the register at index i and return the new state.
  updateReg :: Int -> URM.Reg.Reg -> State -> State
  updateReg i r s = s { iRegs = rs }
    where
      rs = M.insert i r (iRegs s)

  -- return the registers of the URM as a sorted (by index) assoc list.
  regsList :: State -> [(Int, URM.Reg.Reg)]
  regsList = L.sortBy (O.comparing T.fst) . M.assocs . iRegs

  stateView :: State -> URM.StateView.StatView
  stateView s = URM.StateView.mk rs ic
    where
      rs = regsList s
      ic = Zipper.focus (zipper state)

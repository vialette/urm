module Data.URM.RunTime
(
  -- * constructing
-- , mk

  -- * querying

  -- * running
, run
-- , trace
)
where

  import qualified Data.List       as L
  import qualified Data.Tuple      as T
  import qualified Data.Ord        as O
  import qualified Data.Map.Strict as M

  import Data.URM.RegIndex
  import qualified Data.URM.Instruction  as URM.Instruction
  import qualified Data.URM.Reg          as URM.Reg
  import qualified Data.URM.Prog         as URM.Prog
  import qualified Data.URM.State        as URM.State
  import qualified Data.URM.StateView    as URM.StateView
  import qualified Data.URM.Utils.Zipper as Zipper

  -- Helper function: show any integer with at least 3 digits
  intToString3Digits :: Int -> String
  intToString3Digits n
    | length (show n) == 1 = "00" ++ show n
    | length (show n) == 2 =  "0" ++ show n
    | otherwise            = show n

  -- Zipper Show instance
  zipperToString :: Show a => Zipper.Zipper (Int, a) -> String
  zipperToString z = showBackward      (Zipper.getBackward z) ++
                     showForward  True (Zipper.getForward z)
    where
      -- show backward instructions
      showBackward []             = []
      showBackward ((l, i) : lis) = "    "               ++
                                    intToString3Digits l ++
                                    ": "                 ++
                                    show i               ++
                                    "\n"                 ++
                                    showBackward lis
      -- show forward comands
      showForward _ []       = ""
      showForward b ((l, i) : lis) = prompt               ++
                                     intToString3Digits l ++
                                     ": "                 ++
                                     show i               ++
                                     "\n"                 ++
                                     showForward (b && False) lis
        where
          prompt = if b then ">>> " else "    "


  -- | URM Show instance
  -- instance Show URM where
  --   show urm = zipperToString (zipper urm) ++
  --              "\n"                        ++
  --              "register: "                ++
  --              showRegs (regs urm)
  --     where
  --       showRegs = L.intercalate "," . fmap showIReg . L.sortBy (O.comparing T.fst) . M.assocs
  --         where
  --           showIReg (i, r) = "r" ++ show i ++ "=" ++ show r

  {-|
    The 'mk' function creates an URM from a program and a list of indexed
    registers.
  -}
  -- mk :: URM.Prog.Prog -> URM
  -- mk = URM . L.zip [0..]

  -- Execute a Zero instruction.
  -- It returns @Nothing@ in case of invalid register index or invalid instruction
  -- pointer.
  execZero :: RegIndex -> URM.State.State -> Maybe URM.State.State
  execZero i = URM.State.iPtrForward . URM.State.updateReg i URM.Reg.mk

  -- The 'execSucc' function execute a Succ instruction.
  -- It returns @Nothing@ in case of invalid register index or invalid instruction
  -- pointer.
  execSucc :: RegIndex -> URM.State.State -> Maybe URM.State.State
  execSucc i s = URM.State.iPtrForward . URM.State.updateReg i ri' s'
    where
      (s', ri) = URM.State.getReg i s
      ri'      = URM.Reg.mkVal (1 + URM.Reg.getVal ri)

  -- The 'execCopy' function execute a Copy instruction.
  -- It returns @Nothing@ in case of invalid register index or invalid commmand
  -- pointer.
  execCopy :: RegIndex -> RegIndex -> URM.State.State -> Maybe URM.State.State
  execCopy i j s = iPtrForward . URM.State.updateReg i ri s'
    where
      (s', rj) = URM.State.getReg j s
      ri       = URM.Reg.mkVal (URM.Reg.getVal rj)

  -- The 'execJmp' function execute a Jmp instruction.
  -- It returns @Nothing@ in case of invalid register index or invalid instruction
  -- pointer.
  execJmp :: RegIndex -> RegIndex -> Int -> URM.State.State -> Maybe URM.State.State
  execJmp i j l s
    | URM.Reg.getVal ri == URM.Reg.getVal rj = iPtrSet l s''
    | otherwise                              = iPtrForward s''
    where
      (s',  ri) = URM.State.getReg i s
      (s'', rj) = URM.State.getReg j s

  {-|
    The 'run' function executes an URM program.
    It returns all registers in case of success (if the machine did not encounter
    any error).
    It returns @Nothing@ in case of invalid register index or invalid instruction
    pointer.
  -}
  run :: [URM.Prog.Prog] -> [(Int, URM.Reg.Reg)] -> Maybe URM.StateView.StateView
  run is irs = go (URM.State.mk is irs)
    where
      go s = case URM.State.instruction s of
              Nothing -> Just $ URM.State.stateView s
              Just i  -> exec i
        where
          exec :: URM.Instruction.Instruction -> Maybe URM.StateView.StateView
          exec (URM.Instruction.Zero i)    = execZero i     s >>= go
          exec (URM.Instruction.Succ i)    = execSucc i     s >>= go
          exec (URM.Instruction.Copy i j)  = execCopy i j   s >>= go
          exec (URM.Instruction.Jmp i j l) = execJmp  i j l s >>= go

  -- trace :: URM -> [URM.StateView.StateView]
  -- trace = aux []
  --   where
  --     aux :: [URM.StateView.StateView] -> URM -> [String]
  --     aux svs ts urm = case Zipper.focus (zipper urm) of
  --                        Nothing     -> L.reverse svs'
  --                        Just (_, i) -> exec i
  --       where
  --         svs' = stateView urm : svs
  --
  --         exec :: URM.Instruction.Instruction -> [URM.StateView.StateView]
  --         exec (URM.Instruction.Zero i)    = aux svs' $ execZero i     urm
  --         exec (URM.Instruction.Succ i)    = aux svs' $ execSucc i     urm
  --         exec (URM.Instruction.Copy i j)  = aux svs' $ execCopy i j   urm
  --         exec (URM.Instruction.Jmp i j l) = aux svs' $ execJmp  i j l urm

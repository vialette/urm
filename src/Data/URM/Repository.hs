module Data.URM.Repository
(
  -- Adding two registers
  add2
, add2'
)
where

  import qualified Data.Traversable as Tranversable
  import qualified Data.Maybe       as Maybe

  import qualified Data.URM.Instruction as URM.Instruction
  import qualified Data.URM.Prog        as URM.Prog

  {-|
    Return an URM program that adds registers R1 and R2 and presents the result
    in register R0.
  -}
  add2 :: URM.Prog.Prog
  add2 = Maybe.fromMaybe (error "bad programm") add2'

  add2' :: Maybe URM.Prog.Prog
  add2' =  Tranversable.sequence is
    where
        is = [ URM.Instruction.mkZero 3      --  0: initialisation
             , URM.Instruction.mkZero 0      --  1: add R1 to R3 (use R0 to count)
             , URM.Instruction.mkJmp  0 1 6  --  2: loop
             , URM.Instruction.mkSucc 0      --  3: increment R0
             , URM.Instruction.mkSucc 3      --  4: increment R3
             , URM.Instruction.mkJmp  0 0 2  --  5: end loop
             , URM.Instruction.mkZero 0      --  6: add R2 to R3 (use R0 to count)
             , URM.Instruction.mkJmp  0 2 11 --  7: loop
             , URM.Instruction.mkSucc 0      --  8: increment R0
             , URM.Instruction.mkSucc 3      --  9: increment R3
             , URM.Instruction.mkJmp  0 0 7  -- 10: end loop
             , URM.Instruction.mkCopy 0 3    -- 11: copy R3 to R1
             ]

import qualified Data.URM.Reg         as URM.Reg
import qualified Data.URM.Repository  as URM.Repository
import qualified Data.URM.URM         as URM

main :: IO ()
main =
  let rs  = [URM.Reg.mk, URM.Reg.mkVal 2, URM.Reg.mkVal 3, URM.Reg.mk]
      urm = URM.mk' URM.Repository.add2 rs
  in
    do
      print urm
      print $ URM.trace urm

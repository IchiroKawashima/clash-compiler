module ManyDefined where

import qualified Data.List as List
import           Test.Tasty.Clash
import           Test.Tasty.Clash.NetlistTest

import           Clash.Prelude
import           Clash.Netlist.Types

data MaybeIntBool
  = IntVal Int
  | BoolVal Bool
  | NotAThing

topEntity :: MaybeIntBool -> Int
topEntity m = case m of
  IntVal i  -> i
  BoolVal b -> if b then 1 else 0
  NotAThing -> errorX "NotAThing is not a thing"

assertOneAltPerDefined :: Component -> IO ()
assertOneAltPerDefined c =
  case findCondAssign (declarations c) of
    Just (CondAssignment _ _ _ _ xs)
      | List.length xs == 2 -> return ()
      | otherwise -> error $ "Expected 2 alternatives, found " <> show (List.length xs)

    _ -> error "Expected conditional assignment in netlist"
 where
  findCondAssign = List.find isCondAssign

  isCondAssign CondAssignment{} = True
  isCondAssign _                = False

testPath :: FilePath
testPath = "tests/shouldwork/XOptimization/ManyDefined.hs"

getComponent :: (a, b, c, d) -> d
getComponent (_, _, _, x) = x

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL idirs testPath
  mapM_ (assertOneAltPerDefined . getComponent) netlist
 where
  idirs = ["clash-lib/prims/common", "clash-lib/prims/vhdl"]

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog idirs testPath
  mapM_ (assertOneAltPerDefined . getComponent) netlist
 where
  idirs =
    [ "clash-lib/prims/common"
    , "clash-lib/prims/commonverilog"
    , "clash-lib/prims/verilog"
    ]

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  netlist <- runToNetlistStage SSystemVerilog idirs testPath
  mapM_ (assertOneAltPerDefined . getComponent) netlist
 where
  idirs =
    [ "clash-lib/prims/common"
    , "clash-lib/prims/commonverilog"
    , "clash-lib/prims/systemverilog"
    ]


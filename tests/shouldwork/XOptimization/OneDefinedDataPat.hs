module OneDefinedDataPat where

import Clash.Prelude
import Clash.Netlist.Types

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

topEntity :: Maybe (Unsigned 8) -> Unsigned 8
topEntity m = case m of
    Nothing -> errorX "Nothing: Expected Just"
    Just x  -> x

assertNoMux :: Component -> IO ()
assertNoMux c =
  case go 0 $ declarations c of
    0 -> return ()
    n -> error $ "Expected 0 multiplexers, found " <> show n
 where
  go acc []                    = acc
  go acc (CondAssignment{}:ds) = go (acc + 1) ds
  go acc (_:ds)                = go acc ds

testPath :: FilePath
testPath = "tests/shouldwork/XOptimization/OneDefinedDataPat.hs"

getComponent :: (a, b, c, d) -> d
getComponent (_, _, _, x) = x

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL idirs testPath
  mapM_ (assertNoMux . getComponent) netlist
 where
  idirs = ["clash-lib/prims/common", "clash-lib/prims/vhdl"]

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog idirs testPath
  mapM_ (assertNoMux . getComponent) netlist
 where
  idirs =
    [ "clash-lib/prims/common"
    , "clash-lib/prims/commonverilog"
    , "clash-lib/prims/verilog"
    ]

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  netlist <- runToNetlistStage SSystemVerilog idirs testPath
  mapM_ (assertNoMux . getComponent) netlist
 where
  idirs =
    [ "clash-lib/prims/common"
    , "clash-lib/prims/commonverilog"
    , "clash-lib/prims/systemverilog"
    ]


module Test00 where

import Control.Monad

import Clash.Explicit.Prelude
import Clash.Explicit.SimIO

tbInit = do
  fileIn  <- openFile "./goldenInput00.txt"
  fileOut <- openFile "./goldenOutput00.txt"
  return (fileIn,fileOut)

tbMachine (fileIn,fileOut) regOut = do
  eofIn  <- feof fileIn
  eofOut <- feof fileOut
  let eof = eofIn || eofOut

  when eof $ do
    display "success"
    finish 0

  goldenIn  <- fgetc fileIn;
  goldenOut <- fgetc fileOut;

  res <- if regOut == fromEnum goldenOut then
            return (fromEnum goldenIn)
         else do
            display ("Output doesn't match golden output")
            finish 1

  display ("Output matches golden output")
  return res

topEntity = regOut
 where
  clk = systemClockGen
  rst = resetGen
  ena = enableGen

  regOut = register clk rst ena (fromEnum 'a') regIn
  regIn  = mealyIO clk tbMachine tbInit regOut

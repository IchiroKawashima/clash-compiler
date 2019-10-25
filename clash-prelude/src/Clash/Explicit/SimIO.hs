{-|
  Copyright   :  (C) 2019, Google Inc
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE BangPatterns, MagicHash, TypeOperators, ScopedTypeVariables, FlexibleContexts #-}

module Clash.Explicit.SimIO
  ( -- * I\/O environment for simulation
    mealyIO
  , SimIO
  -- * Display on stdout
  , display
  -- * End of simulation
  , finish
  -- * Mutable values
  , Reg
  , reg
  , readReg
  , writeReg
  -- * File I\/O
  , File
  , openFile
  , fgetc
  , feof
  )
where

import Data.Coerce
import Data.IORef
import qualified System.IO as IO
import System.IO.Unsafe

import Clash.Signal.Internal
import Clash.XException (seqX)

-- | Simulation-level I\/O environment; synthesizable to HDL I\/O, which in
-- itself is unlikely to be synthesisable to a digital circuit.
--
-- See 'mealyIO' as to its use.
newtype SimIO a = SimIO {unSimIO :: IO a}

instance Functor SimIO where
  fmap = fmapSimIO#

fmapSimIO# :: (a -> b) -> SimIO a -> SimIO b
fmapSimIO# f (SimIO m) = SimIO (fmap f m)
{-# NOINLINE fmapSimIO# #-}

instance Applicative SimIO where
  pure  = pureSimIO#
  (<*>) = apSimIO#

pureSimIO# :: a -> SimIO a
pureSimIO# a = SimIO (pure a)
{-# NOINLINE pureSimIO# #-}

apSimIO# :: SimIO (a -> b) -> SimIO a -> SimIO b
apSimIO# (SimIO f) (SimIO m) = SimIO (f <*> m)
{-# NOINLINE apSimIO# #-}

instance Monad SimIO where
  return = pureSimIO#
  (>>=)  = bindSimIO#

bindSimIO# :: SimIO a -> (a -> SimIO b) -> SimIO b
bindSimIO# (SimIO m) k = SimIO (m >>= (\x -> x `seqX` coerce k x))
{-# NOINLINE bindSimIO# #-}

-- | Display a string on /stdout/
display
  :: String
  -- ^ String you want to display
  -> SimIO ()
display s = SimIO (putStrLn s)
{-# NOINLINE display #-}

-- | Finish the simulation with an exit code
finish
  :: Integer
  -- ^ The exit code you want to return at the end of the simulation
  -> SimIO a
finish i = return (error (show i))
{-# NOINLINE finish #-}

-- | Mutable reference
newtype Reg a = Reg (IORef a)

-- | Create a new mutable reference with the given starting value
reg
  :: a
  -- ^ The starting value
  -> SimIO (Reg a)
reg a = SimIO (Reg <$> newIORef a)
{-# NOINLINE reg #-}

-- | Read value from a mutable reference
readReg :: Reg a -> SimIO a
readReg (Reg a) = SimIO (readIORef a)
{-# NOINLINE readReg #-}

-- | Write new value to the mutable reference
writeReg
  :: Reg a
  -- ^ The mutable reference
  -> a
  -- ^ The new value
  -> SimIO ()
writeReg (Reg r) a = SimIO (writeIORef r a)
{-# NOINLINE writeReg #-}

-- | File handle
newtype File = File IO.Handle

-- | Open a file
openFile
  :: FilePath
  -- ^ File to open
  -> SimIO File
openFile fp = coerce (IO.openFile fp IO.ReadWriteMode)
{-# NOINLINE openFile #-}

-- | Read one character from a file
fgetc
  :: File
  -- ^ File to read from
  -> SimIO Char
fgetc (File fp) = SimIO (IO.hGetChar fp)
{-# NOINLINE fgetc #-}

-- |  Determine whether we've reached the end of the file
feof
  :: File
  -- ^ File we want to inspect
  -> SimIO Bool
feof (File fp) = SimIO (IO.hIsEOF fp)
{-# NOINLINE feof #-}

-- | Simulation-level I/O environment that can be synthesized to HDL-level I\/O.
-- Note that it is unlikely that the HDL-level I\/O can subsequently be
-- synthesized to a circuit.
--
-- = Example
--
-- @
-- tbMachine (fileIn,fileOut) regOut = do
--   eofFileOut <- 'feof' fileOut
--   eofFileIn  <- 'feof' fileIn
--   when (eofFileIn || eofFileOut) $ do
--     { 'display' "success"
--     ; 'finish' 0
--     }
--
--   goldenIn  <- 'fgetc' fileIn
--   goldenOut <- 'fgetc' fileOut
--   res <- if regOut == fromEnum goldenOut then do
--            return (fromEnum goldenIn)
--          else do
--            'display' ("Output doesn't match golden output")
--            'finish' 1
--   display ("Output matches golden output")
--   return res
--
-- tbInit = do
--   fileIn  <- 'openFile' "./goldenInput00.txt"
--   fileOut <- 'openFile' "./goldenOutput00.txt"
--   return (fileIn,fileOut)
--
-- topEntity = regOut
--   where
--     clk = systemClockGen
--     rst = resetGen
--     ena = enableGen
--
--     regOut = register clk rst ena (fromEnum 'a') regIn
--     regIn  = 'mealyIO' clk tbMachine tbInit regOut
-- @
mealyIO
  :: KnownDomain dom
  => Clock dom
  -- ^ Clock at which rate the I\/O environment progresses
  -> (s -> i -> SimIO o)
  -- ^ Transition function inside an I\/O environment
  -> SimIO s
  -- ^ I/O action to create the initial state
  -> Signal dom i
  -> Signal dom o
mealyIO !_ f (SimIO i) inp = unsafePerformIO (i >>= go inp)
 where
  go q@(~(k :- ks)) s =
    (:-) <$> unSimIO (f s k) <*> unsafeInterleaveIO ((q `seq` go ks s))
{-# NOINLINE mealyIO #-}

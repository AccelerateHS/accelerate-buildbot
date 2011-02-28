{-# LANGUAGE PatternGuards #-}
-- |
-- Module      : Main
-- Copyright   : [2011] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Main where

import Build
import Config
import BuildBox

import System.Random
import System.Directory
import Control.Monad.Error.Class


-- The main buildbot. Process command line arguments and either run the test
-- once now, or initiate a cron loop to run every day at the specified time.
--
main :: IO ()
main = do
  uid      <- getStdRandom $ randomR (0,1000000)
  tmp      <- getTemporaryDirectory
  (cfg,st) <- processArgs (buildStateDefault uid tmp)

  -- Run the build-bot
  let once = runAccBuildTest cfg
      loop | Just at <- configSchedule cfg = cronLoop $ makeSchedule [("buildbot-daily", Daily at, Nothing, once)]
           | otherwise                     = once

  successfully (runBuildPrintWithState st loop)


-- Runs the build/test cycle and sends an email on failure
--
runAccBuildTest :: Config -> Build ()
runAccBuildTest config = do
  outLINE
  outLn "* Starting Build..."
  outLn . ("  - current time is " ++) . show =<< io getZonedTime
  outBlank

  runBuildTest config `catchError` handleBuildError config

  outLn "* Done"
  outLINE


-- Run the complete fetch/build/test cycle once
--
runBuildTest :: Config -> Build ()
runBuildTest config =
  inScratchDir (configScratchDir config) $ do
    -- Check the current environment
    env <- getEnvironmentWith
             [ ("GHC", getVersionGHC $ configWithGHC config)
             , ("GCC", getVersionGCC "gcc") ]
    outLn . render . ppr $ env
    outBlank

    -- Get and build the accelerate library. Sets the current directory to the
    -- newly checked-out repository.
    fetchAcc config
    buildAcc config

    -- Build and run the example programs
    buildTest config
    runTest   config env
    postTest  config


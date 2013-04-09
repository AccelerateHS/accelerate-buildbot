{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Benchmarks
-- Copyright   : [2011] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module Benchmarks (runAccBenchmarks) where

import Util
import Config
import BuildBox

import Control.Applicative
import Data.Char
import Data.Maybe
import Text.CSV.ByteString
import System.FilePath
import qualified Data.Sequence              as Seq
import qualified BuildBox.Data.Log          as Log
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lex.Double as B



-- The list of benchmarks. In most cases it is expected that the program write
-- timing information from criterion (or similar) to the given file. It should
-- also be linked with RTS options to query heap usage.
--
benchmarks :: Config -> [FilePath -> Benchmark]
benchmarks cfg =
  let testcase name args = bench name "accelerate-examples" (name:"--cuda":args)
      bench name exe args summaryLog =
       Benchmark
        { benchmarkName    = name
        , benchmarkSetup   = return ()
        , benchmarkCommand = runAcc summaryLog ("accelerate-examples/dist/build" </> exe </> exe) args
        , benchmarkCheck   = return [] }
      --
      lena_bw = "accelerate-examples/data/images/lena512.pgm"
  in
    [
      -- We run all the accelerate-examples tests separately so we can get individual quirks.
      testcase "map-abs" []
    , testcase "map-plus" []
    , testcase "map-square" []
    , testcase "zip" []
    , testcase "zipWith-plus" []
    , testcase "fold-sum" []
    , testcase "fold-product" []
    , testcase "fold-maximum" []
    , testcase "fold-minimum" []
    , testcase "fold-2d-sum" []
    , testcase "fold-2d-product" []
    , testcase "fold-2d-maximum" []
    , testcase "fold-2d-minimum" []
    , testcase "foldseg-sum" []
    , testcase "scanseg-sum" []
    , testcase "stencil-1D" []
    , testcase "stencil-2D" []
    , testcase "stencil-3D" []
    , testcase "stencil-3x3-cross" []
    , testcase "stencil-3x3-pair" []
    , testcase "stencil2-2D" []
    , testcase "permute-hist" []
    , testcase "backpermute-reverse" []
    , testcase "backpermute-transpose" []
    , testcase "init" []
    , testcase "tail" []
    , testcase "take" []
    , testcase "drop" []
    , testcase "slit" []
    , testcase "gather" []
    , testcase "gather-if" []
    , testcase "scatter" []
    , testcase "scatter-if" []
    , testcase "sasum" []
    , testcase "saxpy" []
    , testcase "dotp" []
    , testcase "filter" []
    , testcase "smvm" ["-m", "accelerate-examples/data/matrices/random.mtx"]
    , testcase "black-scholes" []
    , testcase "radixsort" []
    , testcase "io" []
    , testcase "canny" ["-i", lena_bw]
    , testcase "integral-image" ["-i", lena_bw]
    , testcase "slices" []
    , testcase "sharing-recovery" []
    , testcase "bound-variables" []
    , bench "crystal"    "accelerate-crystal"    ["--benchmark", "--cuda"]
    , bench "fluid"      "accelerate-fluid"      ["--benchmark", "--cuda"]
    , bench "mandelbrot" "accelerate-mandelbrot" ["--benchmark", "--cuda"]
    , bench "nbody"      "accelerate-nbody"      ["--benchmark", "--cuda"]
    , bench "smoothlife" "accelerate-smoothlife" ["--benchmark", "--cuda"]
    ]


-- | Execute all benchmarks and return statistics for each.
--
runAccBenchmarks :: Config -> Build [BenchResult Stats]
runAccBenchmarks config = concat <$> mapM run (benchmarks config)
  where
    run  x = withTempFile $ \f -> do
      let x' = x f
      r <- statBenchResult . BenchResult (benchmarkName x') . unit <$> runBenchmarkOnce 1 x'
      c <- readCriterionStats f
      case c of
        [c'] -> return [r { benchResultRuns = benchResultRuns r ++ benchResultRuns c' }]
        _    -> return $ r : c


-- Read criterion statistics. The file contains the following header, which we
-- ignore, and corresponding format:
--
--   "Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB"
--
readCriterionStats :: FilePath -> Build [BenchResult Stats]
readCriterionStats f = do
  contents <- io $ B.readFile f
  case parseCSV contents of
    Nothing  -> if B.null contents then return [] else throw  $ ErrorOther "failed to parse criterion results"
    Just csv -> return $ map parse (tail csv)

  where
    seconds         = Seconds . fst . fromJust . B.readDouble
    stats avg lb ub = WithSeconds (Time KernelWall Stats { statsMin = seconds lb
                                                         , statsMax = seconds ub
                                                         , statsAvg = seconds avg })

    parse (n:avg:lb:ub:_) = BenchResult (B.unpack n) [BenchRunResult 0 [] [ stats avg lb ub ]]
    parse _               = error "we should not be here..."


-- Run a standard accelerate-examples benchmark. The executable is expected to
-- write kernel statistics to the given file, which will be read later.
--
-- TLM: return the wall time for the entire program? Includes random number
--      generation and the criterion machinery, but also important Accelerate
--      operations such as sharing recovery and code generation.
--
runAcc :: FilePath -> FilePath -> [String] -> Build [WithUnits (Aspect Single)]
runAcc tlog exe args = do
  let cmd = unwords [exe, "--summary=" ++ tlog, unwords args, "+RTS -K16M -t"]
  (status, logOut, logErr) <- systemTeeLog False cmd Log.empty
  case status of
       ExitSuccess -> return $ parseGCLine logErr
       _           -> throw  $ ErrorSystemCmdFailed cmd status logOut logErr


-- Parse one-line GC summary statistics and return heap allocation and maximum
-- residency aspects.
--
-- Example (from ghc-7.0.1):
--
-- <<ghc: 3074525896 bytes, 4949 GCs, 2674563/6977652 avg/max bytes residency (8 samples), ...
--     15M in use, 0.01 INIT (0.00 elapsed), 5.56 MUT (6.61 elapsed), 0.19 GC (0.21 elapsed) :ghc>>
--
parseGCLine :: Log.Log -> [WithUnits (Aspect Single)]
parseGCLine gc =
  case index of
    Nothing -> []
    Just i  ->
      let toks = B.words $ Seq.index gc i
      in
      [ Used HeapAlloc `bytes` read' (toks !! 1)
      , Used HeapMax   `bytes` read' (snd . B.spanEnd isDigit $ toks !! 5) ]

  where
    read' = fst . fromJust . B.readInteger
    index = Seq.findIndexR (\l -> B.isPrefixOf "<<ghc:" l && B.isSuffixOf ":ghc>>" l) gc
      -- TLM: could reasonably assume that this is the last line of the log,
      --      since it is printed by the RTS after program termination.


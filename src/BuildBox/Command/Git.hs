-- |
-- Module      : BuildBox.Command.Git
-- Copyright   : [2013] The Accelerate Team
-- License     : BSD3
--
-- Maintainer  : Robert Clifton-Everest <robertce@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-partable (GHC extensions)
--

module BuildBox.Command.Git where

import Data.Maybe
import qualified Data.Sequence         as Seq
import qualified Data.ByteString.Char8 as B
import System.Locale

import BuildBox
import BuildBox.Data.Log               as Log

type GitPath      = String

data GitCommit = GitCommit
  {
    timestamp :: LocalTime,
    author    :: EmailAddress,
    comment   :: Log
  }

changesN :: Maybe GitPath -> Int -> Build [GitCommit]
changesN repo n = git $ "git log --format=\"format:%ae %ct%n%b%n----\" -n" ++ show n ++ " " ++ fromMaybe "." repo

git :: String -> Build [GitCommit]
git cmd = do
  (status, logOut, logErr) <- systemTeeLog False cmd Log.empty
  case status of
    ExitSuccess -> return $ splitPatches logOut
    _           -> throw  $ ErrorSystemCmdFailed cmd status logOut logErr

splitPatches :: Log.Log -> [GitCommit]
splitPatches l
  | Seq.null l = []
  | otherwise  = let (h,t) = Seq.breakl (==separator) l
                 in  patch h : splitPatches (Seq.dropWhileL B.null t)
  where
    separator = B.pack "----" -- The separator we use
    patch p   =
      let toks          = words . B.unpack $ Seq.index p 0
          (t,a) = splitAt 6 toks
      in
      GitCommit
        {
          timestamp = readTime defaultTimeLocale "%s" (unwords t)
        , author    = unwords a
        , comment   = Seq.drop 1 p
        }


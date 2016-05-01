{-# LANGUAGE QuasiQuotes #-}

module Main (
  main
) where

import Data.Maybe (fromJust)
import Data.Version (showVersion)
import Paths_check_solr (version) -- from cabal
import System.Environment (getArgs)
import Text.RawString.QQ (r)
import qualified System.Console.Docopt.NoTH as O

import CheckSolr.Thresholds (Thresholds(..))
import qualified CheckSolr.Stats as Stats

usage :: String
usage =  "check_solr_stats " ++ showVersion version
  ++ [r|

Usage:
  check_solr_stats [options]

Options:
  -u, --uri=URI           Solr URI [default: http://localhost:8983/solr/CORE/]

  -g, --category=CAT      Category name [default: CORE]
  -k, --key=KEY           Object key [default: searcher]
  -s, --stat=STAT         Parameter to check [default: numDocs]

  -w, --low-warn=LOWW     Warning threshold for low value
  -c, --low-crit=LOWC     Critical threshold for low value

  -W, --high-warn=HIGHW   Warning threshold for high value
  -C, --high-crit=HIGHC   Critical threshold for high value

  -h, --help              Show this message

|]

main :: IO()
main = do
  doco <- O.parseUsageOrExit usage
  args <- O.parseArgsOrExit doco =<< getArgs
  if args `O.isPresent` O.longOption "help"
  then putStrLn $ O.usage doco
  else do
    let
      getString n = O.getArg args (O.longOption n)
      getDouble n = read <$> getString n
      uri = fromJust $ getString "uri"
      category = fromJust $ getString "category"
      key = fromJust $ getString "key"
      stat = fromJust $ getString "stat"
      thresholds = Thresholds
              (getDouble "low-crit") (getDouble "low-warn")
              (getDouble "high-warn") (getDouble "high-crit")
    Stats.run uri category key stat thresholds


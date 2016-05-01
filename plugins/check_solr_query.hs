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
import qualified CheckSolr.Query as Query

usage :: String
usage =  "check_solr_query " ++ showVersion version
  ++ [r|

Usage:
  check_solr_query [options]

Options:
  -u, --uri=URI           Solr URI [default: http://localhost:8983/solr/CORE/]

  -q, --query=QUERY       Solr query [default: *:*]
  -w, --warn=WARN         Warning threshold for low number of results
  -c, --crit=CRIT         Critical threshold for low number of results

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
      query = fromJust $ getString "query"
      thresholds = Thresholds
              (getDouble "crit") (getDouble "warn")
              Nothing Nothing
    Query.run uri query thresholds


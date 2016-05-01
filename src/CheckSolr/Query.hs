{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module CheckSolr.Query (
  run
) where

import Control.Exception (handle, displayException, SomeException)
import System.Nagios.Plugin (runNagiosPlugin, NagiosPlugin, CheckStatus(Critical), addResult, addPerfDatum, PerfValue(IntegralValue), UOM(Millisecond, Counter))
import qualified Data.Text as T

import CheckSolr.Solr.Select (SelectResponse(responseHeader, response), select, Response(numFound))
import CheckSolr.Solr.ResponseHeader (ResponseHeader(qTime))
import CheckSolr.Thresholds (Thresholds(..))
import qualified CheckSolr.Thresholds as Thr

run :: String -> String -> Thresholds -> IO ()
run uri query thr = plugin >>= runNagiosPlugin
  where
    critical :: SomeException -> IO (NagiosPlugin ())
    critical = return . addResult Critical . T.pack . displayException

    plugin :: IO (NagiosPlugin ())
    plugin = handle critical $ do
      sr <- select uri query
      let
        nf = numFound $ response sr
        (warn, crit) = (\ (Thresholds c w _ _)
                        -> ( IntegralValue . truncate <$> w,
                             IntegralValue . truncate <$> c) ) thr
      return $ do
        addResult (Thr.status thr $ fromIntegral nf) $
                  T.concat [ "numFound = ", T.pack . show $ nf, ", query: ", T.pack query ]
        addPerfDatum
          "numFound" (IntegralValue . fromIntegral $ nf)
          Counter (Just . IntegralValue $ 0) Nothing
          warn crit
        addPerfDatum
          "QTime"
          (IntegralValue . fromIntegral . qTime . responseHeader $ sr)
          Millisecond (Just . IntegralValue $ 0)
          Nothing Nothing Nothing


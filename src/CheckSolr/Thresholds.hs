module CheckSolr.Thresholds (
  Thresholds(..),
  status
) where

import System.Nagios.Plugin (CheckStatus(OK, Warning, Critical))

data Thresholds = Thresholds
  (Maybe Double)
  (Maybe Double)
  (Maybe Double)
  (Maybe Double)
  deriving (Show)

status :: Thresholds -> Double -> CheckStatus
status (Thresholds lc lw hw hc) val
  | val `out` (lc, hc)  = Critical
  | val `out` (lw, hw)  = Warning
  | otherwise = OK
  where
    out v (l, h) =  maybe False (\ t -> v < t) l
              || maybe False (\ t -> v > t) h


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module CheckSolr.Stats (
  run
) where

import Control.Arrow ((&&&))
import Control.Exception (throw, handle, catch, displayException,
  Exception, SomeException, toException)
import Data.Aeson (Value(Object, Number))
import Data.HashMap.Strict (HashMap)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Typeable (Typeable)
import System.Nagios.Plugin (runNagiosPlugin, NagiosPlugin,
  CheckStatus(Critical, Warning), addPerfData, addResult,
  PerfDatum(..), PerfValue(RealValue, IntegralValue), UOM(Megabyte,
  Percent, Millisecond, NullUnit), ToPerfData, toPerfData, addPerfData)
import Text.Regex.TDFA ((=~))
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T

import CheckSolr.Solr.MBeans (MBeansResponse(responseHeader, solrMBeans))
import CheckSolr.Solr.ResponseHeader (ResponseHeader(qTime))
import CheckSolr.Thresholds (Thresholds(..))
import qualified CheckSolr.Solr.MBeans as MBeans
import qualified CheckSolr.Thresholds as Thr

run :: String -> String -> String -> String -> Thresholds -> IO ()
run uri cat key p thr = (plugin >>= runNagiosPlugin) `catch` checkSolrStatError
  where
    critical :: SomeException -> IO (NagiosPlugin ())
    critical = return . addResult Critical . T.pack . displayException

    checkSolrStatError :: CheckSolrStatError -> IO ()
    checkSolrStatError e = critical (toException e) >>= runNagiosPlugin

    plugin :: IO (NagiosPlugin ())
    plugin = handle critical $ do
      mb <- get uri cat key
      let
        pt = T.pack p
        info = T.concat [ T.pack $ " for " ++ cat ++ ":" ++ key, " [ ", description mb, " ]" ]
        res = maybe
          (Warning, T.concat ["Not found: ", pt])
          (Thr.status thr &&& printValue pt)
          (HMap.lookup pt $ stats mb)
      return $ do
        addResult (fst res) $ T.concat [ snd res, info ]
        addPerfData mb

    printValue :: Text -> Double -> Text
    printValue k v = T.concat [ k, " = ", T.pack . show $ _value pd, T.pack . show $ _uom pd ]
      where pd = perfDatum (k, v)

perfDatum :: (Text, Double) -> PerfDatum
perfDatum (k, v) = PerfDatum {
    _label = k,
    _value = value v,
    _uom = unit,
    _min = Just (value 0.0),
    _max = top,
    _warn = Nothing,
    _crit = Nothing
  } where
    k' = T.unpack k
    isCounter = k' =~ (".*(handlerStart|indexVersion|hits|lookups|evictions|inserts|requests|timeouts|errors|Docs?)$" :: String) :: Bool
    isRatio = k' =~ (".*ratio$" :: String) :: Bool
    isSize = k' =~ ("^size$" :: String) :: Bool
    isTime = k' =~ (".*(Time|TimePer.*)$" :: String) :: Bool
    value v'
      | isCounter || isTime = IntegralValue (truncate v')
      | isRatio = IntegralValue (truncate (100 * v'))
      | otherwise = RealValue v'
    unit
      | isRatio = Percent
      | isSize = Megabyte
      | isTime = Millisecond
      | otherwise = NullUnit
    top
      | isRatio = Just $ IntegralValue 100
      | otherwise = Nothing

data MBean = MBean {
  description :: Text,
  stats :: HashMap Text Double
} deriving (Show)

instance ToPerfData MBean where
  toPerfData mb = map perfDatum . HMap.toList . stats $ mb

get :: String -> String -> String -> IO MBean
get u c k = extractMetrics (T.pack k) <$> MBeans.get u c k

extractMetrics :: Text -> MBeansResponse -> MBean
extractMetrics k mbeansResp = MBean {description = d, stats = p}
  where
    mbean = fromMaybe (throw . CheckSolrStatBadResponse $ show mbeansResp)
          . find isObject $ solrMBeans mbeansResp
    st = lookupField "stats" $ lookupField k mbean
    d = (\ (JSON.String s) -> s) $ lookupField "description" $ lookupField k mbean
    p = HMap.insert "QTime" (fromIntegral . qTime . responseHeader $ mbeansResp)
      $ getNumbers st

getNumbers :: Value -> HashMap Text Double
getNumbers (Object h) =
  HMap.map (\ (Number n) -> toRealFloat n) . HMap.filter isNumber $ h
getNumbers _ = undefined

lookupField :: Text -> Value -> Value
lookupField f (JSON.Object h) =
    fromMaybe (throw . CheckSolrStatNotFound $ f)
  $ HMap.lookup f h
lookupField _ _ = undefined

isObject :: Value -> Bool
isObject (Object _) = True
isObject _ = False

isNumber :: Value -> Bool
isNumber (Number _) = True
isNumber _ = False

data CheckSolrStatError
  = CheckSolrStatBadResponse String
  | CheckSolrStatNotFound Text
  deriving (Show, Typeable)
instance Exception CheckSolrStatError


{- XXX For references:

# curl -s 'http://solr.example.net/solr/SOMECORE/admin/mbeans?stats=true&wt=json&cat=CORE&key=searcher' | jq .
{
  "responseHeader": {
    "status": 0,
    "QTime": 0
  },
  "solr-mbeans": [
    "CORE",
    {
      "searcher": {
        "class": "org.apache.solr.search.SolrIndexSearcher",
        "version": "1.0",
        "description": "index searcher",
        "src": null,
        "stats": {
          "searcherName": "Searcher@64846689[SOMECORE] main",
          "caching": true,
          "numDocs": 90754,
          "maxDoc": 90754,
          "deletedDocs": 0,
          "reader": "StandardDirectoryReader(segments_ewy:75493:nrt _ott(4.10.3):C90754)",
          "readerDir": "some stuff",
          "indexVersion": 75493,
          "openedAt": "2016-04-26T05:42:53.969Z",
          "registeredAt": "2016-04-26T05:42:53.97Z",
          "warmupTime": 0
        }
      }
    }
  ]
}

-}


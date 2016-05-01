{-# LANGUAGE OverloadedStrings #-}

module CheckSolr.Solr.MBeans (
  MBeansResponse(..)
, get
) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, parseJSON, Value(Object), (.:))
import Data.Maybe (fromMaybe)
import qualified Data.Aeson as JSON
import qualified Network.HTTP.Conduit as HTTP

import CheckSolr.Solr.ResponseHeader (ResponseHeader(..))

data MBeansResponse = MBeansResponse {
  responseHeader :: ResponseHeader,
  solrMBeans :: JSON.Array
} deriving (Show)

instance FromJSON MBeansResponse where
  parseJSON (Object m) = MBeansResponse
    <$> m .: "responseHeader"
    <*> m .: "solr-mbeans"
  parseJSON _ = empty

get :: String -> String -> String -> IO MBeansResponse
get uri category key = do
  req      <- HTTP.parseUrl $ uri
           ++ "admin/mbeans?stats=true&wt=json"
           ++ "&cat=" ++ category ++ "&key=" ++ key
  manager  <- HTTP.newManager HTTP.tlsManagerSettings
  body     <- HTTP.responseBody <$> HTTP.httpLbs req manager
  return . fromMaybe (error $ "couldn't parse " ++ show body)
         . JSON.decode $ body


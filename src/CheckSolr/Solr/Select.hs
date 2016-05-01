{-# LANGUAGE OverloadedStrings #-}

module CheckSolr.Solr.Select (
  SelectResponse(..)
, Response(..)
, select
) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, parseJSON, Value(Object), (.:))
import Data.Maybe (fromMaybe)
import Network.HTTP.Base (urlEncodeVars)
import qualified Data.Aeson as JSON
import qualified Network.HTTP.Conduit as HTTP

import CheckSolr.Solr.ResponseHeader (ResponseHeader(..))

select :: String -> String -> IO SelectResponse
select uri query = do
  req      <- HTTP.parseUrl $ uri ++ "select?"
           ++ urlEncodeVars [
                ("q", query)
              , ("rows", "0")
             ]
  manager  <- HTTP.newManager HTTP.tlsManagerSettings
  body     <- HTTP.responseBody <$> HTTP.httpLbs req manager
  return . fromMaybe (error $ "couldn't parse " ++ show body)
         . JSON.decode $ body

data Response = Response {
  numFound :: Int,
  start :: Int,
  docs :: JSON.Array
} deriving (Show)
instance FromJSON Response where
  parseJSON (Object m) = Response
    <$> m .: "numFound"
    <*> m .: "start"
    <*> m .: "docs"
  parseJSON _ = empty

data SelectResponse = SelectResponse {
  responseHeader :: ResponseHeader,
  response :: Response
} deriving (Show)
instance FromJSON SelectResponse where
  parseJSON (Object m) = SelectResponse
    <$> m .: "responseHeader"
    <*> m .: "response"
  parseJSON _ = empty


{-# LANGUAGE OverloadedStrings #-}

module CheckSolr.Solr.ResponseHeader
(
  ResponseHeader(..)
) where

import Control.Applicative(empty)
import Data.Aeson (FromJSON, parseJSON, Value(Object), (.:))


data ResponseHeader = ResponseHeader {
  status :: Int,
  qTime :: Int
} deriving (Show)

instance FromJSON ResponseHeader where
  parseJSON (Object h) = ResponseHeader
    <$> h .: "status"
    <*> h .: "QTime"
  parseJSON _ = empty


{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module JsonConfig
    ( Config
    , Test 
    )
    where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.=), Object, withObject)
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)
import GHC.Generics

data Config = Config
  { configCode :: Text
  , configClientSecret :: Text
  , configClientId :: Text
  , configUsername :: Text
  } deriving (Show, Eq, Ord, Generic)

newtype Test = Test
    { testaaa :: Int } deriving (Show, Eq, Ord)

instance FromJSON Test where
    parseJSON (Object v) = Test <$> v .: "aaa"
    parseJSON invalid = typeMismatch "Test" invalid

-- instance ToJSON Config where
--   toJSON Config{..} = object
--     [ "code" .= configCode
--     , "client_secret" .= configClientSecret
--     , "client_id" .= configClientId
--     , "username" .= configUsername
--     ] 

instance FromJSON Config where
    parseJSON(Object v) = Config <$> v .: "code" <*> v .: "client_secret" <*> v .: "client_id" <*> v .: "username"
    parseJSON invalid = typeMismatch "Config" invalid
  -- parseJSON (Object v) = do
  --   configCode <- v .: "code"
  --   configClientSecret <- v .: "client_secret"
  --   configClientId <- v .: "client_id"
  --   configUsername <- v .: "username"
  --   pure $ Config{..}
  -- parseJSON invalid = do
  --   prependFailure "parsing Model failed, "
  --     (typeMismatch "Object" invalid)

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module SheetField (SheetField (field_id, name, value), encode, decode, jsonToField) where

import Import
import Data.Aeson
import Data.ByteString as BS
import Data.ByteString.Lazy as BL

data SheetField = SheetField 
  { field_id :: Text
  ,  name :: Text
  ,  value :: Text
  } deriving (Generic, Show)

instance ToJSON SheetField where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON SheetField

jsonToField :: BS.ByteString -> Maybe SheetField
jsonToField bs = decode (BL.fromStrict bs)
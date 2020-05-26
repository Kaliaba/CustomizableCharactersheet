{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Sheet where

import Import
import Data.Map (assocs)
import Data.Aeson           (Value, decode, object, (.=))
import Data.Aeson.Parser    (json)
import Data.ByteString as BS
import Data.ByteString.Lazy as BL

import Handler.Roll
import FieldValueParser as FVParser

data SheetField = SheetField 
  { field_id :: Text
  ,  name :: Text
  ,  value :: Text
  } deriving Show

instance FromJSON SheetField where
  parseJSON (Object v) = SheetField
        <$> v .: "field_id"
        <*> v .: "name"
        <*> v .: "value"
  parseJSON _ = mzero

getSheetR :: Handler Html
getSheetR = do
    sess <- getSession
    defaultLayout $ do 
        $(widgetFile "sheet")

jsonToField :: BS.ByteString -> Maybe SheetField
jsonToField bs = decode (BL.fromStrict bs)






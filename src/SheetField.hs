{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module SheetField where

import Import
import Data.Aeson

data SheetField = SheetField 
  { field_id :: Text
  ,  name :: Text
  ,  value :: Text
  } deriving (Generic, Show)

instance ToJSON SheetField where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON SheetField
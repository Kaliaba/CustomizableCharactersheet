
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Field where

import Import
import System.Random
import Data.Aeson           (Value, encode, object, (.=))
import Data.Aeson.Parser    (json)
import Data.Text

data Options = Options  
    { field_id :: Text
    , name :: Text
    , value :: Text
    }
    deriving Show

instance ToJSON Options where
    toJSON Options {..} = object
        [ "field_id" .= field_id
        , "name" .= name
        , "value" .= value
        ] 

getFieldR :: Handler Html
getFieldR = do
    (widget, enctype) <- generateFormPost fieldForm
    defaultLayout $ do 
        $(widgetFile "field")

postFieldR :: Handler Html
postFieldR = do
    ((result, widget), enctype) <- runFormPost fieldForm
    case result of 
        FormSuccess options -> do
            setSession 
                (field_id options) 
                (toStrict (decodeUtf8 (encode (options))))
            redirect SheetR
        _ -> defaultLayout $ do 
            $(widgetFile "field")

fieldAForm :: AForm Handler Options
fieldAForm= Options
    <$> areq textField "ID"         Nothing
    <*> areq textField "Name"       Nothing
    <*> areq textField "Value"      Nothing

fieldForm :: Html -> MForm Handler (FormResult Options, Widget)
fieldForm = renderTable fieldAForm


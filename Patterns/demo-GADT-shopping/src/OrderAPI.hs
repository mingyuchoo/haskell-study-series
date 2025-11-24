{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module OrderAPI where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson (ToJSON)
import Servant
import Order

-- JSON Response Type
data OrderResponse = OrderResponse { status :: Text
                                   , logs   :: [OrderLog]
                                   } deriving (Generic, Show)

instance ToJSON OrderLog
instance ToJSON OrderResponse
instance ToJSON OrderError

-- REST API Definition
type API = "order" :> Get '[JSON] OrderResponse

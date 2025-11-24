{-# LANGUAGE OverloadedStrings #-}

module Server (server, api)  where

import Servant
import Order
import OrderAPI

api :: Proxy API
api = Proxy

server :: Server API
server = handleOrder

handleOrder :: Handler OrderResponse
handleOrder = do
  case runOrder of
    Left err -> return $ OrderResponse
      { status = "주문 실패"
      , logs = [LogPaymentFailed "결제 실패" | PaymentFailed _ <- [err]]
            ++ [LogShippingFailed "배송 불가" | ShippingFailed _ <- [err]]
      }
    Right (_, logs) -> return $ OrderResponse
      { status = "주문 성공"
      , logs = logs
      }
      

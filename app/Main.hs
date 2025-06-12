{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson
import Network.HTTP.Req
import GHC.Generics
import Text.Printf

-- Data types updated to include all currencies
data PriceResponse = PriceResponse
  { zcash :: ZcashPrice
  } deriving (Show, Generic)

data ZcashPrice = ZcashPrice
  { usd :: Double
  , eur :: Double
  , rub :: Double
  , jpy :: Double
  } deriving (Show, Generic)

instance FromJSON PriceResponse
instance FromJSON ZcashPrice

main :: IO ()
main = do
  putStrLn "Fetching ZEC price..."
  (priceUsd, priceEur, priceRub, priceJpy) <- getZecPrice

  putStrLn $ printf "Current ZEC prices: $%.2f | €%.2f | ₽%.2f | ¥%.2f" priceUsd priceEur priceRub priceJpy

  putStrLn "Pick a currency (1) ZEC (2) USD (3) EUR (4) RUB or (5) JPY ?\nEnter 1, 2, 3, 4 or 5:"
  choice <- getLine
  
  case choice of
    "1" -> do
      putStrLn "Enter amount in ZEC:"
      input <- getLine
      let zecAmount = read input :: Double
      putStrLn $ printf "%.3f ZEC = %.2f USD" zecAmount (zecAmount * priceUsd)
      putStrLn $ printf "%.3f ZEC = %.2f EUR" zecAmount (zecAmount * priceEur)
      putStrLn $ printf "%.3f ZEC = %.2f RUB" zecAmount (zecAmount * priceRub)
      putStrLn $ printf "%.3f ZEC = %.2f JPY" zecAmount (zecAmount * priceJpy)

    "2" -> do
      putStrLn "Enter amount in USD:"
      input <- getLine
      let usdAmount = read input :: Double
          zecAmount = usdAmount / priceUsd
      putStrLn $ printf "%.2f USD = %.3f ZEC" usdAmount zecAmount

    "3" -> do
      putStrLn "Enter amount in EUR:"
      input <- getLine
      let eurAmount = read input :: Double
          zecAmount = eurAmount / priceEur
      putStrLn $ printf "%.2f EUR = %.3f ZEC" eurAmount zecAmount

    "4" -> do
      putStrLn "Enter amount in RUB:"
      input <- getLine
      let rubAmount = read input :: Double
          zecAmount = rubAmount / priceRub
      putStrLn $ printf "%.2f RUB = %.3f ZEC" rubAmount zecAmount

    "5" -> do
      putStrLn "Enter amount in JPY:"
      input <- getLine
      let jpyAmount = read input :: Double
          zecAmount = jpyAmount / priceJpy
      putStrLn $ printf "%.2f JPY = %.3f ZEC" jpyAmount zecAmount

    _ -> putStrLn "Invalid choice."

-- Fetch price from Coingecko
getZecPrice :: IO (Double, Double, Double, Double)
getZecPrice = do
  let url = https "api.coingecko.com" /: "api" /: "v3" /: "simple" /: "price"
      params = "ids" =: ("zcash" :: T.Text) <> "vs_currencies" =: ("usd,eur,rub,jpy" :: T.Text)
  response <- runReq defaultHttpConfig $ req GET url NoReqBody lbsResponse params
  let body = responseBody response
  case eitherDecode body of
    Left err -> error $ "JSON parse error: " ++ err
    Right pr -> do
      let zp = zcash pr
      return (usd zp, eur zp, rub zp, jpy zp)

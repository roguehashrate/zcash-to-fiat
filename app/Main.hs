{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson as Aeson
import Network.HTTP.Req
import GHC.Generics
import Text.Printf
import qualified Options.Applicative as Opt
import Data.Char (toUpper)

-- CLI options data type
data CliOptions = CliOptions
  { interactive :: Bool
  , amount :: Maybe Double
  , fromCurrency :: Maybe String
  , toCurrency :: Maybe String
  } deriving (Show, Generic)

instance Aeson.FromJSON PriceResponse
instance Aeson.FromJSON ZcashPrice

-- Updated data type to include CAD and GBP
data PriceResponse = PriceResponse
  { zcash :: ZcashPrice
  } deriving (Show, Generic)

data ZcashPrice = ZcashPrice
  { usd :: Double
  , eur :: Double
  , rub :: Double
  , jpy :: Double
  , cad :: Double
  , gbp :: Double
  } deriving (Show, Generic)

-- Parser for CLI options
cliOptionsParser :: Opt.Parser CliOptions
cliOptionsParser = CliOptions
  <$> Opt.switch
      ( Opt.long "interactive"
     <> Opt.short 'i'
     <> Opt.help "Run in interactive mode" )
  <*> Opt.optional (Opt.option Opt.auto
      ( Opt.long "amount"
     <> Opt.short 'a'
     <> Opt.metavar "NUMBER"
     <> Opt.help "Amount to convert" ))
  <*> Opt.optional (Opt.strOption
      ( Opt.long "from"
     <> Opt.short 'f'
     <> Opt.metavar "CURRENCY"
     <> Opt.help "Currency to convert from (e.g., ZEC, USD, EUR, CAD, GBP)" ))
  <*> Opt.optional (Opt.strOption
      ( Opt.long "to"
     <> Opt.short 't'
     <> Opt.metavar "CURRENCY"
     <> Opt.help "Currency to convert to (e.g., ZEC, USD, EUR, CAD, GBP)" ))

optsParserInfo :: Opt.ParserInfo CliOptions
optsParserInfo = Opt.info (cliOptionsParser Opt.<**> Opt.helper)
  ( Opt.fullDesc
 <> Opt.header "zcash-to-fiat - a simple ZEC to fiat converter CLI tool" )

main :: IO ()
main = do
  opts <- Opt.execParser optsParserInfo
  putStrLn "Fetching ZEC price..."
  (priceUsd, priceEur, priceRub, priceJpy, priceCad, priceGbp) <- getZecPrice

  if interactive opts
    then interactiveMode (priceUsd, priceEur, priceRub, priceJpy, priceCad, priceGbp)
    else
      case (amount opts, fromCurrency opts, toCurrency opts) of
        (Just amt, Just fromC, Just toC) -> convertAndPrint (amt, fromC, toC) (priceUsd, priceEur, priceRub, priceJpy, priceCad, priceGbp)
        _ -> nonInteractiveMode (priceUsd, priceEur, priceRub, priceJpy, priceCad, priceGbp)

-- Interactive mode
interactiveMode :: (Double, Double, Double, Double, Double, Double) -> IO ()
interactiveMode (priceUsd, priceEur, priceRub, priceJpy, priceCad, priceGbp) = do
  putStrLn $ printf "Current ZEC prices: $%.2f | €%.2f | ₽%.2f | ¥%.2f | C$%.2f | £%.2f"
    priceUsd priceEur priceRub priceJpy priceCad priceGbp

  putStrLn "Pick a currency:"
  putStrLn "(1) ZEC"
  putStrLn "(2) USD"
  putStrLn "(3) EUR"
  putStrLn "(4) RUB"
  putStrLn "(5) JPY"
  putStrLn "(6) CAD"
  putStrLn "(7) GBP"
  putStrLn "Enter 1, 2, 3, 4, 5, 6 or 7:"
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
      putStrLn $ printf "%.3f ZEC = %.2f CAD" zecAmount (zecAmount * priceCad)
      putStrLn $ printf "%.3f ZEC = %.2f GBP" zecAmount (zecAmount * priceGbp)

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

    "6" -> do
      putStrLn "Enter amount in CAD:"
      input <- getLine
      let cadAmount = read input :: Double
          zecAmount = cadAmount / priceCad
      putStrLn $ printf "%.2f CAD = %.3f ZEC" cadAmount zecAmount

    "7" -> do
      putStrLn "Enter amount in GBP:"
      input <- getLine
      let gbpAmount = read input :: Double
          zecAmount = gbpAmount / priceGbp
      putStrLn $ printf "%.2f GBP = %.3f ZEC" gbpAmount zecAmount

    _ -> putStrLn "Invalid choice."

-- Non-interactive mode message
nonInteractiveMode :: (Double, Double, Double, Double, Double, Double) -> IO ()
nonInteractiveMode (priceUsd, priceEur, priceRub, priceJpy, priceCad, priceGbp) = do
  putStrLn $ printf "Current ZEC prices: $%.2f | €%.2f | ₽%.2f | ¥%.2f | $%.2f CAD | £%.2f"
    priceUsd priceEur priceRub priceJpy priceCad priceGbp
  putStrLn "Use --interactive or -i flag to enter interactive mode."
  putStrLn "Or use --amount, --from and --to flags for conversion. For example:"
  putStrLn "  zcash-to-fiat --amount 2.5 --from ZEC --to USD"

-- Conversion logic for direct flags
convertAndPrint :: (Double, String, String) -> (Double, Double, Double, Double, Double, Double) -> IO ()
convertAndPrint (amt, fromC, toC) (priceUsd, priceEur, priceRub, priceJpy, priceCad, priceGbp) = do
  let getRate cur = case map toUpper cur of
        "ZEC" -> Just 1.0
        "USD" -> Just priceUsd
        "EUR" -> Just priceEur
        "RUB" -> Just priceRub
        "JPY" -> Just priceJpy
        "CAD" -> Just priceCad
        "GBP" -> Just priceGbp
        _     -> Nothing

      fromRate = getRate fromC
      toRate = getRate toC

  case (fromRate, toRate) of
    (Just fr, Just tr) -> do
      -- Convert amount to ZEC base, then to target currency
      let amtInZec = amt / fr
          converted = amtInZec * tr
      putStrLn $ printf "%.3f %s = %.3f %s" amt fromC converted toC
    _ -> putStrLn "Unsupported currency code. Supported: ZEC, USD, EUR, RUB, JPY, CAD, GBP."

-- Fetch price from Coingecko API
getZecPrice :: IO (Double, Double, Double, Double, Double, Double)
getZecPrice = do
  let url = https "api.coingecko.com" /: "api" /: "v3" /: "simple" /: "price"
      params = "ids" =: ("zcash" :: T.Text) <> "vs_currencies" =: ("usd,eur,rub,jpy,cad,gbp" :: T.Text)
  response <- runReq defaultHttpConfig $ req GET url NoReqBody lbsResponse params
  let body = responseBody response
  case Aeson.eitherDecode body of
    Left err -> error $ "JSON parse error: " ++ err
    Right pr -> do
      let zp = zcash pr
      return (usd zp, eur zp, rub zp, jpy zp, cad zp, gbp zp)

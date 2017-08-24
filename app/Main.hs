{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString.Base64 (decodeLenient)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import GFW.Args
import GFW.Converters.DNSMasq
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Exit (exitFailure, exitSuccess)

gfwlistTxtURL :: String
gfwlistTxtURL =
  "https://raw.githubusercontent.com/gfwlist/gfwlist/master/gfwlist.txt"


downloadGFWList :: IO L.ByteString
downloadGFWList = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest gfwlistTxtURL
  response <- httpLbs request manager
  return $ responseBody response

process :: Args -> IO ()
process (Args "dnsmasq" d i p) = do
  encoded <- downloadGFWList
  usersRules <- readFile ".extraRules.gfw"
  generate p $ generateConf ((decode' encoded) ++ usersRules) d i
  where
    decode' bs = B8.unpack $ decodeLenient $ L.toStrict bs
process (Args c _ _ _) =
  putStrLn ("Converter " ++ c ++ " is not supported!") >> exitFailure

generate :: String -> String -> IO ()
generate path = writeFile path

main :: IO ()
main = parseOptions >>= process >> exitSuccess

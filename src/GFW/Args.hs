module GFW.Args where

import Data.Semigroup ((<>))
import Options.Applicative

data Args = Args
  { converter :: String
  , dnsServer :: String
  , ipsetName :: String
  , opath     :: String
  }

args :: Parser Args
args =
    Args <$>
    strOption
        (long "convert" <> short 'c' <> metavar "name" <> help "Convert name" <>
         showDefault <>
         value "dnsmasq") <*>
    strOption
        (long "dns" <> short 'd' <> metavar "address#port" <>
         help "DNS server address with port. e.g. 8.8.8.8#53") <*>
    strOption
        (long "ipset" <> short 'i' <> metavar "name" <>
         help "dnsmasq ipset name") <*>
    strOption
        (long "path" <> short 'p' <> metavar "filepath" <>
         help "Output File path. e.g. dnsmasq_list.conf")

options :: ParserInfo Args
options =
  info
    (helper <*> args)
    (fullDesc <> progDesc "generate x" <>
     header "gfwlist2x - a tool convert gfwlist.txt to x")

parseOptions :: IO Args
parseOptions = execParser options

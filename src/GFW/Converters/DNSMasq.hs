module GFW.Converters.DNSMasq
  ( generateConf
  ) where

import Data.List (nub, intercalate)
import GFW.Parser
import GFW.Types

generateConf :: String -> String -> String -> String
generateConf input dnsServer ipsetName = intercalate "\n" domains
  where
    domains =
      case parseGFW input of
        Left _ -> ["parse error!"]
        Right fields -> domains' $ nub fields
    domains' = foldr addDomain []
    addDomain (Domain x) result = concat ["server=/.", x, "/", dnsServer, "\nipset=/.", x, "/", ipsetName] : result
    addDomain _ result = result

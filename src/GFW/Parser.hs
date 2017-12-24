module GFW.Parser
  ( parseGFW
  ) where

import GFW.Types
import Text.ParserCombinators.Parsec

parseGFW :: String -> Either ParseError [Field]
parseGFW = parse gfwRules ""

gfwRules :: Parser [Field]
gfwRules = spaces *> many field <* spaces

field :: Parser Field
field =
  choice
    [ try autoproxy
    , try comment
    , try ruleA
    , try ruleB
    , try ruleC
    , try ruleD
    , try ruleE
    , try ruleF
    , try invalidItem
    ] <*
  spaces <?> "valid field"

--- [AutoProxy 0.2.9]
--- Just pass
autoproxy :: Parser Field
autoproxy = AutoProxy <$> version <?> "valid autoproxy version"
  where
    version = between (char '[') (char ']') content
    content = many1 alphaNum *> space *> many1 (alphaNum <|> char '.')

--- !this is comment
--- Just pass
comment :: Parser Field
comment = Comment <$> comment' <?> "valid comment"
  where
    comment' = char '!' *> many (noneOf "\n")

--- example.com
--- a*.example.com
--- *a.example.com
--- *.example.com
--- com
onlyDomain :: Parser String
onlyDomain =
  try (string "*." *> domain') <|> try domain' <|> try (regexpDomain *> domain') <|>
  domain''
  where
    reg =
      optional (char '*') *> many1 (alphaNum <|> char '-') *>
      optional (char '*')
    regexpDomain = reg *> char '.'
    domain' =
      concat <$>
      ((:) <$> many1 (alphaNum <|> char '-') <*>
       many1 ((:) <$> char '.' <*> many1 (alphaNum <|> char '-')))
    domain'' = many1 alphaNum -- just com net org ...

--- a/b/c
--- /a/b/c
--- /a/b/*.jpg
path :: Parser ()
path =
  optional (char '/') <*
  many1 (alphaNum <|> char '-' <|> char '*' <|> char '-' <|> char '.') <*
  optional (char '/')

--- 192.168.1.1
--- 192.168.1.1:8080
--- |schema://192.168.1.1/
--- |schema://192.168.1.1
--- Just pass
ruleA :: Parser Field
ruleA = IP <$> addr <*> option 80 port <?> "valid ip"
  where
    addr = do
      optional (char '|' *> many1 alphaNum *> string "://")
      a <- many1 digit
      char '.'
      b <- many1 digit
      char '.'
      c <- many1 digit
      char '.'
      d <- many1 digit
      optional (char '/')
      return $ read <$> [a, b, c, d]
    port = char ':' *> (read <$> many1 digit)

--- |schema://example.com
--- |schema://example.com/
--- |schema://example.com/a/b
--- |schema://example.com/*.jpg
--- Ignore scheme and product a Domain value
ruleB :: Parser Field
ruleB = Domain <$> domain' <?> "valid url"
  where
    domain' =
      char '|' *> many1 alphaNum *> string "://" *> onlyDomain <*
      optional (char '/') <*
      many path

--- ||example.com
ruleC :: Parser Field
ruleC = Domain <$> domain' <?> "valid domain"
  where
    domain' = string "||" *> optional (char '.') *> onlyDomain <* many path

--- .example.com
--- example.com
ruleD :: Parser Field
ruleD = Domain <$> domain' <?> "valid domain"
  where
    domain' = optional (char '.') *> onlyDomain <* many path

--- whitelist rules
--- @@|http://example.com
--- @@||sub.example.com
--- Just pass
ruleE :: Parser Field
ruleE = WhiteList <$> domain' <?> "valid white list"
  where
    domain' = string "@@" *> many1 (noneOf "\n")

--- regular expression
--- /example/
--- Just pass
ruleF :: Parser Field
ruleF = RegExp <$> regexp' <?> "valid regexp"
  where
    regexp' = (:) <$> char '/' <*> many1 (noneOf "\n")

--- invalid rules in gfwlist
invalidItem :: Parser Field
invalidItem = Invalid <$> many1 (noneOf "\n")

import Text.ParserCombinators.Parsec
  ( Parser, parse, string, char, spaces, eof, optionMaybe, many, sepBy, sepBy1, try, (<|>) )

joinWithComma [] = ""
joinWithComma [x] = x
joinWithComma (x:xs) = x ++ "," ++ joinWithComma xs

charIn [] = fail "Err."
charIn (c:cs) = char c <|> charIn cs

transpile :: String -> Either String String
transpile input = 
  let
    validFirstChars = '_' : ['a'..'z'] ++ ['A'..'Z']
    digitChars = ['0'..'9']
    validChars = validFirstChars ++ digitChars
    parseName :: Parser String
    parseName = do
      first <- charIn validFirstChars
      rest <- many (charIn validChars)
      spaces
      return (first : rest)
    parseNumber :: Parser String
    parseNumber = 
      try (string "0") <|> do
        firstDigit <- charIn ['1'..'9']
        restDigits <- many (charIn digitChars)
        spaces
        return (firstDigit : restDigits)
    parseLiteral :: Parser String
    parseLiteral = try parseName <|> parseNumber
    parseParams :: Parser String
    parseParams = do
      exprs <- sepBy parseExpr (string "," >> spaces)
      return (joinWithComma exprs)
    parseParamsNonEmpty :: Parser String
    parseParamsNonEmpty = do
      exprs <- sepBy1 parseExpr (string "," >> spaces)
      return (joinWithComma exprs)
    parseStmt :: Parser String
    parseStmt = do
      lits <- sepBy parseLiteral spaces
      return (concat (map (++ ";") lits))
    parseShortLambda :: Parser String
    parseShortLambda = do
      _ <- char '{'
      spaces
      stm <- parseStmt
      _ <- char '}'
      spaces
      return $ "(){" ++ stm ++ "}"
    parseLongLambda :: Parser String
    parseLongLambda = do
      _ <- char '{'
      spaces
      paramsList <- parseParamsNonEmpty
      _ <- string "->"
      spaces
      stm <- parseStmt
      _ <- char '}'
      spaces
      return $ "(" ++ paramsList ++ "){" ++ stm ++ "}"

    parseLambda :: Parser String
    parseLambda = try parseLongLambda <|> parseShortLambda
    parseExpr :: Parser String
    parseExpr = try parseLiteral <|> parseLambda
    parseParenParams :: Parser String
    parseParenParams = do
      _ <- char '('
      spaces
      params <- parseParams
      _ <- char ')'
      spaces
      return params
    parseLongFunc :: Parser String
    parseLongFunc = do
      expr <- parseExpr
      params <- parseParenParams
      lamOpt <- optionMaybe parseLambda
      return $ case lamOpt of
        Nothing -> expr ++ "(" ++ params ++ ")"
        Just lam -> if null params
                      then expr ++ "(" ++ lam ++ ")"
                      else expr ++ "(" ++ params ++ "," ++ lam ++ ")"
    parseShortFunc :: Parser String
    parseShortFunc = do
      expr <- parseExpr
      lam <- parseLambda
      return $ expr ++ "(" ++ lam ++ ")"
    parseFunction :: Parser String
    parseFunction = try parseLongFunc <|> parseShortFunc
    parseFinal :: Parser String
    parseFinal = do
      spaces
      f <- parseFunction
      eof
      return f
  in case parse parseFinal "" input of
       Left _ -> Left "Hugh?"
       Right res -> Right res
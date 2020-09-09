import Data.Char (isDigit, isAlpha)
import Data.List (elemIndex)
import qualified Data.Map as Map
data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)
data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i)        = TInt (read i) : tokenize is
  | not (null s)        = TStr s : tokenize ss
  | otherwise           = tokenize cs
  where
    (i, is) = span isDigit xxs
    (s, ss) = span isAlpha xxs
compile :: String -> [String]
compile = pass3 . pass2 . pass1
prior :: Map.Map String Int
prior = Map.fromList [("+", 1), ("-", 1), ("*", 2), ("/", 2)]
pass1 :: String -> AST
pass1 program = buildAST tokens
  where
    tokens = tokenize program
    buildAST :: [Token] -> AST
    buildAST toks =
      let (vars, bodyTokens) = parseVars toks
          output = parseExpr vars (TChar '(' : bodyTokens ++ [TChar ')'])
      in head output

    parseVars :: [Token] -> ([String], [Token])
    parseVars (TChar '[' : rest) = go rest []
      where
        go (TChar ']' : xs) acc = (reverse acc, xs)
        go (TStr s : xs) acc    = go xs (s:acc)
        go (_:xs) acc           = go xs acc
    parseVars xs = ([], xs)
    parseExpr :: [String] -> [Token] -> [AST]
    parseExpr vars = go [] []
      where
        go out ops [] = out
        go out ops (TInt n : ts) = go (Imm n : out) ops ts
        go out ops (TStr s : ts)
          | s `elem` vars =
              let Just idx = elemIndex s vars
              in go (Arg idx : out) ops ts
          | otherwise = error $ "Unknown var" ++ s
        go out ops (TChar '(' : ts) = go out ('(':ops) ts
        go out ops (TChar ')' : ts) = 
          let (out', ops') = popUntilParen out ops
          in go out' (tail ops') ts
        go out ops (TChar op : ts)
          | [op] `Map.member` prior =
              let (out', ops') = popWhile (\o -> o /= '(' && prec o >= prec op) out ops
              in go out' (op:ops') ts
        go out ops (_:ts) = go out ops ts
        prec o = Map.findWithDefault 0 [o] prior
        popUntilParen out (o:ops)
          | o == '(' = (out, o:ops)
          | otherwise =
              let (x:y:ys) = out
              in popUntilParen ((combine o y x):ys) ops
        popWhile _ out [] = (out, [])
        popWhile cond out (o:ops)
          | cond o =
              let (x:y:ys) = out
              in popWhile cond (combine o y x : ys) ops
          | otherwise = (out, o:ops)
        combine op a b = case op of
          '+' -> Add a b
          '-' -> Sub a b
          '*' -> Mul a b
          '/' -> Div a b
pass2 :: AST -> AST
pass2 ast = case ast of
  Add a b -> simplify Add (+) a b
  Sub a b -> simplify Sub (-) a b
  Mul a b -> simplify Mul (*) a b
  Div a b -> simplify Div div a b
  _       -> ast
  where
    simplify ctor op a b =
      let a' = pass2 a
          b' = pass2 b
      in case (a', b') of
        (Imm x, Imm y) -> Imm (op x y)
        _              -> ctor a' b'
pass3 :: AST -> [String]
pass3 ast = case ast of
  Imm n     -> ["IM " ++ show n, "PU"]
  Arg n -> ["AR " ++ show n, "PU"]
  Add a b   -> compileBin a b "AD"
  Sub a b   -> compileBin a b "SU"
  Mul a b   -> compileBin a b "MU"
  Div a b   -> compileBin a b "DI"
  where
    compileBin a b op =
      pass3 a ++ pass3 b ++ ["PO", "SW", "PO", op, "PU"]
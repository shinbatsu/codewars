import Text.Read (readMaybe)

data Expr = Var String | Const Double | Op String [Expr] deriving (Eq, Show)

diff :: String -> String
diff = serialize . suppress . df . parse . tokenize
tokenize [] = []
tokenize (c:cs)
  | c == '('  = "[" : tokenize cs
  | c == ')'  = "]" : tokenize cs
  | c == ' '  = tokenize cs
  | otherwise = let (t, r) = span (`notElem` " ()") (c:cs) in t : tokenize r
parse = fst . parseExpr
  where
    parseExpr ("[":ts) = let (es, r) = parseList ts in (Op (getOp es) (tail es), r)
    parseExpr (t:ts) = (maybe (Var t) Const (readMaybe t), ts)
    parseList ("]":ts) = ([], ts)
    parseList ts = let (e, r1) = parseExpr ts; (es, r2) = parseList r1 in (e:es, r2)
    getOp (Op o _ : _) = o
    getOp (Var o : _) = o
    getOp _ = error "Err"
df (Var _)       = Const 1
df (Const _)     = Const 0
df (Op op [a,b]) = suppress $ case op of
  "+" -> Op "+" [df a, df b]
  "-" -> Op "-" [df a, df b]
  "*" -> Op "+" [Op "*" [df a, b], Op "*" [a, df b]]
  "/" -> Op "/" [Op "-" [Op "*" [df a, b], Op "*" [a, df b]], Op "^" [b, Const 2]]
  "^" -> Op "*" [b, Op "^" [a, Op "-" [b, Const 1]]]
  _   -> Op op [df a, df b]
df (Op op [a])   = suppress $ Op "*" [df a, case op of
  "sin" -> Op "cos" [a]
  "cos" -> Op "*" [Const (-1), Op "sin" [a]]
  "tan" -> Op "+" [Const 1, Op "^" [Op "tan" [a], Const 2]]
  "exp" -> Op "exp" [a]
  "ln"  -> Op "/" [Const 1, a]
  _     -> Op op [df a]]
df _ = error "Err"
suppress (Op op args) = case (op, map suppress args) of
  ("+", [Const 0, b]) -> b
  ("+", [a, Const 0]) -> a
  ("+", [Const x, Const y]) -> Const (x + y)
  ("-", [a, Const 0]) -> a
  ("-", [Const x, Const y]) -> Const (x - y)
  ("*", [Const 0, _]) -> Const 0
  ("*", [_, Const 0]) -> Const 0
  ("*", [Const 1, b]) -> b
  ("*", [a, Const 1]) -> a
  ("*", [Const x, Const y]) -> Const (x * y)
  ("/", [Const 0, _]) -> Const 0
  ("/", [a, Const 1]) -> a
  ("/", [Const x, Const y]) -> Const (x / y)
  ("^", [_, Const 0]) -> Const 1
  ("^", [a, Const 1]) -> a
  ("^", [Const x, Const y]) -> Const (x ** y)
  (o, as) -> Op o as
suppress e = e
serialize :: Expr -> String
serialize (Var v) = v
serialize (Const x) = if fromInteger (round x) == x then show (round x) else show x
serialize (Op o xs) = "(" ++ unwords (o : map serialize xs) ++ ")"
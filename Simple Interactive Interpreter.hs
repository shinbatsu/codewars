import Data.Char (isAlpha, isDigit, isAlphaNum)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Fixed (mod')
import Data.Maybe (fromJust)

newInterpreter = (M.empty, M.empty)
input ln st = lexr ln >>= parse st
parse st [] = Right (Nothing, st)
parse (fs, vs) (Fn : Id fnm : rs)
  | M.member fnm vs = Left "Error"
  | otherwise = (\f -> (Nothing, (M.insert fnm f fs, vs)))
                  <$> parseFnDef fnm (aris fs) rs
parse (fs, vs) ts =
  (\(vs', v) -> (Just v, (fs, vs')))
    <$> (parseExp (aris fs) ts >>= eval fs vs)
data Token
  = Fn | FnOp | Lp | Rp | OpE
  | OpA Char | OpM Char
  | Id String | Nu Double
  deriving (Eq, Show)
type FnT    = ([String], Exp)
type Ft     = Map String FnT
type Vt     = Map String Double
type Intp   = (Ft, Vt)
type Res    = Maybe Double
type Aris   = Map String Int
aris = M.map (length . fst)
data Exp
  = Asn String Exp
  | Op (Double -> Double -> Double) Exp Exp
  | Lit Double
  | Var String
  | Call String [Exp]
idS c = c == '_' || isAlpha c
idC c = c == '_' || isAlphaNum c
numS c = isDigit c || c == '.'
lexr [] = Right []
lexr str@(c:cs)
  | Just (tok, rest) <- mp str = do
      toks <- lexr rest
      return (tok : toks)
  | c `elem` " \t\n" = lexr cs
  | c `elem` "+-"    = consToken (OpA c) cs
  | c `elem` "*/%"   = consToken (OpM c) cs
  | idS c            = lexId str
  | numS c           = lexNum str
  | otherwise        = Left "Error"
  where
    mp ('f':'n':xs) = Just (Fn, xs)
    mp ('=':'>':xs) = Just (FnOp, xs)
    mp ('(':xs)     = Just (Lp, xs)
    mp (')':xs)     = Just (Rp, xs)
    mp ('=':xs)     = Just (OpE, xs)
    mp _            = Nothing
    consToken t rest = do
      toks <- lexr rest
      return (t : toks)
lexId s = fmap (Id n :) $ lexr r
  where (n, r) = span idC s
lexNum s = do
  let (p, ap) = span isDigit s
      (sf, rest) = case ap of
                    ('.':ds) -> let (f, r) = span isDigit ds in ('.':f, r)
                    r        -> ("", r)
      ns = p ++ sf
  if null ns then
    Left "Error"
  else if last ns == '.' then
    Left "Error"
  else do
    toks <- lexr rest
    return (Nu (read ('0' : ns)) : toks)
parseExp fa ts = pE ts >>= finish
  where
    finish ([], e) = Right e
    finish (_:_, _) = Left "Error"
    pE (Id v : OpE : ts)
      | M.member v fa = Left "Error"
      | otherwise = fmap (fmap $ Asn v) (pE ts)
    pE ts = pSum ts
    pSum ts = pTerm ts >>= pSumT
    pSumT (OpA o : ts, l) = pTerm ts >>= pSumT . fmap (Op (getOp o) l)
    pSumT x = Right x
    pTerm ts = pFact ts >>= pTermT
    pTermT (OpM o : ts, l) = pFact ts >>= pTermT . fmap (Op (getOp o) l)
    pTermT x = Right x
    pFact (Nu x : ts) = Right (ts, Lit x)
    pFact (Lp : ts) = pE ts >>= checkRp
    pFact (Id nm : ts) =
      case M.lookup nm fa of
        Nothing -> Right (ts, Var nm)
        Just nargs -> fmap (Call nm) <$> pArgs nargs ts
    pFact _ = Left "Error"
    pArgs 0 ts = Right (ts, [])
    pArgs n ts = pE ts >>= \(ts', a) -> fmap (a :) <$> pArgs (n - 1) ts'
checkRp (Rp : ts, e) = Right (ts, e)
checkRp _ = Left "Error"
eval fs = go
  where
    go vs (Asn n e) = do
      (vs', x) <- go vs e
      return (M.insert n x vs', x)
    go vs (Op f l r) = do
      (vs', x) <- go vs l
      (vs'', y) <- go vs' r
      return (vs'', f x y)
    go vs (Lit x) = Right (vs, x)
    go vs (Var n) =
      case M.lookup n vs of
        Just val -> Right (vs, val)
        Nothing  -> Left "Error"
    go vs (Call fn args) =
      case M.lookup fn fs of
        Nothing -> Left "Error"
        Just (params, body) -> do
          (_, argVals) <- evalSeq vs args
          eval fs (M.fromList $ zip params argVals) body
    evalSeq vs [] = Right (vs, [])
    evalSeq vs (e:es) = do
      (vs', x) <- go vs e
      (vs'', xs) <- evalSeq vs' es
      return (vs'', x:xs)
parseFnDef fn fa ts = pParams ts >>= \(ps, ts') ->
  parseExp (M.insert fn (length ps) fa) ts' >>= chkParams ps
  where
    pParams (Id n : ts) = fmap (\(ps, r) -> (n:ps, r)) $ pParams ts
    pParams (FnOp : ts) = Right ([], ts)
    pParams [] = Left "Error"
    pParams (_ : _) = Left "Error"
    chkParams ps e
      | null bad = Right (ps, e)
      | otherwise = Left "Error"
      where
        varsInE = vars e
        bad = filter (`notElem` ps) varsInE
    vars (Asn n e)    = n : vars e
    vars (Op _ l r)   = vars l ++ vars r
    vars (Lit _)      = []
    vars (Var n)      = [n]
    vars (Call _ es)  = concatMap vars es
getOp c = case c of 
  '+' -> (+)
  '-' -> (-)
  '*' -> (*)
  '/' -> (/)
  '%' -> mod'
  _   -> error "Error"
module Interp where

import Grammars

-- Aux para las funciones n-arias 

fvList :: [ASA] -> [String]
fvList []     = []
fvList (e:es) = freeVars e ++ fvList es

namesList :: [ASA] -> [String]
namesList []     = []
namesList (e:es) = names e ++ namesList es


-- RETO 3: sustitucion nominal que evita captura

freeVars :: ASA -> [String]
freeVars (Num _)       = []
freeVars (Boolean _)   = []
freeVars (Id x)        = [x]

freeVars (Not e)       = freeVars e
freeVars (Add1 e)      = freeVars e
freeVars (Sub1 e)      = freeVars e
freeVars (ZeroP e)     = freeVars e

freeVars (Expt e1 e2)  = freeVars e1 ++ freeVars e2
freeVars (EqP e1 e2)   = freeVars e1 ++ freeVars e2

freeVars (And es)      = fvList es
freeVars (Or es)       = fvList es
freeVars (Add es)      = fvList es
freeVars (Sub es)      = fvList es
freeVars (Mul es)      = fvList es
freeVars (Div es)      = fvList es
freeVars (Lt es)       = fvList es
freeVars (Gt es)       = fvList es
freeVars (Le es)       = fvList es
freeVars (Ge es)       = fvList es

freeVars (Let [] body)            = freeVars body
freeVars (Let ((x, e) : bs) body) =
  freeVars e ++ filter (/= x) (freeVars (Let bs body))

freeVars (LetStar [] body)            = freeVars body
freeVars (LetStar ((x, e) : bs) body) =
  freeVars e ++ filter (/= x) (freeVars (LetStar bs body))


names :: ASA -> [String]
names (Num _)       = []
names (Boolean _)   = []
names (Id x)        = [x]

names (Not e)       = names e
names (Add1 e)      = names e
names (Sub1 e)      = names e
names (ZeroP e)     = names e

names (Expt e1 e2)  = names e1 ++ names e2
names (EqP e1 e2)   = names e1 ++ names e2

names (And es)      = namesList es
names (Or es)       = namesList es
names (Add es)      = namesList es
names (Sub es)      = namesList es
names (Mul es)      = namesList es
names (Div es)      = namesList es
names (Lt es)       = namesList es
names (Gt es)       = namesList es
names (Le es)       = namesList es
names (Ge es)       = namesList es

names (Let [] body)            = names body
names (Let ((x, e) : bs) body) =
  [x] ++ names e ++ names (Let bs body)

names (LetStar [] body)            = names body
names (LetStar ((x, e) : bs) body) =
  [x] ++ names e ++ names (LetStar bs body)


freshName :: [String] -> String


sust :: ASA -> String -> ASA -> ASA

sustMany :: ASA -> [Binding] -> ASA

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA

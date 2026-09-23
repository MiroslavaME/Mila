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
names (Let ((x, e) : xs) body) =
  [x] ++ names e ++ names (Let xs body)

names (LetStar [] body)            = names body
names (LetStar ((x, e) : xs) body) =
  [x] ++ names e ++ names (LetStar xs body)


freshName :: [String] -> String


sust :: ASA -> String -> ASA -> ASA

sustMany :: ASA -> [Binding] -> ASA

-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.

--funciones auxiliares para ahorrar codigo
evalNum :: ASA -> Maybe Int
evalNum e = case bigStep e of
  Just (Num n) -> Just n
  _            -> Nothing

evalBool :: ASA -> Maybe Bool
evalBool e = case bigStep e of
  Just (Boolean b) -> Just b
  _                -> Nothing

evalNums :: [ASA] -> Maybe [Int]
evalNums args = 
  let res = map evalNum args
  in if all (/= Nothing) res
     then Just [n | Just n <- res]
     else Nothing

evalBools :: [ASA] -> Maybe [Bool]
evalBools args = 
  let res = map evalBool args
  in if all (/= Nothing) res
     then Just [b | Just b <- res]
     else Nothing

compara :: (a -> a -> Bool) -> [a] -> Bool
compara op xs = and [op x y | (x, y) <- zip xs (tail xs)]

evalValues :: [(String, ASA)] -> Maybe [(String, ASA)]
evalValues [] = Just []
evalValues ((x, expr) : xs) = case bigStep expr of
  Just val -> case evalValues xs of
    Just y -> Just ((x, val) : y)
    Nothing -> Nothing
  Nothing  -> Nothing


bigStep :: ASA -> Maybe ASA
bigStep (Num n)     = Just (Num n)
bigStep (Boolean b) = Just (Boolean b)
bigStep (Id _)      = Nothing

bigStep (Add args) = case evalNums args of
  Just ns -> Just (Num (sum ns))
  Nothing -> Nothing

bigStep (Mul args) = case evalNums args of
  Just ns -> Just (Num (product ns))
  Nothing -> Nothing

bigStep (Sub args) = case evalNums args of
  Just (x:xs) -> Just (Num (foldl (\acc y -> max 0 (acc - y)) x xs))
  Just []     -> Just (Num 0)
  Nothing     -> Nothing

bigStep (Div args) = case evalNums args of
  Just (x:xs) | not (null xs) && notElem 0 xs -> Just (Num (foldl div x xs))
  _                                           -> Nothing

bigStep (And args) = case evalBools args of
  Just xs -> Just (Boolean (and xs))
  Nothing -> Nothing

bigStep (Or args) = case evalBools args of
  Just xs -> Just (Boolean (or xs))
  Nothing -> Nothing

bigStep (Lt args) = case evalNums args of
  Just ns -> Just (Boolean (compara (<) ns))
  Nothing -> Nothing

bigStep (Gt args) = case evalNums args of
  Just ns -> Just (Boolean (compara (>) ns))
  Nothing -> Nothing

bigStep (Le args) = case evalNums args of
  Just ns -> Just (Boolean (compara (<=) ns))
  Nothing -> Nothing

bigStep (Ge args) = case evalNums args of
  Just ns -> Just (Boolean (compara (>=) ns))
  Nothing -> Nothing

bigStep (Expt e1 e2) = case (evalNum e1, evalNum e2) of
  (Just n1, Just n2) -> Just (Num (n1 ^ n2))
  _                  -> Nothing

bigStep (EqP e1 e2) = case (bigStep e1, bigStep e2) of
  (Just (Num n1), Just (Num n2))         -> Just (Boolean (n1 == n2))
  (Just (Boolean b1), Just (Boolean b2)) -> Just (Boolean (b1 == b2))
  _                                      -> Nothing

bigStep (Not e) = case bigStep e of
  Just (Boolean b) -> Just (Boolean (not b))
  Just (Num _)     -> Just (Boolean False)
  _                -> Nothing

bigStep (Add1 e) = case evalNum e of
  Just n  -> Just (Num (n + 1))
  Nothing -> Nothing

bigStep (Sub1 e) = case evalNum e of
  Just n  -> Just (Num (max 0 (n - 1)))
  Nothing -> Nothing

bigStep (ZeroP e) = case evalNum e of
  Just n  -> Just (Boolean (n == 0))
  Nothing -> Nothing

bigStep (Let values body) = case evalValues values of
  Just cleanBindings -> bigStep (sustMany body cleanBindings)
  Nothing            -> Nothing

bigStep (LetStar [] body) = bigStep body
bigStep (LetStar ((x, expr) : xs) body) = case bigStep expr of
  Just val -> 
    let xs'   = map (\(y, e) -> (y, sust e x val)) xs
        body' = sust body x val
    in bigStep (LetStar xs' body')
  Nothing -> Nothing

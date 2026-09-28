module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] e = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x : xs) e
  | contains x xs = Nothing
  | otherwise = case curryFun xs e of
      Just v -> Just (Fun x v)
      Nothing -> Nothing

-- aux
contains :: (Eq a) => a -> [a] -> Bool
contains _ [] = False
contains y (x : xs) = (y == x) || contains y xs

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (foldl App e xs)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp e (x : y : xs)
  | xs == [] = Just (e x y)
  | otherwise = Just (binaryOpAux e (e x y) xs)
binaryOp _ _ = Nothing

-- aux
binaryOpAux :: (ASA -> ASA -> ASA) -> ASA -> [ASA] -> ASA
binaryOpAux e acc [x] = e acc x
binaryOpAux e acc (x : xs) = binaryOpAux e (e acc x) xs

-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS x) = Just (Num x)
desugar (BooleanS x) = Just (Boolean x)
desugar (AddS xs)
  | Just xs <- traverse desugar xs = binaryOp Add xs
  | otherwise = Nothing
desugar (SubS xs)
  | Just xs <- traverse desugar xs = binaryOp Sub xs
  | otherwise = Nothing
desugar (NotS e)
  | Just e' <- desugar e = Just (Not e')
  | otherwise = Nothing
desugar (LetS p v c) =
  case (desugar v, desugar c) of
    (Just v', Just c') -> Just (App (Fun p c') v')
    _ -> Nothing
desugar (LetStarS [] e) = Nothing
desugar (LetStarS [(p, v)] c) =
  case (desugar v, desugar c) of
    (Just v', Just c') -> Just (App (Fun p c') v')
    _ -> Nothing
desugar (LetStarS ((p, v) : bs) c) =
  case (desugar v, desugar (LetStarS bs c)) of
    (Just v', Just x) ->
      Just (App (Fun p x) v')
    _ -> Nothing
desugar (FunS l b) =
  case desugar b of
    Just b' -> curryFun l b'
    Nothing -> Nothing
desugar (AppS f args) =
  case (desugar f, traverse desugar args) of
    (Just f', Just args') -> curryApp f' args'
    _ -> Nothing

-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv id [] = Nothing
lookupEnv id ((x, v) : vs)
  | id == x = Just v
  | otherwise = lookupEnv id vs

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep e (Id x) = lookupEnv x e
bigStep e (Num x) = Just (NumV x)
bigStep e (Boolean x) = Just (BooleanV x)
bigStep e (Add x y) =
  let x' = bigStep e x
      y' = bigStep e y
   in case (x', y') of
        (Just (NumV n), Just (NumV m)) -> Just (NumV (n + m))
        _ -> Nothing
bigStep e (Sub x y) =
  let x' = bigStep e x
      y' = bigStep e y
   in case (x', y') of
        (Just (NumV n), Just (NumV m)) -> Just (NumV (max (n - m) 0))
        _ -> Nothing
bigStep e (Not b) =
  case bigStep e b of
    Just (BooleanV b') -> Just (BooleanV (not b'))
    Just (NumV n) -> Just (BooleanV False)
    _ -> Nothing
bigStep e (Fun p c) = Just (ClosureV p c e)
bigStep e (App f a) =
  let f' = bigStep e f
      a' = bigStep e a
   in case (f', a') of
        (Just (ClosureV p c e'), Just a') -> bigStep ((p, a') : e') c
        _ -> Nothing
module Interp where

import Grammars
import Data.List (foldl')

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

-- Aux
contains :: Eq a => a -> [a] -> Bool
contains _ []     = False
contains y (x:xs) = (y == x) || contains y xs

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] e = Just e
curryFun (x:xs) e 
    | contains x xs = Nothing
    | otherwise     = case curryFun xs e of
                         Just v  -> Just (Fun x v)
                         Nothing -> Nothing

-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (foldl' App e xs)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp _ []       = Nothing
binaryOp _ [_]      = Nothing
binaryOp op (x:xs)  = Just (foldl' op x xs)

-- Desazucarado
desugar :: SASA -> Maybe ASA
desugar (IdS x)      = Just (Id x)
desugar (NumS n)     = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)

desugar (AddS xs) = do
  xs' <- traverse desugar xs
  binaryOp Add xs'

desugar (SubS xs) = do
  xs' <- traverse desugar xs
  binaryOp Sub xs'

desugar (NotS e) = do
  e' <- desugar e
  Just (Not e')

desugar (LetS x e1 e2) = do
  e1' <- desugar e1
  e2' <- desugar e2
  Just (App (Fun x e2') e1')

-- Pendientes para siguiente commit:
desugar (LetStarS _ _) = Nothing
desugar (FunS _ _)     = Nothing
desugar (AppS _ _)     = Nothing

-- RETO 2: evaluacion con cerraduras ---------------------------------------

lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((k, v):rest)
  | x == k    = Just v
  | otherwise = lookupEnv x rest

bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x)        = lookupEnv x env
bigStep _   (Num n)       = Just (NumV n)
bigStep _   (Boolean b)   = Just (BooleanV b)
bigStep env (Fun x body)  = Just (ClosureV x body env)

bigStep e (Add x y) = do
  v1 <- bigStep e x
  v2 <- bigStep e y
  case (v1, v2) of
    (NumV n, NumV m) -> Just (NumV (n + m))
    _                -> Nothing

bigStep e (Sub x y) = do
  v1 <- bigStep e x
  v2 <- bigStep e y
  case (v1, v2) of
    (NumV n, NumV m) -> Just (NumV (max 0 (n - m)))
    _                -> Nothing

bigStep e (Not f) = do
  val <- bigStep e f
  case val of
    BooleanV b -> Just (BooleanV (not b))
    NumV _     -> Just (BooleanV False) 
    _          -> Nothing

bigStep _ (App _ _) = Nothing
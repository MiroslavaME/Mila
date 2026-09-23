module Interp where

import Grammars
import Distribution.Compat.Lens (_1)

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
curryFun [] e = Just e
curryFun (x:xs) e 
    | contains x xs = Nothing
    | otherwise     = case curryFun xs e of
                         Just v  -> Just (Fun x v)
                         Nothing -> Nothing
    --aux
contains :: Eq a => a -> [a] -> Bool
contains _ []     = False
contains y (x:xs) = (y == x) || contains y xs


-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (fold App e xs)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA

-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar Num _ = Just Num _
desugar Bool _ = Just Bool _
desugar String _ = Nothing
desugar (AddS xs) 
    | Just xs <- traverse desugar xs = binaryOp Add xs
    | otherwise = Nothing
desugar (SubS xs) 
    | Just xs <- traverse desugar xs = binaryOp Sub xs
    | otherwise = Nothing
desugar (NotS e) 
    | Just e' <- desugar e = Just (Not e')
    | otherwise = Nothing 
desugar (Just xs) = traverse desugar xs
desugar (Let x e1 e2) = App (Fun x, desugar e1, desugar e2)
desugar (LetStar [] e) = desugar e
desugar (LetStar ((x, e1):bs) e2) = App (Fun (x, desugar (LetStar bs e2)) e1)

-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep e Num _ = Just Num _
bigStep e Bool _ = Just Bool _
bigStep e (Add x y) = let x' = bigStep x 
                          y' = bigStep y
                          in case (x', y') of
                            (Num n, Num m) -> Just Num (n+m)
                            (_,_) -> Nothing
bigStep e (Not f) = Just not (bigStep e f)
bigStep e (Sub x y) = let x' = bigStep x 
                          y' = bigStep y
                          in case (x', y') of
                            (Num n, Num m) -> Just Num (max n-m 0)
                            (_,_) -> Nothing
bigStep env (App e1, e2) 
    | Just (ClosureV xs env) = Just v
    | otherwise = 0
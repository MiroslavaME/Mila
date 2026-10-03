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
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] e = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x : xs) e
  | contains x xs = Nothing
  | otherwise = aux (curryFun xs e)
  where
    aux (Just v) = Just (Fun x v)
    aux Nothing = Nothing

-- aux
contains :: (Eq a) => a -> [a] -> Bool
contains _ [] = False
contains y (x : xs) = (y == x) || contains y xs

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (foldl App e xs)

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp e (x : y : xs)
  | xs == [] = Just (e x y)
  | otherwise = Just (binaryOpAux e (e x y) xs)
binaryOp _ _ = Nothing

-- aux
binaryOpAux :: (ASA -> ASA -> ASA) -> ASA -> [ASA] -> ASA
binaryOpAux e acc [x] = e acc x
binaryOpAux e acc (x : xs) = binaryOpAux e (e acc x) xs

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] _ = Nothing
desugarCond [(c, t)] e = aux (desugar c) (desugar t) (desugar e)
  where
    aux (Just c') (Just t') (Just e') = Just (If c' t' e')
    aux _ _ _ = Nothing
desugarCond ((c, t) : clauses) e = aux (desugar c) (desugar t) (desugarCond clauses e)
  where
    aux (Just c') (Just t') (Just e') = Just (If c' t' e')
    aux _ _ _ = Nothing

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS x) = Just (Num x)
desugar (BooleanS x) = Just (Boolean x)
desugar (AddS xs) = aux (traverse desugar xs)
  where
    aux (Just ys) = binaryOp Add ys
    aux Nothing = Nothing
desugar (SubS xs) = aux (traverse desugar xs)
  where
    aux (Just ys) = binaryOp Sub ys
    aux Nothing = Nothing
desugar (NotS e) = aux (desugar e)
  where
    aux (Just e') = Just (Not e')
    aux Nothing = Nothing
desugar (LetS p v c) = aux (desugar v) (desugar c)
  where
    aux (Just v') (Just c') = Just (App (Fun p c') v')
    aux _ _ = Nothing
desugar (LetStarS [] e) = Nothing
desugar (LetStarS [(p, v)] c) = aux (desugar v) (desugar c)
  where
    aux (Just v') (Just c') = Just (App (Fun p c') v')
    aux _ _ = Nothing
desugar (LetStarS ((p, v) : bs) c) = aux (desugar v) (desugar (LetStarS bs c))
  where
    aux (Just v') (Just x) = Just (App (Fun p x) v')
    aux _ _ = Nothing
desugar (FunS l b) = aux (desugar b)
  where
    aux (Just b') = curryFun l b'
    aux Nothing = Nothing
desugar (AppS f args) = aux (desugar f) (traverse desugar args)
  where
    aux (Just f') (Just args') = curryApp f' args'
    aux _ _ = Nothing
desugar (IfS c t e) = aux (desugar c) (desugar t) (desugar e)
  where
    aux (Just c') (Just t') (Just e') = Just (If c' t' e')
    aux _ _ _ = Nothing
desugar (CondS clauses e) = desugarCond clauses e
desugar (LetRecS f s1 s2) = desugar (LetS f (AppS (IdS "Y") [FunS ["f"] s1]) s2)

-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv id [] = Nothing
lookupEnv id ((x, v) : env)
  | id == x = Just v
  | otherwise = lookupEnv id env

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (NumV n) = Just (NumV n)
strict (BooleanV b) = Just (BooleanV b)
strict (ClosureV x e env) = Just (ClosureV x e env)
strict (ExprV e env) = aux (bigStep env e)
  where
    aux Nothing = Nothing
    aux (Just v) = strict v

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x) = lookupEnv x env
bigStep env (Num n) = Just (NumV n)
bigStep env (Boolean b) = Just (BooleanV b)
bigStep env (Add x y) = aux (bigStep env x) (bigStep env y)
  where
    aux (Just x') (Just y') = aux' (strict x') (strict y')
      where
        aux' (Just (NumV n)) (Just (NumV m)) = Just (NumV (n + m))
        aux' _ _ = Nothing
    aux _ _ = Nothing
bigStep env (Sub x y) = aux (bigStep env x) (bigStep env y)
  where
    aux (Just x') (Just y') = aux' (strict x') (strict y')
      where
        aux' (Just (NumV n)) (Just (NumV m)) = Just (NumV (max (n - m) 0))
        aux' _ _ = Nothing
    aux _ _ = Nothing
bigStep env (Not b) = aux (bigStep env b)
  where
    aux (Just x) = aux' (strict x)
      where
        aux' (Just (BooleanV b')) = Just (BooleanV (not b'))
        aux' (Just (NumV _)) = Just (BooleanV False)
        aux' _ = Nothing
    aux _ = Nothing
bigStep env (Fun p c) = Just (ClosureV p c env)
bigStep env (App f a) = aux (bigStep env f)
  where
    aux (Just x) = aux' (strict x)
      where
        aux' (Just (ClosureV p b env')) = bigStep ((p, ExprV a env) : env') b
        aux' _ = Nothing
    aux _ = Nothing
bigStep env (If c t e) = aux (bigStep env c)
  where
    aux (Just x) = aux' (strict x)
      where
        aux' (Just (BooleanV True)) = bigStep env t
        aux' (Just (BooleanV False)) = bigStep env e
        aux' (Just (NumV 0)) = bigStep env t
        aux' (Just (NumV n)) = bigStep env e
        aux' _ = Nothing
    aux _ = Nothing

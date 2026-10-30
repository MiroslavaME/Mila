module Laboratorio06 where

-- 1. Potencia --------------------------------------------------------------

-- Precondicion: el exponente es mayor o igual que cero.
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia base exponente =
  base * potencia base (exponente - 1)

potenciaCola :: Int -> Int -> Int
potenciaCola base exp0 = aux exp0 1
  where
    aux 0 acc = acc
    aux e acc = aux (e - 1) (acc * base)

potenciaCPS :: Int -> Int -> (Int -> r) -> r
potenciaCPS _ 0 k = k 1
potenciaCPS base exp0 k =
  potenciaCPS base (exp0 - 1) (\v -> k (base * v))


-- 2. Longitud de una lista -------------------------------------------------

longitud :: [a] -> Int
longitud [] = 0
longitud (_ : xs) = 1 + longitud xs

longitudCola :: [a] -> Int
longitudCola lista = aux lista 0
  where
    aux [] acc       = acc
    aux (_ : xs) acc = aux xs (acc + 1)

longitudCPS :: [a] -> (Int -> r) -> r
longitudCPS [] k       = k 0
longitudCPS (_ : xs) k = longitudCPS xs (\v -> k (1 + v))


-- 3. Reversa de una lista --------------------------------------------------

reversa :: [a] -> [a]
reversa [] = []
reversa (x : xs) = reversa xs ++ [x]

reversaCola :: [a] -> [a]
reversaCola xs = aux xs []
  where
    aux [] acc      = acc
    aux (x:xs) acc = aux xs (x : acc)

reversaCPS :: [a] -> ([a] -> r) -> r
reversaCPS [] k     = k []
reversaCPS (x:xs) k = reversaCPS xs (\v -> k (v ++ [x]))


-- 4. Concatenacion de una lista de listas ---------------------------------

concatena :: [[a]] -> [a]
concatena [] = []
concatena (xs : xss) = xs ++ concatena xss

concatenaCola :: [[a]] -> [a]
concatenaCola xss = aux xss []
  where
    aux [] acc       = acc
    aux (l : ls) acc = aux ls (acc ++ l)

concatenaCPS :: [[a]] -> ([a] -> r) -> r
concatenaCPS [] k       = k []
concatenaCPS (l : ls) k = concatenaCPS ls (\v -> k (l ++ v))


-- 5. Intercalado de dos listas --------------------------------------------

intercala :: [a] -> [a] -> [a]
intercala [] ys = ys
intercala xs [] = xs
intercala (x : xs) (y : ys) =
  x : y : intercala xs ys

intercalaCola :: [a] -> [a] -> [a]
intercalaCola xs ys = aux xs ys []
  where
    aux [] resto acc     = reversaCola acc ++ resto
    aux resto [] acc     = reversaCola acc ++ resto
    aux (a:as) (b:bs) acc = aux as bs (b : a : acc)

intercalaCPS :: [a] -> [a] -> ([a] -> r) -> r
intercalaCPS [] ys k = k ys
intercalaCPS xs [] k = k xs
intercalaCPS (x : xs) (y : ys) k =
  intercalaCPS xs ys (\v -> k (x : y : v))
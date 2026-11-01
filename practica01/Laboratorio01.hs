module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt (x^2 + y^2)

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares [] = 0
sumaCuadradosPares xs = sum (map (^2) (filter even xs))

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces f a = f(f(f a))

varianza2 :: Double -> Double -> Double
varianza2 x y = ((x - mu)^2+(y-mu)^2)/2 where mu = (x+y)/2

clasificaTemperatura :: Int -> String
clasificaTemperatura a 
    | a < 1 = "frio extremo"
    | a <= 15 = "frio"
    | a <= 25 = "templado"
    | a <= 35 = "calido"
    | otherwise = "calor extremo"


intercala :: a -> [a] -> [a]
intercala a [] = []
intercala a [x] = [x]
intercala a (x:xs) = x:a:(intercala a xs)

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit x) = x
evalua (Suma x y)= evalua x + evalua y
evalua (Producto x y)= evalua x * evalua y
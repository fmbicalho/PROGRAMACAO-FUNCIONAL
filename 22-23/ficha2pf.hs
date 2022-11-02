module F2PF where

import Data.Char

------------ 1 ------------
----- a)

funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

-- funA [2,3,5,1] = 4.0+9.0+25.0+1.0 = 39.0

----- b)

funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0
             then h : (funB t) else (funB t)

-- funB [8,5,12] = 8:12:[] = [8,12]

----- c)

funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

-- funC [1,2,3,4,5] = funC [3,4,5] = funC [5] = [5]

----- d)

funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t

-- funD "otrec" = funD = reverse = "certo"

------------ 2 ------------
----- a)

dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h*2: dobros t

----- b)

numOcorre :: Char -> String -> Int
numOcorre x [] = 0
numOcorre x (h:t) | x == h = 1 + numOcorre x t
                  | otherwise = numOcorre x t

----- c)

positivos :: [Int] -> Bool
positivos [] = error "Lista Vazia"
positivos (h:t) |(h>=0) = positivos t
                |otherwise = False

----- d)

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h>=0      = h: soPos t
            | otherwise = soPos t

----- e)

somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h < 0     = h + somaNeg t
              | otherwise = somaNeg t 

----- f)

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) | length t <=2 = h:t
              | otherwise    = tresUlt t

----- g)

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b:segundos t

----- h)

--nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool 

----- i)
--sumTriplos [(2,4,11), (3,1,-5), (10,-3,6)] = (15,2,12)
--sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
--sumTriplos ((a,b,c):(as:bs:cs):t) = (a + sumTrilpos bs)


------------ 3 ------------
----- a)

--soDigitos :: [Char] -> [Char] -- 48->57
--soDigitos [] = []
--soDigitos (h:t) | h >= ord 48 && h<= ord 57 = h:soDigitos t
--                | otherwise = soDigitos t

----- b)

--minusculas :: [Char] -> Int

----- c)

--nums :: String -> [Int]


------------ 4 ------------


type Polinomio = [Monomio]

type Monomio = (Float,Int) --toquei float para int



p = [(2,3), (3,4), (5,3), (4,5)]

----- a)

--  expoente   lista de pol.  quantos x
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((x,y):t) | n == y    = 1 + conta n t
                  | otherwise = conta n t

----- b)
--tem de dar o maior grau
grau :: Polinomio -> Int
grau [(x,y)]   = y
grau ((x,y):t) = max y (grau t)

----- c)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((x,y):t) | n == y    = (x,y):selgrau n t
                    | otherwise = selgrau n t

----- d)

--deriv :: Polinomio -> Polinomio
--deriv [] = []
--deriv ((c,0):t) = deriv t
--deriv ((c:e):t) = (c*(fromIntegral e), e-1):deriv t

----- e)

--calcula :: Float -> Polinomio -> Float
--calcula n [(x,y)]   = x*(n^y)
--calcula n ((x,y):t) = x*(n^y) + calcula n t

----- f)

simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,y):t) | x > 0 = (x,y): simp t
               | otherwise = simp t

----- g)



----- h)

normaliza :: Polinomio -> Polinomio
normaliza p = normaliza' [] p

normaliza' acc []        = acc
normaliza' acc ((x,y):t) = 
           let acc' = addMonomio (x,y) acc
           in normaliza' acc' t


addMonomio :: Monomio -> Polinomio -> Polinomio
addMonomio (x,y) [] = [(x,y)]
addMonomio (x,y) ((a,b):t)
                   | y == b    = (x+a, y): t
                   | otherwise = (a,b):addMonomio (x,y) t

----- i)



----- j)



----- k)



----- l)
















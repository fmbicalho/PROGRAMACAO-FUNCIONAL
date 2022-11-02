module PF50 where

import Data.Char
import Data.Either

-------- 01 -------- OK

myenumFromTo :: Int -> Int -> [Int]
myenumFromTo x y = [x..y]

-------- 02 -------- OK

myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo x y z = [x,y..z]

-------- 03 -------- OK

juntaListas :: [a] -> [a] -> [a]
juntaListas [] []     = []
juntaListas [] l = l
juntaListas l []  = l
juntaListas (h:t) l = h:(juntaListas t l) 

-------- 04 -------- OK

posicaoLista :: [a] -> Int -> a
posicaoLista [] _     = error "Lista Vazia ou muito curta"
posicaoLista (x:xs) n | n == 0 = x
                      | otherwise = posicaoLista xs (n-1)

-------- 05 -------- OK

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h:t) = myreverse t ++ [h]

-------- 06 -------- OK

mytake :: Int -> [a] -> [a]
mytake 0 _     = []
mytake _ []    = []
mytake n (h:t) = h:mytake (n-1) t

-------- 07 -------- OK

mydrop :: Int -> [a] -> [a]
mydrop _ []     = []
mydrop 0 l      = l
mydrop n (x:xs) | n == 1 = xs
                | n > 1 = mydrop (n-1) xs

-------- 08 -------- OK

myzip :: [a] -> [b] -> [(a,b)]
myzip (a:as) []     = []
myzip [] (b:bs)     = []
myzip (a:as) (b:bs) = (a,b):myzip as bs

-------- 09 -------- OK

myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate x a = a:myreplicate (x-1) a

-------- 10 -------- OK

intersperse :: a -> [a] -> [a]
intersperse x [] = []
intersperse x (h:t) = h:x:intersperse x t

-------- 11 -------- OK

--group [1,2,2,3,4,4,4,5,4]=[[1],[2,2],[3],[4,4,4],[5],[4]]

group :: Eq a => [a] -> [[a]]
group []    = []
group [h]   = [[h]]
group (h:t) = let ((x:xs):ys) = group t
              in if h == x
                 then (h:x:xs):ys
                 else [h] : (x:xs):ys

-------- 12 -------- OK

l = [[1],[2,2],[3],[4,4,4],[5],[4]]

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (h:t) = h ++ myconcat t

-------- 13 -------- OK

i = [11,21,13] -- [[],[11],[11,21],[11,21,13]]

inits' :: [a]->[[a]]
inits' [] = [[]]
inits' l  = inits' (init l) ++ [l]


-------- 14 -------- OK

t = [1,2,3] -- [[1,2,3],[2,3],[3],[]]

tails:: [a]->[[a]]
tails [] = [[]]
tails l  = l : tails (tail l)


-------- 15 -------- OK

h = [[2,3,4],[1,7],[],[8,5,3]] -- [2,1,8]

heads :: [[a]] -> [a]
heads [] = []
heads ([]:resto) = heads resto
heads ((h:t):resto) = h: heads resto

-------- 16 -------- OK

lt = [[2,3,4],[1,7],[],[8,5,3]] -- 8

total :: [[a]] -> Int
total [] = 0
total ([]:t) = 0 + total t
total ((h:t):ts) = 1 + total (t:ts)

-------- 17 -------- OK

f = [("rui",3,2), ("maria",5,2), ("ana",43,7)]
-- [("rui",2), ("maria",2), ("ana",7)]

fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = [(a,c)] ++ fun t


-------- 18 -------- OK

cola :: [(String,b,c)] -> String
cola [] = ""
cola ((nome,b,c):t) = nome ++ cola t

-------- 19 -------- OK

idade :: Int -> Int -> [(String,Int)] -> [String]
idade ano ida [] = []
idade ano ida ((nome,nascimento):t)
      | ano-nascimento >= ida = [nome] ++ idade ano ida t
      | otherwise             = idade ano ida t

-------- 20 -------- OK

-- Dado n e m controi uma lista = [n^0,...,n^m-1]
powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' n 1 = [1]
powerEnumFrom' n m
        | m > 1     = powerEnumFrom' n (m - 1) ++ [n^(m-1)]
        | otherwise = []


-------- 21 -------- OK

isPrime' ::  Int -> Bool
isPrime' x | x >= 2    = testar' x 2
           | otherwise = False

test' n m  | m*m > n      = True
           | mod n m == 0 = False
           | otherwise    = test' n (m+1)


-------- 22 -------- OK

-- isPrefixOf [10,20] [10,20,30] = True
-- isPrefixOf [10,30] [10,20,30] = False

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] []  = True
isPrefixOf' [] l   = True
isPrefixOf' l []   = False
isPrefixOf' (l1:t1) (l2:t2)
                       | l1 == l2  = isPrefixOf' t1 t2
                       | otherwise = False

-------- 23 -------- OK

isSuffixOf' :: Eq a => [a] -> [a] -> Bool 
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' (x:xs) (y:ys) = if x /= y
                           then isSuffixOf' xs (y:ys)
                           else False

-------- 24 -------- OK

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool 
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) (y:ys) = if x == y
                                 then isSubsequenceOf' xs ys
                                 else isSubsequenceOf' (x:xs) ys

-------- 25 -------- OK

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x (h:hs) = if x == h
                        then 0 : map (+1) (elemIndices' x hs)
                        else map (+1) (elemIndices' x hs)

-------- 26 -------- OK

-- nub [1,2,1,2,3,1,2] = [1,2,3]

nub' :: Eq a => [a] -> [a]
nub' []     = []
nub' (h:hs) = h : filter (/= h) (nub' hs)


-------- 27 -------- OK

delete' :: Eq a => a -> [a] -> [a]
delete' n []    = []
delete' n (h:t) = if (h/=n)
                  then h: delete' n t
                  else t

-------- 28 -------- OK

barraBarra :: Eq a => [a] -> [a] -> [a]
barraBarra l1 []         = l1
barraBarra [] l2         = []
barraBarra (x:xs) (y:ys)
               | y == x = barraBarra xs ys
               | otherwise = x: barraBarra xs (y:ys)

-------- 29 -------- OK

uniao :: Eq a => [a] -> [a] -> [a]
uniao l [] = l
uniao [] l = l
uniao l (x:xs) = if x `elem` l
                 then uniao l xs
                 else uniao (l ++ [x]) xs

-------- 30 -------- OK

-- remove da primeira o que nao esta na segunda
-- intersect [1,1,2,3,4] [1,3,5] = [1,1,3]

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect l [] = []
intersect (x:xs) (y:ys)
             | x == y = x: intersect xs (y:ys)
             | otherwise = intersect xs ys

-------- 31 -------- OK

-- insert 25 [1,20,30,40] = [1,20,25,30,40]

insert' :: Ord a => a -> [a] -> [a]
insert' n [] = []
insert' n (h:t)
           | n <= h = n:h:t
           | otherwise = h : insert' n t

-------- 32 -------- OK

unwords1 :: [String] -> String
unwords1 [] = ""
unwords1 (h:t) = h ++ " " ++ unwords1 t

-------- 33 -------- OK

unlines1 :: [String] -> String
unlines1 [] = ""
unlines1 (h:t) = h ++ "\n" ++ unlines1 t

-------- 34 -------- OK

-- Retorna a posicao do maior elemento

--        lista n vazia   posicao
pMaior :: Ord a => [a] -> Int
pMaior (h:hs) = if h == maximum (h:hs) then 0 
                else 1 + pMaior hs 

-------- 35 -------- OK

lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' x ((a,b):ys)
                | x == a    = Just b
                | otherwise = lookup' x ys


-------- 36 -------- OK

preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente (x:xs:xss)
               | x <= xs = x:xs: preCrescente xss
               | otherwise = [x]

-------- 37 -------- OK

iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (x:xs) = insert' x (iSort' xs)
    where insert' :: Ord a => a -> [a] -> [a]
          insert' x [] = [x]
          insert' x (y:ys) 
            | x <= y = x : y : ys 
            | x > y = y : insert' x ys

-------- 38 -------- OK

menor :: String -> String -> Bool
menor [] l2 = True
menor l1 [] = False
menor (x:xs) (y:ys) = menor xs ys

-------- 39 -------- OK

-- elemMSet 'a' [('b',2), ('a',4), ('c',1)] = True
-- elemMSet 'd' [('b',2), ('a',4), ('c',1)] = False

elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet letra [] = False
elemMSet letra ((x,xs):t)
                  | x == letra = True
                  | otherwise = elemMSet letra t

-------- 40 -------- OK

-- converteMSet' [(’b’,2), (’a’,4), (’c’,1)] = "bbaaaac"

converteMSet' :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((h,1):hs) = h : converteMSet' hs
converteMSet' ((h,t):hs) = h : converteMSet' ((h,t-1):hs) 


-------- 41 -------- OK

-- insereMSet 'c' [('b',2), ('a',4), ('c',1)] = [(’b’,2), (’a’,4), (’c’,2)]

insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet c [] = []
insereMSet c ((a,b):t)
                 | c == a    = [(a,b+1)] ++ insereMSet c t
                 | otherwise = [(a,b)] ++ insereMSet c t

-------- 42 -------- OK

-- removeMSet 'c' [('b',2), ('a',4), ('c',1)]
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet c [] = []
removeMSet c ((a,b):t)
                 | c == a && b > 1  = [(a,b-1)] ++ removeMSet c t
                 | c == a && b <= 1 = removeMSet c t
                 | otherwise        = [(a,b)] ++ removeMSet c t

-------- 43 -------- OK

-- constroiMSet "aaabccc" = [(’a’,3), (’b’,1), (’c’,3)]

constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = insereMSet h (constroiMSet t)


-------- 44 -------- OK

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' l = (partitionLeft l, partitionRight l)
    where partitionLeft [] = []
          partitionLeft ((Left x):xs) = x : partitionLeft xs
          partitionLeft ((Right _):xs) = partitionLeft xs 
          partitionRight [] = []
          partitionRight ((Right x):xs) = x : partitionRight xs
          partitionRight ((Left _):xs) = partitionRight xs

-------- 45 -------- OK

-- Colecciona os elementos do tipo a de uma lista.

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just a): xs) = a : catMaybes' xs
catMaybes' (Nothing: xs) = catMaybes' xs


-------- 46 -------- OK

data Movimento = Norte
               | Sul
               | Este
               | Oeste
               deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) 
    | xi < xf = Este :  caminho (xi+1,yi) (xf,yf)
    | xi > xf = Oeste : caminho (xi-1,yi) (xf,yf)
    | yi < yf = Norte : caminho (xi,yi+1) (xf,yf)
    | yi > yf = Sul :   caminho (xi,yi-1) (xf,yf)
    | otherwise = []

-------- 47 -------- OK

hasLoops :: (Int,Int) -> [Movimento] -> Bool 
hasLoops (a,b) [] = False
hasLoops (a,b) x
           | elem (a,b) (aux (a,b) x) = True
           | otherwise                = False

aux :: (Int,Int) -> [Movimento] -> [(Int,Int)]
aux (a,b) [] = []
aux (a,b) (x:xs) = case x of 
  Norte -> (a,b+1):aux (a,b+1) xs
  Sul -> (a,b-1):aux (a,b-1) xs
  Este -> (a+1,b):aux (a+1,b) xs
  Oeste -> (a-1,b):aux (a-1,b) xs


-------- 48 -------- OK

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int 
contaQuadrados [] = 0
contaQuadrados ((Rect (a,b)(c,d)):xs)
                      | abs (a-c) == abs (b-d) = 1+ contaQuadrados xs
                      | otherwise              = contaQuadrados xs

-------- 49 -------- OK

areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1)(x2,y2)):t) = abs ((x1-x2)*(y1-y2)) + areaTotal t

-------- 50 -------- OK

data Equipamento = Bom
                 | Razoavel
                 | Avariado
                deriving Show

naoReparar:: [Equipamento] -> Int
naoReparar []           = 0
naoReparar (Bom:t)      = 1 + naoReparar t
naoReparar (Razoavel:t) = 1 + naoReparar t
naoReparar (Avariado:t) = naoReparar t


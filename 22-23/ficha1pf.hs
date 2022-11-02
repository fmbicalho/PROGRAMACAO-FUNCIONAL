module F1PF where

import Data.Char

---------------------- 1 ----------------------
------ a)

perimetro :: Float -> Float
perimetro 0 = 0
perimetro r = 6.28 * r

------ b)

dist :: (Double,Double) -> (Double,Double) -> (Double,Double)
dist (x,y) (z,w) | (x > z)&&(y > w) = (x-z,y-w)
                 | (x < z)&&(y > w) = (z-x,y-w)
                 | (x > z)&&(y < w) = (x-z,w-y)
                 | (x < z)&&(y < w) = (z-x,w-y)

------ c)

primUlt :: [a] -> (a,a)
primUlt (x:y) = (x,last y)

------ d)

multiplo :: Float -> Float -> Bool
multiplo 0 _ = False
multiplo _ 0 = False
multiplo _ 1 = True
multiplo m n | m / n == n = True
             | otherwise  = False

------ e)

truncaImpar :: [a] -> [a]
truncaImpar l | odd (length l) = tail l
              | otherwise      = l

------ f)

max2 :: Int -> Int -> Int
max2 x y | x > y     = x
         | otherwise = y

------ g)

max3 :: Int -> Int -> Int -> Int
max3 x y z | (max2 x y) == x = max2 x z
           | otherwise       = max2 y z


---------------------- 2 ----------------------

------ a)

-- nRaizes :: (Int,Int,Int) -> Int
-- nRaizes a b c 

------ b)

-- raizes

---------------------- 3 ----------------------

type Hora = (Int,Int)

-- Meia noite e um quarto
hora1 :: Hora
hora1 = (0,15)

-- Duas menos um uqarto
hora2 :: Hora
hora2 = (13,45)

-- Error
hora3 :: Hora
hora3 = (25,61)

------ a)

horaValida :: Hora -> Bool
horaValida (h,m) = (h >= 0 && h < 24) && (m >= 0 && m < 60)

------ b)

comparaHoras :: Hora -> Hora -> Bool
comparaHoras (h1,m1) (h2,m2) | (h1 > h2) = True
                             | (h1 == h2) && (m1 > m2) = True
                             | otherwise = False


------ c)

horasToMinutos :: Hora -> Int
horasToMinutos (hora,minutos) = 60*hora + minutos

------ d)

minutosParaHoras :: Int -> Hora
minutosParaHoras m = (div m 60, mod m 60)

------ e)

diferencaHoras :: Hora -> Hora -> Int
diferencaHoras h1 h2 = abs (horasToMinutos h1 - horasToMinutos h2)

------ f)

adicionaMinutos :: Int -> Hora -> Hora
adicionaMinutos m h = minutosParaHoras ((horasToMinutos h) + m)


---------------------- 4 ----------------------

data Horas = H Int Int
        deriving (Show,Eq)

-- meia noite e um quarto
newHora1 :: Horas
newHora1 = H 0 15

-- duas menos um quarto
newHora2 :: Horas
newHora2 = H 13 15

------ a)

horaValida' :: Hora -> Bool
horaValida' (h,m) = (h >= 0 && h < 24) && (m >= 0 && m < 60)

------ b)

comparaHoras' :: Hora -> Hora -> Bool
comparaHoras' (h1,m1) (h2,m2) | (h1 > h2) = True
                             | (h1 == h2) && (m1 > m2) = True
                             | otherwise = False


------ c)

horasToMinutos' :: Hora -> Int
horasToMinutos' (hora,minutos) = 60*hora + minutos

------ d)

minutosParaHoras' :: Int -> Hora
minutosParaHoras' m = (div m 60, mod m 60)

------ e)

diferencaHoras' :: Hora -> Hora -> Int
diferencaHoras' h1 h2 = abs (horasToMinutos h1 - horasToMinutos h2)

------ f)

adicionaMinutos' :: Int -> Hora -> Hora
adicionaMinutos' m h = minutosParaHoras ((horasToMinutos h) + m)

---------------------- 5 ----------------------

data Semaforo = Verde
              | Amarelo
              | Vermelho
              deriving (Show,Eq)

------ a)

next :: Semaforo -> Semaforo
next s | s == Verde    = Amarelo
       | s == Amarelo  = Vermelho
       | s == Vermelho = Verde

------ b)

stop :: Semaforo -> Bool
stop s | s == Vermelho = True
       | otherwise     = False

------ c)

safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 | (s1 == Verde)&&(s2 == Vermelho) = True
           | (s2 == Verde)&&(s1 == Vermelho) = True
           | otherwise  = False

---------------------- 6 ----------------------

data Ponto = Cartesiano Double Double
           | Polar Double Double
           deriving (Show,Eq)

------ a)
--posx :: Ponto -> Double

------ b)
--posy :: Ponto -> Double

------ c)
--raio :: Ponto -> Double

------ d)
--angulo :: Ponto -> Double

------ e)
--dist :: Ponto -> Ponto -> Double


---------------------- 7 ----------------------

data Figura = Circulo    Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo  Ponto Ponto Ponto
            deriving (Show,Eq)

------ a)

--poligono :: Figura -> Bool
--poligono Circulo    = True
--poligono Rectangulo = True
--poligono Triangulo  = True
--poligono _          = False


------ b)

--vertices :: Figura -> [Ponto]


------ c)

--area :: Figura -> Double
--area (Triangulo p1 p2 p3) =
--       let a = dist p1 p2
--           b = dist p2 p3
--           c = dist p3 p1
--           s = (a+b+c) / 2 -- semi-perimetro
--       in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron

------ d)

--perimetro :: Figura -> Double

---------------------- 8 ----------------------

-- ord :: Char -> Int
-- chr :: Int -> Char

------ a)
isLower' :: Char -> Bool
isLower' c = ord c>=97 && ord c<=122



------ b) isDigit :: Char -> Bool



------ c) isAlpha :: Char -> Bool



------ d) toUpper :: Char -> Char 



------ e) intToDigit :: Int -> Char



------ f) digitToInt :: Char -> Int







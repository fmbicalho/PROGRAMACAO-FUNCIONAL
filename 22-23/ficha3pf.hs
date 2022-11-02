module F3PF where

import Data.Char

-- Gestao de informacao em listas

------------ 1 ------------

data Hora = H Int Int
            deriving Show

type Etapa = (Hora,Hora)

type Viagem = [Etapa]

v = [(H 9 30, H 10 25), 
     (H 11 20, H 12 45), 
     (H 13 30, H 14 45)]

----- a)

etapaValida :: Etapa -> Bool
etapaValida (H x y,H x1 y1) | x < x1          = True
                            | x == x1 && y<y1 = True
                            | otherwise       = False

----- b)

viagemValida :: Viagem -> Bool
viagemValida (e1:t)
                | etapaValida e1 == True = viagemValida t
                | otherwise = False
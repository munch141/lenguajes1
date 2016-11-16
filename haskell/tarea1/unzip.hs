{-|
- __Módulo__      : Unzip
- __Descripción__ : Varias implementaciones de @unzip@
- __Materia__     : Taller de Lenguajes de Programación I (CI-3661)
- __Grupo__       : 25
- __Entrega__     : Tarea Haskell

Se implementa @unzip@ usando recursión directa, @foldr@, @map@ y listas por
comprensión.
-}

module Unzip where

-- |@unzipR@ es una implementación de @unzip@ usando recursión directa.
unzipR :: [(a,b)] -> ([a],[b])
unzipR []         = ([],[])
unzipR ((x,y):xys) = (x:xs, y:ys)
  where (xs,ys) = unzipR xys

-- |@unzipF@ es una implementación de @unzip@ usando @foldr@.
unzipF :: [(a,b)] -> ([a],[b])
unzipF = foldr (\(x,y) (xs,ys) -> (x:xs, y:ys)) ([],[])

-- |@unzipM@ es una implementación de @unzip@ usando @map@.
unzipM :: [(a,b)] -> ([a],[b])
unzipM xs = (map fst xs, map snd xs)

-- |@unzipL@ es una implementación de @unzip@ usando listas por comprensión.
unzipL :: [(a,b)] -> ([a],[b])
unzipL xs = ([fst x | x <- xs], [snd x | x <- xs])
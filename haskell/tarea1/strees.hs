{-|
- __Módulo__      : Strees
- __Descripción__ : implementación de un árbol de sufijos
- __Materia__     : Taller de Lenguajes de Programación I (CI-3661)
- __Grupo__       : 25
- __Entrega__     : Tarea Haskell

Implementación de un árbol de sufijos. Se implementaron funciones para crear un 
árbol de sufijos a partir de un string y para encintrar las sub-cadenas repetidas 
más largas de un string.
-}

module Strees where

-- |Árbol de sufijos.
data SuffixTree = Leaf Int                    -- ^Hoja del árbol, contiene el 
                                              --  índice donde ocurre un sufijo.
                | Node [(String, SuffixTree)] -- ^Nodo del árbol, contiene la  
                                              --  etiqueta de una arista y el 
                                              --  sub-árbol correspondiente.

    deriving (Eq, Ord, Show)

-- *Operaciones sobre @String@

-- |@isPrefix@ determina si un @String@ @p@ es prefijo de otro.
isPrefix :: String -> String -> Bool
isPrefix "" _          = True
isPrefix _ ""          = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

-- |@removePrefix@ calcula el resultado de remover un prefijo de un 
--  @String@. Si el primer @String@ no es prefijo del segundo, resultado es 
--  @""@.
removePrefix :: String -> String -> String
removePrefix "" s = s
removePrefix (x:xs) (y:ys)
  | x == y    = removePrefix xs ys
  | otherwise = ""

-- |@suffixes@ produce una lista con todos los sufijos posibles de un @String@ 
--  en orden descendiente de longitud.
suffixes :: [a] -> [[a]]
suffixes []         = [[]]
suffixes s@(_:cola) = s : suffixes cola

-- |@isSubstring@ determina si un @String@ es una subcadena de otro.
isSubstring :: String -> String -> Bool
isSubstring s1 s2 = any (isPrefix s1) (suffixes s2)

-- |@findSubstrings@ produce los índices de cada ocurrencia de un @String@ en 
--  otro.
findSubstrings :: String -> String -> [Int]
findSubstrings s1 s2 = foldl (\indices (sufijo, indice) -> 
                                if isPrefix s1 sufijo
                                  then indice : indices
                                  else indices)
                             [] sufijosConIndice
  where sufijosConIndice = zip (suffixes s2) [0..]

-- *Operaciones sobre árboles de sufijos

-- |@getIndices@ produce los valores almacenados en las hojas del árbol.
getIndices :: SuffixTree -> [Int]
getIndices (Leaf n) = [n]
getIndices (Node l) = concatMap getIndices subArboles
  where subArboles = snd $ unzip l

-- |@findSubstrings'@ devuelve los índices de todas las ocurrencias de un 
--  @String@ en otro.
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' _ (Leaf n)  = []
findSubstrings' "" _        = []
findSubstrings' _ (Node []) = []
findSubstrings' s (Node ((a, subArbol):arboles))
  | isPrefix s a = getIndices subArbol
  | isPrefix a s = findSubstrings' (removePrefix a s) subArbol
  | otherwise    = findSubstrings' s (Node arboles)

-- |@longestRepeatedSubstring@ encuentra las subcadenas repetidas más largas
--  en un árbol de sufijos.
longestRepeatedSubstring :: SuffixTree -> [String]
longestRepeatedSubstring t = filter ((maxLength ==) . length) repetidas
  where
    repetidas = explorar t
    maxLength = maximum $ map length repetidas
    explorar (Node []) = []
    explorar (Node ((_, Leaf _):(_, Leaf _):[]))  = [""]
    explorar (Node ((_, Leaf _):hermanos)) = explorar (Node hermanos)
    explorar (Node ((s, Node hijos):hermanos)) =
      (map (s++) (explorar (Node hijos))) ++ (explorar (Node hermanos))

-- *Construcción de un árbol de sufijos

-- |@insert@ produce el árbol de sufijos resultante de insertar un sufijo 
--  con índice @i@ en un árbol de sufijos.
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s,i) (Node []) = Node [(s, Leaf i)]
insert (s,i) (Node l)
  | prefijoRepetido = Node (
                        (p, Node [(removePrefix p s, Leaf i),
                                  (removePrefix p r, rArbol)])
                        : (l0 ++ l2) )
  | otherwise       = Node ( (s, Leaf i):l )
  where
    (l0,l1)         = break ((""/=) . commonPrefix . fst) l -- se divide la lista
                                                            -- justo en la arista
                                                            -- cuya etiqueta comparte
                                                            -- un prefijo con s
    prefijoRepetido = not $ null l1  -- si la segunda lista no es vacía, hay una
                                     -- arista cuya etiqueta comparte algún prefijo 
                                     -- con s
    (r, rArbol)     = head l1 -- arista con el mismo prefijo que s
    p               = commonPrefix r -- prefijo que comparten la etiqueta y s
    l2              = tail l1 -- resto de las aristas
    commonPrefix    = map fst . takeWhile (\(a,b) -> a == b) . zip s

-- |@buildTree@ construye el árbol de sufijos de un @String@.
buildTree :: String -> SuffixTree
buildTree s = foldl (\t s -> insert s t) (Node []) sufijosConIndice
  where sufijosConIndice = zip (init $ suffixes s) [0..]


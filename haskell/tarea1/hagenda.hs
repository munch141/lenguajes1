{-|
- __Módulo__      : Hagenda
- __Descripción__ : Programa para planear eventos en el calendario.
- __Materia__     : Taller de Lenguajes de Programación I (CI-3661)
- __Grupo__       : 25
- __Entrega__     : Tarea Haskell

Este programa sirve para registrar y revisar eventos en el calendario.

  Al inicio se lee una lista de eventos guardada en el archivo de texto "__@hagenda.txt@__",
si el archivo no existe o no tiene eventos se empieza con una lista eventos vacía.
Los comandos del programa son:

    - __@j@__ para avanzar un día.
    - __@k@__ para retroceder un día.
    - __@l@__ para avanzar un mes.
    - __@h@__ para retroceder un mes.
    - __@r@__ para registrar un evento.
    - __@d@__ para eliminar un evento.
    - __@q@__ para terminar el programa.
    - Cualquier otra tecla para para mostrar el mensaje de ayuda.

  Al finalizar la ejecución del programa, se guarda la lista de eventos registrados
en el archivo "__@hagenda.txt@__".
-}

module Main where

import Control.Exception (catch)
import Data.List (sortBy)
import Data.Time (toGregorian,localDay,zonedTimeToLocalTime,getZonedTime)
import System.IO (hSetBuffering,BufferMode(..),stdin,stdout)
import System.IO.Error (ioError,isDoesNotExistError)


-- |Altura
type Height = Int
-- |Ancho
type Width  = Int

-- |@Picture@ representa una "imagen".
data Picture = Picture {
                 height :: Height,
                 width :: Width,
                 pixels :: [[Char]] -- ^Caracteres que forman la imagen.
             } deriving (Show)


-- |Días del mes. Se asume que está entre 1 y 31.
type Day   = Int

-- |Año. Se asume que es positivo.
type Year  = Int

-- |@Month@ es una enumeración de los meses del año.
data Month = Enero
           | Febrero
           | Marzo
           | Abril
           | Mayo
           | Junio
           | Julio
           | Agosto
           | Septiembre
           | Octubre
           | Noviembre
           | Diciembre
           deriving (Show,Eq,Ord,Enum,Read)

-- |@DayName@ representa los días de la semana
data DayName = Domingo
             | Lunes
             | Martes
             | Miercoles
             | Jueves
             | Viernes
             | Sabado
             deriving (Show,Eq,Ord,Enum)

-- |@Evento@ representa un evento en un día del año.
data Evento = Evento {
                year        :: Year,
                month       :: Month,
                eday        :: Day,
                nth         :: Int,
                description :: String
              }
            deriving (Show,Read)

-- *Álgebra de imágenes

-- |@pixel@ produce una imagen de 1x1 con el caracter especificado.
pixel :: Char -> Picture
pixel c = Picture {height = 1, width = 1, pixels = [[c]]}

-- |@above@ recibe dos imágenes y devuelve la imagen que resulta de poner la 
--  la primera encima de la segunda.
above :: Picture -> Picture -> Picture
above (Picture h0 w0 p0) (Picture h1 w1 p1)
  | w0 == w1 = Picture {height = h0+h1, width = w0, pixels = p0 ++ p1}
  | otherwise = error "can’t ’above’ different widths"

-- |@beside@ recibe dos imágenes y devuelve la imagen que resulta de poner la 
--  segunda del lado derecho de la primera.
beside :: Picture -> Picture -> Picture
beside (Picture h0 w0 p0) (Picture h1 w1 p1)
  | h0 == h1 = Picture {height = h0, width = w0+w1, pixels = zipWith (++) p0 p1}
  | otherwise = error "can’t ’beside’ different heights"

-- |@toString@ devuelve la cadena de caracteres de una imagen.
toString :: Picture -> String
toString = unlines . pixels

-- |@stack@ construye la imagen que resulta de combinar una lista de imágenes 
--  en una imagen en forma vertical. La primera imagen de la lista estará al 
--  tope.
stack :: [Picture] -> Picture
stack = foldl1 above

-- |@spread@ construye la imagen resultante de combinar una lista de imágenes 
--  en una imagen en forma horizontal. La primera imagen estará más a la 
--  izquierda.
spread :: [Picture] -> Picture
spread = foldl1 beside

-- |@row@ transforma un @String@ de tamaño __n__ en una imagen de altura 1 y 
--  longitud __n__ donde cada pixel es un caracter del @String@.
row :: String -> Picture
row = spread . map pixel

-- |@blank@ construye (dado un par de enteros (h,w)) una imagen vacía de 
--  dimensiones h*w. Una imagen vacía está compuesta por espacios en blanco.
blank :: (Height, Width) -> Picture
blank = spread . map stack . pixelMatrix
  where pixelMatrix = uncurry $ flip replicate . (flip replicate $ pixel ' ')

-- |@stackWith@ construye una imagen a partir de una lista de imágenes 
--  combinándolas verticalmente con una imagen en blanco entre cada una de 
--  ellas.
stackWith :: Height -> [Picture] -> Picture
stackWith = foldr1 . (\h cookie cookiesAndCream ->
                        let cream = blank (h, width cookie)
                        in cookie `above` (cream `above` cookiesAndCream))

-- |@spreadWith@ construye una imagen a partir de una lista de imágenes 
--  combinándolas horizontalmente con una imagen en blanco entre cada una de 
--  ellas.
spreadWith :: Width -> [Picture] -> Picture
spreadWith = foldr1 . (\w img imgs ->
                         let imgVacia = blank (height img, w)
                         in img `beside` (imgVacia `beside` imgs))

-- |@tile@ combina una matriz de imágenes en una imagen combinando las filas 
--  horizontalmente y las imágenes resultantes verticalmente.
tile :: [[Picture]] -> Picture
tile = stack . map spread

-- |@tileWith@ combina una matriz de imágenes como @tile@ pero entre cada fila 
--  coloca una columna en blanco de un ancho @w@ y entre cada columna coloca 
--  una fila en blanco de un alto @h@.
tileWith :: (Height,Width) -> [[Picture]] -> Picture
tileWith = uncurry $ flip (flip (.) . map . spreadWith) . stackWith

-- *Calendario

-- **Funciones auxiliares para la construcción del calendario

-- |@leap@ indica si un año es bisiesto.
leap :: Year -> Bool
leap y = (y `mod` 4 == 0) && ((y `mod` 100 /= 0) || (y `mod` 400 == 0))

-- |@mlengths@ recibe un año y devuelve una lista con la cantidad de días en 
--  cada mes del año.
mlengths :: Year -> [Day]
mlengths y = map dias [1..12]
  where dias m
          | m == 2 && leap y                       = 29
          | m == 2                                 = 28
          | (odd m && m <= 7) || (even m && 7 < m) = 31
          | otherwise                              = 30

-- |@jan1@ recibe un año y devuelve el día de la semana que correspondió con 
--  el primero de Enero de ese año.
jan1 :: Year -> DayName
jan1 y = toEnum $ (365*y + length (filter leap [1..y-1])) `mod` 7

-- |@mtotals@ calcula, para cada mes, la cantidad de días acumulados después 
--  con respecto al lunes antes del primer día del año después de que haya 
--  pasado el mes.
mtotals :: Year -> [Int]
mtotals y = scanl (+) (fromEnum $ jan1 y) (mlengths y)

-- |@fstdays@ produce una lista con el primer día de cada mes de un año.
fstdays :: Year -> [DayName]
fstdays y = map (toEnum . (flip mod) 7) (init $ mtotals y)

-- |@fstday@ devuelve el día de la semana correspondiente al primer día del 
--  mes en un año especificado.
fstday :: Month -> Year -> DayName
fstday m y = fstdays y !! (fromEnum m)

-- |@day@ produce el día de la semana correspondiente a una fecha en 
--  particular.
day :: Day -> Month -> Year -> DayName
day d m y = toEnum $ ((d-1+(fromEnum $ fstday m y)) `mod` 7)

-- |@rjustify@ dado un @String@ y un número, produce un nuevo @String@ con 
--  suficientes espacios para que el original quede alineado a la derecha.
rjustify :: Int -> String -> String
rjustify n s = (replicate (n-(length s)) ' ') ++ s

-- |@dnames@ construye una imagen que corresponde a los días de la semana.
dnames :: Picture
dnames = spread $ map (row . rjustify 3 . take 2 . show) [Domingo .. Sabado]

-- |@banner@ construye el membrete de un mes del calendario de un año.
banner :: Month -> Year -> Picture
banner m y = row $ rjustify (width dnames) (show m ++ " " ++ show y)

-- |@heading@ contruye el encabezado de un mes del calendario.
heading :: Month -> Year -> Picture
heading m y = banner m y `above` dnames

-- |@group@ organiza los elementos de una lista en grupos de tamaño igual 
--  (el último grupo puede ser de menor tamaño).
group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = subGrupo : (group n resto)
  where (subGrupo,resto) = splitAt n xs

-- |@compararEventos@ compara dos eventos por fecha, si la fecha es la misma
--  se comparan con respecto al campo @nth@.
compararEventos :: Evento -> Evento -> Ordering
compararEventos e1 e2
  | year e1 < year e2 = LT
  | year e1 > year e2 = GT
  | month e1 < month e2 = LT
  | month e1 > month e2 = GT
  | eday e1 < eday e2 = LT
  | eday e1 > eday e2 = GT
  | nth e1 < nth e2 = LT
  | nth e1 > nth e2 = GT
  | otherwise = EQ

-- **Construcción del calendario completo

-- |@pix@ construye una lista de imágenes que  corresponden a los días de un 
--  mes. Si un día tiene algún evento, se marca con '>'.
pix :: DayName -> Day -> [Day] -> [Picture]
pix d s ms = blanksAntes ++ map row (numeros [1..s] ms) ++ blanksDespues
  where
    blanksAntes   = replicate (fromEnum d) (blank (1,3))
    blanksDespues = replicate (7 - (s + (fromEnum d)) `mod` 7) (blank (1,3))
    numeros [] _                   = []
    numeros (dia:dias) []          = (rjustify 3 (show dia)) : numeros dias []
    numeros (dia:dias) ed@(de:des) =
          if de == dia
            then (rjustify 3 $ '>':(show dia)) : numeros dias des
            else (rjustify 3 (show dia)) : numeros dias ed

-- |@entries@ construye una imagen para los días de un mes del calendario.
entries :: DayName -> Day -> [Day] -> Picture
entries dn d ds = tile $ group 7 $ pix dn d ds

-- |@picture@ produce una imagen del calendario completo para un mes y año.
picture :: (Month,Year,DayName,Day,[Day]) -> Picture
picture (m,y,d,s,ms) = stack [heading m y, entries d s ms]


{-=============================================================================
=====================================MAIN======================================
=============================================================================-}

-- *El Programa Principal

-- **Cargar y guardar eventos

-- |@loadEvents@ lee eventos de un archivo y devuelve una lista de dichos 
--  eventos ordenados desde el más antiguo hasta el más reciente.
loadEvents :: FilePath -> IO [Evento]
loadEvents pathName = do
  s <- readFile pathName
  let eventos = map read (lines s)
  return $ sortBy compararEventos eventos

-- |@saveEvents@ escribe un lista de eventos ordenados por fecha en un archivo.
--  si los eventos son en la misma fecha, se ordenan por el campo @nth@.
saveEvents :: FilePath -> [Evento] -> IO ()
saveEvents pathName eventos = 
  writeFile pathName (unlines $ map show $ sortBy compararEventos eventos)
  

-- |@eventsOnMonth@ produce, dado un mes, una lista con los días en los que 
--  hay algún evento registrado para ese mes.
eventsOnMonth :: [Evento] -> Month -> [Day]
eventsOnMonth es m = removeDuplicates $ map eday $ filter ((m==) . month) es
  where
    removeDuplicates []         = []
    removeDuplicates [x]        = [x]
    removeDuplicates (x0:x1:xs) = if x0 == x1
                                    then removeDuplicates (x0:xs)
                                    else x0 : removeDuplicates (x1:xs)

-- **Funciones auxiliares para el programa principal

-- |@getCurrentDate@ devuelve una tupla con el día, mes y año actuales de la 
--  zona horaria del equipo con los tipos de datos @Day@, @Month@ y @Year@.
getCurrentDate :: IO (Day,Month,Year)
getCurrentDate = 
  do (y0,m0,d) <- fmap (toGregorian.localDay.zonedTimeToLocalTime) getZonedTime 
     let y     = fromIntegral y0 :: Int
     let m     = toEnum (m0-1)
     return (d,m,y)
        

-- |@nDays@ devuelve la cantidad de días de un mes de un año.
nDays :: Month -> Year -> Int
nDays m y = mlengths y !! fromEnum m

-- |@daysEvents@ separa la lista en eventos antes, durante y después de una fecha.
daysEvents :: [Evento] -> (Day,Month,Year) -> ([Evento],[Evento],[Evento])
daysEvents e (d,m,y) = (antes,durante,despues)
  where
    checkEventDate (d,m,y) e = (eday e == d) && (month e == m) && (year e == y)
    (antes,dd)        = break (checkEventDate (d,m,y)) e
    (durante,despues) = span (checkEventDate (d,m,y)) dd

-- |@printEventosDelDia@ imprime las descripciones de los eventos de un día.
printEventosDelDia :: [Evento] -> (Day,Month,Year) -> IO ()
printEventosDelDia e (d,m,y) =
  if null $ descripciones
    then do putStrLn "No hay eventos registrados para hoy.\n"
    else do putStrLn "Eventos del día:\n"
            putStrLn $ unlines $ map p $ zip [1..] descripciones
  where p (n,event)         = (rjustify 6 (show n ++ "- ")) ++ event
        descripciones       = map description $ eventosDelDia
        (_,eventosDelDia,_) = daysEvents e (d,m,y)

-- |@calAndEvents@ imprime el calendario y los eventos de una fecha (si los hay).
calAndEvents :: [Evento] -> (Day,Month,Year) -> Day -> IO ()
calAndEvents e (d,m,y) s =
  do putStrLn $ toString $ picture (m, y, fstday m y, s, eventsOnMonth e m)
     printEventosDelDia e (d,m,y)

-- |@clrScr@ limpia la pantalla.
clrScr :: IO ()
clrScr = putStr $ toString $ blank (24,1)


{---------------------------------OPERACIONES---------------------------------}

-- **Operaciones del programa principal

-- |@nextDay@ devuelve el sucesor de un día, si el día es el último del mes, 
--  se devuelve el primer día del mes siguiente (y año siguiente en caso de 
--  que el día sea el 31 de Diciembre)
nextDay ::  [Evento] -> (Day,Month,Year) -> IO ()
nextDay e (d,m,y) = do clrScr
                       calAndEvents e (d',m',y') (nDays m' y')
                       prompt e (d',m',y')
  where
    (d',m',y') = if d == nDays m y
                   then if m == Diciembre
                     then (1,Enero,y+1)
                     else (1,succ m,y)
                   else (d+1,m,y)

-- |@previousDay@ devuleve el predecesor de un día, si el día es el primero de 
--  un mes, devuelve el último día del mes anterior, y si el mes es Diciembre, 
--  devuelve el 1 de Enero del año anterior.
previousDay ::  [Evento] -> (Day,Month,Year) -> IO ()
previousDay e (d,m,y) = do clrScr
                           calAndEvents e (d',m',y') (nDays m' y')
                           prompt e (d',m',y')
  where
    (d',m',y') = if d == 1
                   then if m == Enero
                     then (31,Diciembre,y-1)
                     else (nDays (pred m) y,pred m,y)
                   else (d-1,m,y)

-- |@nextMonth@ devuelve la fecha correspondiente a un mes después de una fecha 
--  dada. Si el día es el último del mes, se toma el mínimo entre la cantidad 
--  de días del mes actual y del mes siguiente.
nextMonth ::  [Evento] -> (Day,Month,Year) -> IO ()
nextMonth e (d,m,y) = do clrScr
                         calAndEvents e (d',m',y') (nDays m' y')                         
                         prompt e (d',m',y')
  where 
    (m',y') = if m == Diciembre then (Enero,y+1) else (succ m,y)
    d'
      | m' == Febrero && d > (nDays Febrero y) = nDays Febrero y
      | d  == nDays m y                        = min (nDays m y) (nDays m' y)
      | otherwise                              = d

-- |@previousMonth@ devuelve la fecha correspondiente a un mes antes de una 
--  fecha dada. Si el día es el último del mes, se toma el mínimo entre la 
--  cantidad de días del mes actual y del mes anterior.
previousMonth ::  [Evento] -> (Day,Month,Year) -> IO ()
previousMonth e (d,m,y) = do clrScr
                             calAndEvents e (d',m',y') (nDays m' y')
                             prompt e (d',m',y')
  where
    (m',y') = if m == Enero then (Diciembre,y-1) else (pred m,y)
    d'
      | m' == Febrero && d > (nDays Febrero y) = nDays Febrero y
      | d  == (nDays m y)                      = min (nDays m y) (nDays m' y)
      | otherwise = d

-- |@registrarEvento@ inserta un nuevo evento de una fecha en una lista de eventos.
registrarEvento :: [Evento] -> (Day,Month,Year) -> String -> [Evento]
registrarEvento e (d,m,y) des = concat [antes,durante',despues]
  where (antes,durante,despues) = daysEvents e (d,m,y)
        durante' = durante ++ [ne]
        ne = Evento {
               year        = y,
               month       = m,
               eday        = d,
               nth         = length durante,
               description = des
             }

-- |@promptRegistrarEvento@ presenta el prompt para registrar un evento, luego de 
--  que el usuario haya metido la descripción del evento a registrar, se 
--  hace el registro y se continúa con la ejecución del programa.
promptRegistrarEvento :: [Evento] -> (Day,Month,Year) -> IO ()
promptRegistrarEvento e (d,m,y) =
  do hSetBuffering stdin LineBuffering
     putStr "Introduzca la descripción del evento a registrar: "
     des <- getLine
     putStrLn ""
     hSetBuffering stdin NoBuffering
     let e' = registrarEvento e (d,m,y) des
     clrScr
     calAndEvents e' (d,m,y) (nDays m y)
     prompt e' (d,m,y)

-- |@eliminarEvento@ elimina un evento de una fecha según su orden (campo @nth@).
eliminarEvento :: [Evento] -> (Day,Month,Year) -> Int -> [Evento]
eliminarEvento e (d,m,y) n = concat [antes,durante',despues]
  where (a,b) = break ((n-1==) . nth) $ durante
        (antes,durante,despues) = daysEvents e (d,m,y)
        durante' = if not $ null b
                     then a ++ map bajarOrden (tail b)
                     else a
        bajarOrden x = Evento {
                         year        = year x,
                         month       = month x,
                         eday        = eday x,
                         nth         = (nth x) - 1,
                         description = description x
                       }

-- |@promptEliminarEvento@ presenta el prompt para eliminar un evento, luego de 
--  que el usuario haya metido el número de evento a eliminar, se elimina el 
--  evento y se continúa con la ejecución del programa.
promptEliminarEvento :: [Evento] -> (Day,Month,Year) -> IO ()
promptEliminarEvento e (d,m,y) =
  do hSetBuffering stdin LineBuffering
     putStr "Número del evento a eliminar: "
     n <- getLine
     putStrLn ""
     let enumber = read n
     hSetBuffering stdin NoBuffering
     let e' = eliminarEvento e (d,m,y) enumber
     clrScr
     calAndEvents e' (d,m,y) (nDays m y)
     prompt e' (d,m,y)

-- |@printHelp@ imprime un mensaje con las operaciones que se pueden realizar.
printHelp :: [Evento] -> (Day,Month,Year) -> IO ()
printHelp e (d,m,y) = do
  clrScr
  putStrLn "Uso:"
  putStrLn "    - Presione 'j' para avanzar un día."
  putStrLn "    - Presione 'k' para retroceder un día."
  putStrLn "    - Presione 'l' para avanzar un mes."
  putStrLn "    - Presione 'h' para retroceder un mes."
  putStrLn "    - Presione 'q' para salir del programa."
  putStrLn "    - Presione cualquier otra tecla para mostrar este mensaje.\n"
  prompt e (d,m,y)

-- |@quit@ guarda los eventos en el archivo __hagenda.txt__, limpia la pantalla 
--  y termina el programa.
quit :: [Evento] -> IO ()
quit e = do saveEvents "hagenda.txt" e
            clrScr

{-----------------------------------------------------------------------------}

-- |@prompt@ presenta un prompt con la fecha que se está revisando y espera el 
--  comando del usuario.
prompt :: [Evento] -> (Day,Month,Year) -> IO ()
prompt e (d,m,y) = do
  putStr $ show y ++ " " ++ show m ++ " " ++ show d ++ "> "
  c <- getChar
  putChar '\n'
  
  case c of
    'j' -> nextDay e (d,m,y)
    'k' -> previousDay e (d,m,y)
    'l' -> nextMonth e (d,m,y)
    'h' -> previousMonth e (d,m,y)
    'r' -> promptRegistrarEvento e (d,m,y)
    'd' -> promptEliminarEvento e (d,m,y)
    'q' -> quit e
    _   -> printHelp e (d,m,y)

-- |@main@ es la función principal.
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  e <- catch (loadEvents "hagenda.txt")
             (\err -> if isDoesNotExistError err
                        then return []
                        else ioError err)
  (d,m,y) <- getCurrentDate
  clrScr
  calAndEvents e (d,m,y) (nDays m y)
  prompt e (d,m,y)


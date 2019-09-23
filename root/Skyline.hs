--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Cabecera del programa Skyline.hs
-- Práctica de Teoría de los Lenguajes de Programación
-- Curso 2015-2016
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
module Skyline where
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
   -- Tipos de datos
   type Edificio   = (Int, Int, Int)
   type Coordenada = (Int, Int)
   type Skyline    = [Coordenada]
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
   -- Función principal de la práctica. Convierte una lista de edificios en la lista de coordenadas que representa su skyline
   resuelveSkyline :: [Edificio] -> Skyline
   resuelveSkyline (x:xs)
                 | length xs == 0 = edificioAskyline x
                 | otherwise      = let
                                         tupla = divide (x:xs)
                                    in
                                         combina (resuelveSkyline (fst tupla)) (resuelveSkyline (snd tupla))
   resuelveSkyline [] = []
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
   -- Toma un único edificio como entrada y devuelve su skyline
   edificioAskyline :: Edificio -> Skyline
   edificioAskyline (a,b,c) = [(a,c),(b,0)]
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
   -- Toma una lista de edificios como entrada y la divide en dos partes iguales, o a lo sumo con una diferencia de un edificio
   divide :: [Edificio] -> ([Edificio],[Edificio])
   divide xs = let
                    mitad = div (length xs) 2
               in
                    (take mitad xs , drop mitad xs)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
   -- Toma dos Skyline como entrada y devuelve el Skyline resultado de combinarlos
   combina :: Skyline -> Skyline -> Skyline
   combina xs ys = combina2 xs ys 0 0 0
           where combina2 xs ys hx hy h -- combina2 :: Skyline -> Skyline -> Int -> Int -> Int -> Skyline
                          -- skyline 'ys' consumido
                          | (0 == length ys)               = xs -- CASO1: caso base 1
                          -- skyline 'xs' consumido
                          | (0 == length xs)               = ys -- CASO2: caso base 2
                          -- coordenada 'x' del primer skyline es menor
                          | fst (head xs) < fst (head ys)  = let 
                                                                 altura = max (snd (head xs)) hy
                                                                 descartar = altura == h
                                                             in
                                                                 if descartar then -- CASO 3: se reduce 'xs' sin añadir nueva coordenada
                                                                    combina2 (tail xs) ys (snd (head xs)) hy h
                                                                 else -- CASO 4: se añade nueva coordenada y se reduce 'xs'
                                                                    (fst (head xs) , altura):(combina2 (tail xs) ys (snd (head xs)) hy altura)
                          -- coordenada 'x' del segundo skyline es menor
                          | fst (head xs) > fst (head ys)  = let
                                                                 altura = max (snd (head ys)) hx
                                                                 descartar = altura == h
                                                             in
                                                                 if descartar then -- CASO 5: se reduce 'ys' sin añadir nueva coordenada
                                                                    combina2 xs (tail ys) hx (snd (head ys)) h
                                                                 else -- CASO 6: se añade nueva coordenada y se reduce 'ys'
                                                                    (fst (head ys) , altura):(combina2 xs (tail ys) hx (snd (head ys)) altura)
                          -- coordenadas 'x' iguales
                          | otherwise                      = let
                                                                 altura = max (snd (head xs)) (snd (head ys))
                                                                 descartar = altura == h
                                                             in
                                                                 if descartar then -- CASO 7: se reducen 'xs' e 'ys' sin añadir nueva coordenada
                                                                    combina2 (tail xs) (tail ys) (snd (head xs)) (snd (head ys)) h
                                                                 else -- CASO 8: se añade nueva coordenada y se reducen 'xs' e 'ys'
                                                                    (fst (head xs) , altura):(combina2 (tail xs) (tail ys) (snd (head xs)) (snd (head ys)) altura)
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
   -- Toma un skyline como entrada y devuelve la cadena de caracteres de su dibujo
   dibujaSkyline :: Skyline -> [Char]
   dibujaSkyline []  = []
   dibujaSkyline cs = let
                            listaAlturas = calculaAlturas cs 0 0
                            altura       = maximum listaAlturas
                       in
                            escribe listaAlturas altura 0
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
   -- Función auxiliar que obtiene la lista de alturas de un skyline
   calculaAlturas :: Skyline -> Int -> Int -> [Int]
   calculaAlturas (c:cs) x h
                  | x == (fst c) = (snd c):(calculaAlturas cs (x + 1) (snd c))
                  | otherwise    = h:(calculaAlturas (c:cs) (x + 1) h)
   calculaAlturas [] _ _  = []
--------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------

   -- Función auxiliar que obtiene la cadena de caracteres skyline asociada a una lista de alturas
   escribe :: [Int] -> Int -> Int -> [Char]
   escribe lista h x
           | (x == length lista) && (h == 0) = []
           | x == length lista               = ('\n'):(escribe lista (h - 1) 0)
           | h == 0                          = ('-'):(escribe lista h (x + 1))
           | otherwise                       = if h > (lista !! x) then
                                                    (' '):(escribe lista h (x + 1))
                                               else
                                                    ('*'):(escribe lista h (x + 1))
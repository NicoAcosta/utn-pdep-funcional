module Library where
--import PdePreludat


-- En haskell existen los alias de tipos, que permiten darle un poco mas de expresividad al codigo.
-- Estos alias se definen con la palabra "type", por ejemplo:

type Libro = (String, String, Int)

-- Como es un tipo de dato, siempre debe ir con la primer letra mayuscula.
-- Ahora podes usar ese tipo en todas las firmas de tus funciones!


-- Cuidado con estos tipos de la tupla! Cuando pones 'a' y 'b' sin ninguna restriccion, estas dejando
-- que venga cualquier tipo de dato y por mas que ignores los valores solo la tupla (String, String, Int)
-- deberia tipar en cada funcion

titulo :: (String, a, b) -> String
titulo (titulo, _, _) = titulo

autor :: (a, String, b) -> String
autor (_, autor, _) = autor

paginas :: (a, b, Number) -> Number
paginas (_, _, paginas) = paginas


promedioDeHojas :: [(String, String, Number)] -> Number 
promedioDeHojas biblioteca = ((/(length biblioteca)).sum.(map paginas)) biblioteca
-- Cuando le pongas bien los tipos posiblemente se te complique al usar (/) porque exige Fractional
-- Y length devuelve un entero
-- En ese caso podes usar la funcion "div" que es la division entera para evitar usar otras funciones
-- O tener que convertir tipos

lecturaObligatoria :: (String, String, Number) -> Bool
lecturaObligatoria libro =
    autor libro == "Stephen King" ||
    take 6 (titulo libro) == "Eragon" || -- Eragon es uno de los libros de la saga, hay mas
    (titulo libro == "Fundación" && autor libro == "Isaac Asimov" && paginas libro == 230)

-- Podrias aprovechas el Pattern Matching asi te queda un poquito mas declarativo y no tenes la necesidad
-- de usar las funciones para acceder a cada valor de la tupla


esPaoliniOGaiman :: (String, String, Number) -> Bool
esPaoliniOGaiman libro = autor libro == "Christopher Paolini" || autor libro == "Neil Gaiman"
-- Nuevamente podrias usar pattern matching, en este caso para evitar la repeticion de "autor libro"

fantasiosa :: [(String, String, Number)] -> Bool
fantasiosa = any esPaoliniOGaiman


esVocal :: Char -> Bool
esVocal l = (l == 'a') || (l == 'e') || (l == 'i') || (l == 'o') || (l == 'u') ||
            (l == 'A') || (l == 'E') || (l == 'I') || (l == 'O') || (l == 'U')
-- Podrias tener una lista con las vocales y usar la funcion elem para ver si una letra es vocal o no
-- De esa forma es menos tediosa la funcion.
-- "l" no es muy expresivo

nombreDeLaBiblioteca :: [(String, String, Number)] -> String
nombreDeLaBiblioteca = (filter (not.esVocal)).concat.(map titulo)
-- Existe la funcion "concatMap" pero esta perfecto igual!

bibliotecaLigera :: [(String, String, Number)] -> Bool
bibliotecaLigera = all ((40>=).paginas)

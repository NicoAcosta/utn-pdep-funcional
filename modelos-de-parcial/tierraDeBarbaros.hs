-- https://docs.google.com/document/d/1ZJLz84JEPoEWZ9aVCJRWoHomfscUc04yECqSMOvdVZU/

import Data.Char
import Text.Show.Functions

--
--  PUNTO 1
--

type Nombre = String
type Fuerza = Int
type Habilidad = String
type Habilidades = [Habilidad]
type Objeto = (Barbaro -> Barbaro)
type Objetos = [Objeto]

data Barbaro = Barbaro {
    nombre :: Nombre,
    fuerza :: Fuerza,
    habilidades :: Habilidades,
    objetos :: Objetos
} deriving Show

dave :: Barbaro
dave = Barbaro {
    nombre = "Dave",
    fuerza = 100,
    habilidades = ["tejer","escribirPoesia","tejer"],
    objetos = [ardilla]
}

cambiarNombre :: Nombre -> Barbaro -> Barbaro
cambiarNombre unNombre unBarbaro = unBarbaro {
    nombre = unNombre
}

cambiarFuerza :: (Int -> Int) -> Barbaro -> Barbaro
cambiarFuerza unaFuncion unBarbaro = unBarbaro {
    fuerza = unaFuncion (fuerza unBarbaro)
}

agregarHabilidad :: Habilidad -> Barbaro -> Barbaro
agregarHabilidad unaHabilidad unBarbaro = unBarbaro {
    habilidades = habilidades unBarbaro ++ [unaHabilidad]
}

agregarObjeto :: Objeto -> Barbaro -> Barbaro
agregarObjeto unObjeto unBarbaro = unBarbaro {
    objetos = objetos unBarbaro ++ [unObjeto]
}


espadas :: Int -> Barbaro -> Barbaro
espadas unPeso unBarbaro = cambiarFuerza (+ (2*unPeso) ) unBarbaro

amuletosMisticos :: Habilidad -> Barbaro -> Barbaro
amuletosMisticos unaHabilidad unBarbaro = agregarHabilidad unaHabilidad unBarbaro

eliminarElRestoDeLosObjetos :: Objeto -> Barbaro -> Barbaro
eliminarElRestoDeLosObjetos unObjeto unBarbaro = unBarbaro {
    objetos = [unObjeto]
}

varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = ( agregarHabilidad "hacerMagia" . eliminarElRestoDeLosObjetos varitasDefectuosas ) unBarbaro

ardilla :: Objeto
ardilla unBarbaro = unBarbaro

cuerda :: Objeto -> Objeto -> Objeto
cuerda unObjeto otroObjeto = unObjeto.otroObjeto


--
--  PUNTO 2
--

concatenarYMayusculas :: [String] -> [String]
concatenarYMayusculas listaDeStrings = [ (map toUpper.concat) listaDeStrings ]
--  concatenarYMayusculas listaDeStrings = [ concatMap (map toUpper) listaDeStrings ]

megafono :: Objeto
megafono unBarbaro = unBarbaro {
    habilidades = concatenarYMayusculas (habilidades unBarbaro)
}

megafonoBarbarico :: Objeto
megafonoBarbarico unBarbaro = (cuerda ardilla megafono) unBarbaro


--
--  PUNTO 3
--

type Aventura = (Barbaro -> Bool)

tieneHabilidad :: Habilidad -> Barbaro -> Bool
tieneHabilidad unaHabilidad unBarbaro = elem unaHabilidad (habilidades unBarbaro)

invasionDeDuendes :: Aventura
invasionDeDuendes unBarbaro = tieneHabilidad "Escribir Poesía Atroz" unBarbaro 

tienePulgares :: Aventura
tienePulgares unBarbaro = nombre unBarbaro == "Faffy" || nombre unBarbaro == "Astro"

cremalleraDelTiempo :: Aventura
cremalleraDelTiempo unBarbaro = tienePulgares unBarbaro


saqueo :: Aventura
saqueo unBarbaro = tieneHabilidad "robar" unBarbaro && fuerza unBarbaro > 80

poderDeGrito :: Barbaro -> Int
poderDeGrito unBarbaro = ((*4).length.objetos) unBarbaro

cantidadDeLetrasDeHabilidades :: Barbaro -> Int
cantidadDeLetrasDeHabilidades unBarbaro = (sum.map length.habilidades) unBarbaro

gritoDeGuerra :: Aventura
gritoDeGuerra unBarbaro = poderDeGrito unBarbaro >= cantidadDeLetrasDeHabilidades unBarbaro

esVocal :: Char -> Bool
esVocal caracter =  caracter == 'a' || caracter == 'A' ||
                    caracter == 'e' || caracter == 'E' ||
                    caracter == 'i' || caracter == 'I' ||
                    caracter == 'o' || caracter == 'O' ||
                    caracter == 'u' || caracter == 'U'

contiene3OMasVocales :: Habilidad -> Bool
contiene3OMasVocales unaHabilidad = ( (>=3) . length . filter esVocal ) unaHabilidad

empiezaConMayuscula :: Habilidad -> Bool
empiezaConMayuscula unaHabilidad = isUpper (head unaHabilidad)

caligrafia :: Aventura
caligrafia unBarbaro =  all (contiene3OMasVocales)  (habilidades unBarbaro) &&
                        all (empiezaConMayuscula)   (habilidades unBarbaro)

ritualDeFechorias :: Aventura
ritualDeFechorias unBarbaro =   saqueo unBarbaro ||
                                gritoDeGuerra unBarbaro ||
                                caligrafia unBarbaro



sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes listaDeBarbaros unaAventura = filter unaAventura listaDeBarbaros



--
--  PUNTO 4
--

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos lista 
    |   lista == []                                 =   []
    |   elem        (head lista)    (tail lista)    =                       sinRepetidos    (tail lista)   
    |   otherwise                                   =   [head lista]    ++  sinRepetidos    (tail lista)

habilidadesSinRepetidos :: Barbaro -> Barbaro
habilidadesSinRepetidos unBarbaro = unBarbaro {
    habilidades = (sinRepetidos.habilidades) unBarbaro
}


nombreDescendiente :: Barbaro -> Barbaro
nombreDescendiente unBarbaro = cambiarNombre ( ((++ "*").nombre) unBarbaro ) unBarbaro

utilizarObjetos :: Barbaro -> Barbaro
utilizarObjetos unBarbaro = foldr ($) unBarbaro (objetos unBarbaro)

descendiente :: Barbaro -> Barbaro
descendiente unBarbaro = (utilizarObjetos.habilidadesSinRepetidos.nombreDescendiente) unBarbaro

descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = [descendiente unBarbaro] ++ descendientes (descendiente unBarbaro)


{-

Para poder aplicar sinRepetidos a un una lista los elementos
de dicha lista deben pertenecer a un tipo de dato que pertenezca
a la clase Eq, para poder ver si hay otro elemento igual.

Por lo tanto, no se podría aplicar a la lista de objetos ya que
las funciones no pertenecen a la clase Eq.

Sí se podría aplicar al nombre ya que String == [Char]
El tipo Char pertenece a Eq, por lo tanto se eliminarían
los caracteres dentro del nombre que se repiten en el nombre

-}

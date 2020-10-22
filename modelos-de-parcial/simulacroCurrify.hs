-- https://docs.google.com/document/u/0/d/1f5nzJo2HO2K-yD8RVPx3Ftbf4LHYdFCwyd12bH50EJU/mobilebasic

import Text.Show.Functions()

-----
----- Parte A
-----

type Titulo = String
type Genero = String
type Duracion = Int

data Cancion = Cancion {
    titulo :: Titulo,
    genero :: Genero,
    duracion :: Duracion
} deriving (Show, Eq)

type Nombre = String
type Efecto = Cancion -> Cancion

data Artista = Artista {
    nombre :: Nombre,
    canciones :: [Cancion],
    efectoPreferido :: Efecto
} deriving Show

type ModificacionCancion = Cancion -> Cancion

minutos :: Float -> Duracion
minutos = round . (* 60)


cambiarDuracion :: (Duracion -> Duracion) -> ModificacionCancion
cambiarDuracion unaFuncion unaCancion = unaCancion {
    duracion = (unaFuncion.duracion) unaCancion
}

definirDuracion :: Duracion -> ModificacionCancion
definirDuracion unaDuracion unaCancion = unaCancion {
    duracion = unaDuracion
}

cambiarTitulo :: (Titulo -> Titulo) -> ModificacionCancion
cambiarTitulo unaFuncion unaCancion = unaCancion {
    titulo = (unaFuncion.titulo) unaCancion
}

cambiarGenero :: (Genero -> Genero) -> ModificacionCancion
cambiarGenero unaFuncion unaCancion = unaCancion {
    genero = (unaFuncion.genero) unaCancion
}

definirGenero :: Genero -> ModificacionCancion
definirGenero unGenero unaCancion = unaCancion {
    genero = unGenero
}


unMinutoMenosMayorOIgualACero :: Duracion -> Duracion
unMinutoMenosMayorOIgualACero = (max 0) . ( (-) 60)

acortar :: Efecto
acortar = cambiarDuracion unMinutoMenosMayorOIgualACero

remixar :: Efecto
remixar = cambiarTitulo (++ " Remix") . cambiarDuracion (*2) . definirGenero "remixado"

acustizar :: Duracion -> Efecto
acustizar unaDuracion unaCancion
    |   genero unaCancion /= "acústico" =   ( definirDuracion unaDuracion . definirGenero "acústico" ) unaCancion
    |   otherwise                       =   unaCancion

metaEfecto :: [Efecto] -> ModificacionCancion
metaEfecto unosEfectos unaCancion = foldr ($) unaCancion unosEfectos



cafeParaDos :: Cancion
cafeParaDos = Cancion {
    titulo = "Café para dos",
    genero = "rock melancólico",
    duracion = 146
}

fuiHastaAhi :: Cancion
fuiHastaAhi = Cancion {
    titulo = "Fuí hasta ahí",
    genero = "rock",
    duracion = 279
}

losEscarabajos :: Artista
losEscarabajos = Artista {
    nombre = "Los escarabajos",
    canciones = [], --[rocketRaccoon, mientrasMiBateriaFesteja, tomateDeMadera] --
    efectoPreferido = acortar
}

adela :: Artista
adela = Artista {
    nombre = "Adela",
    canciones = [], -- [teAcordas, unPibeComoVos, daleMechaALaLluvia]
    efectoPreferido = remixar
}

elTigreJoaco :: Artista
elTigreJoaco = Artista {
    nombre = "El tigre Joaco",
    canciones = [],
    efectoPreferido = acustizar (minutos 6)
}




-----
----- Parte B
-----

esCancionCorta :: Cancion -> Bool
esCancionCorta = (< minutos 2.5) . duracion

cancionesCortas :: Artista -> [Cancion]
cancionesCortas unArtista = filter esCancionCorta ( canciones unArtista )

vistazo :: Artista -> [Cancion]
vistazo = take 3 . cancionesCortas


esGenero :: Genero -> Cancion -> Bool
esGenero unGenero = ( == unGenero ) . genero

filterGenero :: Genero -> [Cancion] -> [Cancion]
filterGenero unGenero unasCanciones = filter (esGenero unGenero) unasCanciones

playlist :: Genero -> [Artista] -> [Cancion]
playlist unGenero = filter (esGenero unGenero) . concatMap canciones




-----
----- Parte C
-----


type ModificacionArtista = Artista -> Artista

modificarCanciones :: ModificacionCancion -> ModificacionArtista
modificarCanciones unaFuncion unArtista = unArtista {
    canciones = ( map unaFuncion . canciones ) unArtista
}

hacerseDJ :: ModificacionArtista
hacerseDJ unArtista = modificarCanciones ( efectoPreferido unArtista ) unArtista

todosIguales :: Eq a => [a] -> Bool
todosIguales unaLista = all ( == head unaLista ) unaLista

tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo = todosIguales . canciones

componerEfectos :: [Artista] -> Efecto
componerEfectos unosArtistas = foldr (.) id (map efectoPreferido unosArtistas)

formarBanda :: Nombre -> [Artista] -> Artista
formarBanda unNombre unosArtistas = Artista {
    nombre = unNombre,
    canciones = concatMap canciones unosArtistas,
    efectoPreferido = componerEfectos unosArtistas
}

algunoEsDeGenero :: Genero -> Genero -> Genero -> Bool
algunoEsDeGenero generoBuscado unGenero otroGenero = unGenero == generoBuscado || otroGenero == generoBuscado

elQueNoEsReaggeton :: Genero -> Genero -> Genero
elQueNoEsReaggeton unGenero otroGenero
    |   unGenero == "reggaeton" =   otroGenero
    |   otherwise               =   unGenero

generoMasLargo :: Genero -> Genero -> Genero
generoMasLargo unGenero otroGenero
    |   length unGenero > length otroGenero =   unGenero
    |   otherwise                           =   otroGenero

generoSuperador' :: Genero -> Genero -> Genero
generoSuperador' unGenero otroGenero
    |   algunoEsDeGenero "rock" unGenero otroGenero         =   "rock"
    |   algunoEsDeGenero "reggaeton" unGenero otroGenero    =   elQueNoEsReaggeton unGenero otroGenero
    |   otherwise                                           =   generoMasLargo unGenero otroGenero

generos :: Artista -> [Genero]
generos = map genero . canciones

generoSuperador :: [Genero] -> Genero
generoSuperador unosGeneros = foldr (generoSuperador') "reggaeton" unosGeneros

generoOMP :: Artista -> Genero
generoOMP = (++ " progresivo") . generoSuperador . generos

titulosConcatenados :: Artista -> Titulo
titulosConcatenados = concat . map titulo . canciones

sumaDuracionCanciones :: Artista -> Int
sumaDuracionCanciones = sum.map duracion.canciones

obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva unArtista = Cancion {
    titulo = titulosConcatenados unArtista,
    genero = generoOMP unArtista,
    duracion = sumaDuracionCanciones unArtista
}




-----
----- Parte C
-----

cancionesInfinitas :: [Cancion]
cancionesInfinitas = cafeParaDos:cancionesInfinitas

soda :: Artista
soda = Artista {
    nombre = "Soda Stereo",
    canciones = cancionesInfinitas,
    efectoPreferido = id
}



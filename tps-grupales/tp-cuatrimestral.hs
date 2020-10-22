import Text.Show.Functions()


type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
 
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Int,
 temperaturaAgua :: Int,
 ultimoArreglo :: Fecha
} deriving Show

-----
-----

type Modificacion = Auto -> Auto
type Temperatura = Int
type Rpm = Int

cambiarRpm :: Rpm -> Modificacion
cambiarRpm unasRpm unAuto = unAuto {
    rpm = unasRpm
}

cambiarTemperatura :: Temperatura -> Modificacion
cambiarTemperatura unaTemperatura unAuto = unAuto {
    temperaturaAgua = unaTemperatura
}


-----
-----

-- Casos de prueba

autoEjemplo :: Auto
autoEjemplo = Auto {
    patente = "DFH029",
    desgasteLlantas = [0.1, 0.4, 0.2, 0.1],
    rpm = 1500,
    temperaturaAgua = 90,
    ultimoArreglo = (1,1,2020)
}

autoEjemplo2 :: Auto
autoEjemplo2 = Auto {
    patente = "DRH029",
    desgasteLlantas = [0.3, 0.5, 0.6, 0.1],
    rpm = 1500,
    temperaturaAgua = 75,
    ultimoArreglo = (1,1,2020)
}

autoEjemplo3 :: Auto
autoEjemplo3 = Auto {
    patente = "DRH129",
    desgasteLlantas =   [0.1, 0.1, 0.1, 0],
    rpm = 1500,
    temperaturaAgua = 75,
    ultimoArreglo = (1,6,2010)
}


-----   
-----   PUNTO 1 delegar 2 condicones por un lado  y el calculo de 3mil mas declarativo y expresivo
-----


largoDePatente :: Auto -> Int
largoDePatente = length.patente

compararPatente :: (String -> Bool) -> Auto -> Bool
compararPatente condicion = condicion.patente

calculoPatental :: Auto -> Int
calculoPatental unAuto
    |   (last.patente) unAuto == '4'    =   ( (* 3000) . largoDePatente ) unAuto
    |   otherwise                       =   20000

correspondeCalculoPatental :: Auto -> Bool
correspondeCalculoPatental unAuto = compararPatente (>"DJ") unAuto && compararPatente (<"NC") unAuto

costoDeReparacion :: Auto -> Int
costoDeReparacion unAuto
    |   largoDePatente unAuto == 7          =   12500
    |   correspondeCalculoPatental unAuto   =   calculoPatental unAuto
    |   otherwise                           =   15000



-----   
-----   PUNTO 2 Bien
-----

--- Parte 1

desgastePrimeraLlanta :: Auto -> Float
desgastePrimeraLlanta = head.desgasteLlantas 

esAutoPeligroso :: Auto -> Bool
esAutoPeligroso  = (>0.5).desgastePrimeraLlanta


--- Parte 2

cuandoSeArreglo :: Auto -> Int
cuandoSeArreglo = anio.ultimoArreglo

necesitaRevision :: Auto -> Bool
necesitaRevision = (<=2015).cuandoSeArreglo


-----   
-----   PUNTO 3  
-----


--- Parte 1

type Tecnico = Modificacion --tecnico = modificacion? / 


alfa :: Tecnico
alfa unAuto
    |   ( (>2000) . rpm ) unAuto    =   cambiarRpm 2000 unAuto -- funcion min podemos evitar guarda
    |   otherwise                   =   unAuto

cambiarCubiertas :: Modificacion -- cambiarCubiertas poner otro nombre mas especifico
cambiarCubiertas unAuto = unAuto {  -- hacer una funcion mas general para el cambio del data
    desgasteLlantas = [0,0,0,0]
}

bravo :: Tecnico
bravo = cambiarCubiertas

charly :: Tecnico
charly = alfa.bravo


--- Parte 2

tango :: Tecnico
tango = id

anteultimo :: [a] -> a
anteultimo = last.init

primerosDosCero :: Num a => [a] -> [a]  -- con take/drop puede ser mas declarativo 
primerosDosCero (_:_:xs) = (0:0:xs)
primerosDosCero [] = [0,0,0,0]
primerosDosCero [_] = [0,0,0,0]

cambiarCubiertasDelanteras :: Modificacion  -- repetimos logica para cambiar el desgaste del data, hacer una funcion mas general
cambiarCubiertasDelanteras unAuto = unAuto {
    desgasteLlantas = (primerosDosCero.desgasteLlantas) unAuto
} 

lima :: Tecnico
lima = cambiarCubiertasDelanteras

zulu :: Tecnico
zulu = lima . cambiarTemperatura 90



-----   
-----   PUNTO 4  --Bien
-----

cantidadDeDesgaste :: Auto -> Int
cantidadDeDesgaste = round.(*10).sum.desgasteLlantas 

desgasteImpar :: Auto -> Bool
desgasteImpar = odd.cantidadDeDesgaste

desgastePar :: Auto -> Bool
desgastePar = not.desgasteImpar

ordenadoToc :: [Auto] -> Bool
ordenadoToc [] = True
ordenadoToc [x] = desgasteImpar x
ordenadoToc (x:y:xs) = desgasteImpar x && desgastePar y && ordenadoToc xs



-----   
-----   PUNTO 5 armar una tupla con tecnico fecha de modificacion para no repetir logica
-----

aplicarTrabajoDeTecnicos :: [Tecnico] -> Modificacion
aplicarTrabajoDeTecnicos tecnicos unAuto = foldr ($) unAuto tecnicos

actualizarUltimoArreglo :: Fecha -> Modificacion
actualizarUltimoArreglo unaFecha unAuto = unAuto {
    ultimoArreglo = unaFecha
}

ordenDeReparacion :: Fecha -> [Tecnico] -> Modificacion
ordenDeReparacion unaFecha unosTecnicos =
    actualizarUltimoArreglo unaFecha . aplicarTrabajoDeTecnicos unosTecnicos



-----   
-----   PUNTO 6
-----

--- Parte 1

listaDeTecnicosEjemplo :: [Tecnico]
listaDeTecnicosEjemplo = [alfa, bravo, charly, tango, zulu, lima]

estaEnCondiciones :: Auto -> Bool
estaEnCondiciones = not . esAutoPeligroso

tecnicosQueDejanEnCondiciones :: [Tecnico] -> Auto -> [Tecnico]
tecnicosQueDejanEnCondiciones tecnicos unAuto =
    filter (estaEnCondiciones . ( $ unAuto) ) tecnicos


--- Parte 2

autosQueNecesitanReparacion :: [Auto] -> [Auto]
autosQueNecesitanReparacion unosAutos = filter necesitaRevision unosAutos

costosDeReparacion :: [Auto] -> [Int]
costosDeReparacion = map costoDeReparacion . autosQueNecesitanReparacion

sumaDeCostosDeReparacion :: [Auto] -> Int
sumaDeCostosDeReparacion = sum . costosDeReparacion



-----   
-----   PUNTO 7
-----

--- Parte 1

tecnicosInfinitos :: [Tecnico]
tecnicosInfinitos = zulu:tecnicosInfinitos

tecnicosInfinitos' :: [Tecnico]
tecnicosInfinitos' = listaDeTecnicosEjemplo ++ tecnicosInfinitos'

primerTecnicoQueDejaEnCondiciones :: Auto -> [Tecnico] -> Tecnico -- hacer composicion con la funcion de filter que hicimos atras
primerTecnicoQueDejaEnCondiciones unAuto unosTecnicos = head (filter (estaEnCondiciones . ( $ unAuto) ) unosTecnicos)

{-
Sí se puede obtener el primer técnico que deja el auto en condiciones.
Al ejecutar
primerTecnicoQueDejaEnCondiciones autoEjemplo tecnicosInfinitos
devuelve un técnico
Esto se debe a que Haskell trabaja con lazy evaluation que, al basarse en
call-by-name, aplica las funciones antes de que se evalúen los parámetros.
Por lo tanto, busca únicamente el primer (head) elemento que cumpla con
las condiciones. No evalúa toda la lista.
-}
-- es cierto lo que pusimos siempre y cuando hay algun elemento en la lista que cumpla la condicion

--- Parte 2

autosInfinitos :: [Auto]
autosInfinitos = autosInfinitos' 0
 
autosInfinitos' :: Int -> [Auto]
autosInfinitos' n = Auto {
 patente = "AAA000",
 desgasteLlantas = [(fromIntegral n)/10, 0, 0, 0.3],
 rpm = 1500 + n,
 temperaturaAgua = 90,
 ultimoArreglo = (20, 1, 2013)
} : autosInfinitos' (n + 1)

costosPrimerosTres :: [Auto] -> [Int]
costosPrimerosTres = take 3 . costosDeReparacion

sumaCostosPrimerosTres :: [Auto] -> Int
sumaCostosPrimerosTres = sum . costosPrimerosTres

{-
La expresión
costosDeReparacion autosInfinitos
toma una lista de autos (en este caso infinita) y devuelve una lista (en este caso infinita),
por lo tanto, no puede terminar de evaluarla.
La versión
costosPrimerosTres autosInfinitos
al aplicar primero (take 3) antes que (costosDeReparacion) 
evalua la lista infinita hasta encontrar los primeros tres autos que cumplan las condición de autosQueNecesitanReparacion
Por lo tanto, termina de evaluarla.
-}
-- es cierto lo que pusimos siempre y cuando hay algun elemento en la lista que cumpla la condicion
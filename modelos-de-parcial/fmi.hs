type IngresoPerCapita = Float
type Poblacion = Int
type RecursoNatural = String
type Deuda = Float

data Pais = Pais {
    ingresoPerCapita :: IngresoPerCapita,
    poblacionActivaSectorPublico :: Poblacion,
    poblacionActivaSectorPrivado :: Poblacion,
    recursosNaturales :: [RecursoNatural],
    deuda :: Deuda
}

type Modificacion = Pais -> Pais

modificarDeuda :: (Deuda -> Deuda) -> Modificacion
modificarDeuda funcionAAplicar unPais = unPais {
    deuda = (funcionAAplicar.deuda) unPais
}

modificarPoblacionActivaSectorPublico :: (Poblacion -> Poblacion) -> Modificacion
modificarPoblacionActivaSectorPublico funcionAAplicar unPais = unPais {
    poblacionActivaSectorPublico = (funcionAAplicar.poblacionActivaSectorPublico) unPais
}

modificarPoblacionActivaSectorPrivado :: (Poblacion -> Poblacion) -> Modificacion
modificarPoblacionActivaSectorPrivado funcionAAplicar unPais = unPais {
    poblacionActivaSectorPrivado = (funcionAAplicar.poblacionActivaSectorPrivado) unPais
}

modificarIngresoPerCapita :: (IngresoPerCapita -> IngresoPerCapita) -> Modificacion
modificarIngresoPerCapita funcionAAplicar unPais = unPais {
    ingresoPerCapita = (funcionAAplicar.ingresoPerCapita) unPais
}

modificarRecursosNaturales :: ([RecursoNatural] -> [RecursoNatural]) -> Modificacion
modificarRecursosNaturales funcionAAplicar unPais = unPais {
    recursosNaturales = (funcionAAplicar.recursosNaturales) unPais
}



type Estrategia = Pais -> Pais

endeudamiento :: Int -> Deuda
endeudamiento cantidadDeMillones = ((* 1.5).(*1000000).fromIntegral) cantidadDeMillones

prestarMillones :: Int -> Estrategia
prestarMillones cantidadDeMillones = modificarDeuda (+ (endeudamiento cantidadDeMillones))


reducirPuestosSectorPublico :: Poblacion -> Estrategia
reducirPuestosSectorPublico cantidadDePuestos
    |   cantidadDePuestos > 100     =   modificarIngresoPerCapita (* 0.8)   . modificarPoblacionActivaSectorPublico ((-) cantidadDePuestos)
    |   otherwise                   =   modificarIngresoPerCapita (* 0.85)  . modificarPoblacionActivaSectorPublico ((-) cantidadDePuestos)



quitarRecursoNatural :: RecursoNatural -> [RecursoNatural] -> [RecursoNatural]
quitarRecursoNatural unRecursoNatural unosRecursosNaturales = filter (/= unRecursoNatural) unosRecursosNaturales

darRecursoNatural :: RecursoNatural -> Estrategia
darRecursoNatural unRecurso = modificarDeuda ((-) 2000000)    .   modificarRecursosNaturales (quitarRecursoNatural unRecurso)


poblacionActivaTotal :: Pais -> Int
poblacionActivaTotal unPais = poblacionActivaSectorPrivado unPais + poblacionActivaSectorPublico unPais

pbi :: Pais -> Float
pbi unPais = ((* ingresoPerCapita unPais).fromIntegral.poblacionActivaTotal) unPais


blindaje :: Estrategia
blindaje unPais = (modificarDeuda (+ (pbi unPais)) . modificarPoblacionActivaSectorPublico ((-) 500) ) unPais



namibia :: Pais
namibia = Pais {
    ingresoPerCapita = 4140,
    poblacionActivaSectorPublico = 400000,
    poblacionActivaSectorPrivado = 650000,
    recursosNaturales = ["Minería", "Ecoturismo"],
    deuda = 50000000
}


type Receta = Pais -> Pais

recetaA :: Receta
recetaA = prestarMillones 200 . darRecursoNatural "Minería"

namibia' :: Pais
namibia' = recetaA namibia


tieneRecursoNatural :: RecursoNatural -> Pais -> Bool
tieneRecursoNatural unRecursoNatural unPais = elem unRecursoNatural (recursosNaturales unPais)

puedeZafar :: Pais -> Bool
puedeZafar = tieneRecursoNatural "Petróleo"

puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter puedeZafar


totalDeDeuda :: [Pais] -> Deuda
totalDeDeuda = sum . map deuda

estaOrdenado' :: Receta -> Pais -> Pais -> Bool
estaOrdenado' unaReceta unPais otroPais = pbi (unaReceta unPais) <= pbi (unaReceta otroPais)

estaOrdenado :: Receta -> [Pais] -> Bool
estaOrdenado _ [] = True
estaOrdenado _ [_] = True
estaOrdenado unaReceta (x:y:xs) = estaOrdenado' unaReceta x y   &&   estaOrdenado unaReceta (y:xs)


recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos




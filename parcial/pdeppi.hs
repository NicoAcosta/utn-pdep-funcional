import Text.Show.Functions()

-----
-----   Parte A
-----

----- 1

type NombrePersona  = String -- Diferencié NombrePersona y NombreComida por expresividad
type Direccion      = String
type Dinero         = Float
type Cupon          = Comida -> Comida


data Persona = Persona {
    nombrePersona   :: NombrePersona,
    direccion       :: Direccion,
    dineroDisponible:: Dinero,
    comidaFavorita  :: Comida,
    cupones         :: [Cupon]
} deriving Show

type ModificacionPersona = Persona -> Persona

modificarDineroDisponible :: (Dinero -> Dinero) -> ModificacionPersona
modificarDineroDisponible funcionAAplicar unaPeronsa = unaPeronsa {
    dineroDisponible = (funcionAAplicar.dineroDisponible) unaPeronsa
}

descontarDinero :: Dinero -> ModificacionPersona
descontarDinero unImporte = modificarDineroDisponible (+ (-unImporte))

cambiarComidaFavorita :: Comida -> ModificacionPersona
cambiarComidaFavorita unaComida unaPersona = unaPersona {
    comidaFavorita = unaComida
}

----- 2

type NombreComida   = String
type Ingrediente    = String

data Comida = Comida {
    nombreComida    :: NombreComida,
    costo           :: Dinero,
    ingredientes    :: [Ingrediente]
} deriving (Show, Eq)

type ModificacionComida = Comida -> Comida

modificarCosto :: (Dinero -> Dinero) -> ModificacionComida
modificarCosto unaFuncion unaComida = unaComida {
    costo = (unaFuncion.costo) unaComida
}

sumarCosto :: Dinero -> ModificacionComida
sumarCosto unImporte = modificarCosto (+ unImporte)

porcentajeDeDescuentoAFactor :: Int -> Float
porcentajeDeDescuentoAFactor = (1 -).(/100).fromIntegral

descuento :: Int -> ModificacionComida
descuento porcentajeDeDescuento = modificarCosto (* (porcentajeDeDescuentoAFactor porcentajeDeDescuento))

modificarNombreComida :: (NombreComida -> NombreComida) -> ModificacionComida
modificarNombreComida unaFuncion unaComida = unaComida {
    nombreComida = (unaFuncion.nombreComida) unaComida
}

modificarIngredientes :: ([Ingrediente] -> [Ingrediente]) -> ModificacionComida
modificarIngredientes unaFuncion unaComida = unaComida {
    ingredientes = (unaFuncion.ingredientes) unaComida
}

----- 3

paula :: Persona
paula = Persona {
    nombrePersona       = "Paula",
    direccion           = "Thames 1585",
    comidaFavorita      = hamburguesaDeluxe,
    dineroDisponible    = 3600,
    cupones =           []
}

----- 4

hamburguesaDeluxe :: Comida
hamburguesaDeluxe = Comida {
    nombreComida    = "Hamburguesa Deluxe",
    costo           = 350,
    ingredientes    = ["Pan", "Carne", "Lechuga", "Tomate", "Panceta", "Queso", "Huevo Frito"]
}

----- 5

nicolas :: Persona
nicolas = Persona {
    nombrePersona       = "Nicolas",
    direccion           = "Av. Beiro 4600",
    comidaFavorita      = asado,
    dineroDisponible    = 3000,
    cupones =           [largaDistancia, esoNoEsCocaPapi "Jack Daniel's"]
}

asado :: Comida
asado = Comida {
    nombreComida    = "Asado",
    costo           = 600,
    ingredientes    = ["Tira", "Picanha", "Chorizo", "Morcilla", "Molleja", "Pollo", "Provoleta"]
}



-----
-----   Parte B
-----

----- 1

leAlcanzaElDinero :: Comida -> Persona -> Bool
leAlcanzaElDinero unaComida = (>= costo unaComida).dineroDisponible

descontarCostoDeComida :: Comida -> ModificacionPersona
descontarCostoDeComida unaComida = descontarDinero (costo unaComida)

type Compra = Persona -> Persona

{-
Aunque tengan el mismo tipo, para que sea más expresivo diferencié
Compra  de  ModificacionPersona y
Cupon   de  ModificacionComida
-}

comprar :: Comida -> Compra
comprar unaComida unaPersona
    |   leAlcanzaElDinero unaComida unaPersona  =   realizarCompra unaComida unaPersona
    |   otherwise                               =   unaPersona

realizarCompra :: Comida -> Compra
realizarCompra unaComida
    |   ((< 200).costo) unaComida   =   descontarCostoDeComida unaComida . cambiarComidaFavorita unaComida
    |   otherwise                   =   descontarCostoDeComida unaComida

----- 2

comprarVariasComidas :: [Comida] -> ModificacionPersona
comprarVariasComidas unasComidas unaPersona = foldr comprar unaPersona unasComidas

carritoDeCompras :: [Comida] -> ModificacionPersona
carritoDeCompras unasComidas = descontarDinero 100 . comprarVariasComidas unasComidas


-----
-----   Parte C
-----

----- 1

esIngredienteVegano :: Ingrediente -> Bool
esIngredienteVegano unIngrediente =     unIngrediente /= "Carne"
                                    &&  unIngrediente /= "Huevos"
                                    &&  unIngrediente /= "Queso"

esComidaVegana :: Comida -> Bool
esComidaVegana = all esIngredienteVegano . ingredientes

semanaVegana :: Cupon
semanaVegana unaComida
    |   esComidaVegana unaComida    =   descuento 50 unaComida
    |   otherwise                   =   unaComida

----- 2

type Bebida = Ingrediente

agregarANombreDeComida :: String -> ModificacionComida
agregarANombreDeComida unaCadena = modificarNombreComida (++ unaCadena)

agregarIngrediente :: Ingrediente -> ModificacionComida
agregarIngrediente unIngrediente = modificarIngredientes (++ [unIngrediente])

esoNoEsCocaPapi :: Bebida -> Cupon
esoNoEsCocaPapi unaBebida = agregarANombreDeComida " Party" . agregarIngrediente unaBebida

----- 3

mapIngredientes :: (Ingrediente -> Ingrediente) -> ModificacionComida
mapIngredientes unaFuncion = modificarIngredientes (map unaFuncion)

agregarTextoAIngredientes :: String -> ModificacionComida
agregarTextoAIngredientes unaCadena = mapIngredientes (++ unaCadena)

sinTACCis :: Cupon
sinTACCis = agregarTextoAIngredientes " libre de gluten"

----- 4

tieneIngrediente :: Ingrediente -> Comida -> Bool
tieneIngrediente unIngrediente = elem unIngrediente . ingredientes

notieneCarne :: Comida -> Bool
notieneCarne = not . tieneIngrediente "Carne"

findeVegetariano :: Cupon
findeVegetariano unaComida
    |   notieneCarne unaComida  =   descuento 30 unaComida
    |   otherwise               =   unaComida

----- 5

filtrarIngredientes :: (Ingrediente -> Bool) -> ModificacionComida
filtrarIngredientes condicion = modificarIngredientes (filter condicion)

noEsIngredienteLargo :: Ingrediente -> Bool
noEsIngredienteLargo = (<= 10).length

filtrarIngredientesLargos :: ModificacionComida
filtrarIngredientesLargos = filtrarIngredientes noEsIngredienteLargo

largaDistancia :: Cupon
largaDistancia = sumarCosto 50 . filtrarIngredientesLargos



-----
-----   Parte C
-----

----- 1

aplicarCuponesAUnaComida :: [Cupon] -> Comida -> Comida
aplicarCuponesAUnaComida unosCupones unaComida = foldr ($) unaComida unosCupones

aplicarCuponesDisponiblesAComidaFavorita :: Persona -> Comida
aplicarCuponesDisponiblesAComidaFavorita unaPersona = aplicarCuponesAUnaComida (cupones unaPersona) (comidaFavorita unaPersona)

comprarConCupones :: Compra
comprarConCupones unaPersona = comprar (aplicarCuponesDisponiblesAComidaFavorita unaPersona) unaPersona

----- 2

estaRepetido :: Eq a => a -> [a] -> Bool
estaRepetido elemento lista = elem elemento lista -- Esta función está solo por expresividad

noEstaRepetido :: Eq a => a -> [a] -> Bool
noEstaRepetido elemento = not . estaRepetido elemento

considerarSiNoEstaRepetido :: Eq a => a -> [a] -> [a]
considerarSiNoEstaRepetido elemento lista
    |   not (estaRepetido elemento lista)   =   [elemento]
    |   otherwise                           =   []

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos (x:xs) = considerarSiNoEstaRepetido x xs ++ sinRepetidos xs
sinRepetidos [] =[]

esVocal :: Char -> Bool
esVocal caracter =  caracter == 'a' || caracter == 'A' ||
                    caracter == 'e' || caracter == 'E' ||
                    caracter == 'i' || caracter == 'I' ||
                    caracter == 'o' || caracter == 'O' ||
                    caracter == 'u' || caracter == 'U'

filtrarConsonantes :: String -> String
filtrarConsonantes = filter (not.esVocal)

precioSuperComdia :: [Comida] -> Dinero
precioSuperComdia = sum . map costo

nombreSuperComida :: [Comida] -> NombreComida
nombreSuperComida = filtrarConsonantes . concat . map nombreComida

ingredientesSuperComida :: [Comida] -> [Ingrediente]
ingredientesSuperComida = sinRepetidos . concat . map ingredientes

superComida :: [Comida] -> Comida
superComida unasComidas = Comida {
    nombreComida = nombreSuperComida unasComidas,
    costo = precioSuperComdia unasComidas,
    ingredientes = ingredientesSuperComida unasComidas
}

----- 3

duplicarPrecio :: ModificacionComida
duplicarPrecio = modificarCosto (*2)

filtrarPorPrecio :: (Dinero -> Bool) -> [Comida] -> [Comida]
filtrarPorPrecio condicion = filter (condicion . costo)

cuestanMenosDe400 :: [Comida] -> [Comida]
cuestanMenosDe400 = filtrarPorPrecio (< 400)

comidasParaCompraDeluxe :: [Comida] -> [Comida]
comidasParaCompraDeluxe = map duplicarPrecio . cuestanMenosDe400

superComidaDeluxe :: [Comida] -> Comida
superComidaDeluxe = superComidaDeluxe . comidasParaCompraDeluxe

compraDeluxe :: [Comida] -> Compra
compraDeluxe unasComidas = comprar (superComidaDeluxe unasComidas)


-----
-----   THE END.
-----
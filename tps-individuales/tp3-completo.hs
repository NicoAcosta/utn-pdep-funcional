import Text.Show.Functions()

type Nombre = String
type Dinero = Int
type Tactica = String
type Accion = (Participante -> Participante)
type Acciones = [Accion]
type Propiedad = (String, Int)
type Propiedades = [Propiedad]

nombreDeUnaPropiedad :: Propiedad -> String
nombreDeUnaPropiedad unaPropiedad = fst unaPropiedad

precioDeUnaPropiedad :: Propiedad -> Int
precioDeUnaPropiedad unaPropiedad = snd unaPropiedad



data Participante = Participante {
    nombre :: String,
    dinero :: Int,
    tactica :: String,
    propiedades :: Propiedades,
    acciones :: Acciones
} deriving Show

cambiarNombre :: Nombre -> Participante -> Participante
cambiarNombre unNombre unParticipante = unParticipante {
    nombre = unNombre
}

sumarDinero :: Dinero -> Participante -> Participante
sumarDinero cantidadDeDinero unParticipante = unParticipante {
    dinero = dinero unParticipante + cantidadDeDinero
}

cambiarTactica :: Tactica -> Participante -> Participante
cambiarTactica unaTactica unParticipante = unParticipante {
    tactica = unaTactica
}

agregarPropiedad :: Propiedad -> Participante -> Participante
agregarPropiedad unaPropiedad unParticipante = unParticipante {
    propiedades = propiedades unParticipante ++ [unaPropiedad]
}

agregarAccion :: Accion -> Participante -> Participante
agregarAccion unaAccion unParticipante = unParticipante {
    acciones = acciones unParticipante ++ [unaAccion]
}



gritar :: Accion
gritar unParticipante = cambiarNombre ("AHHHH" ++ nombre unParticipante) unParticipante


pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = (cambiarTactica "Comprador Compulsivo" . sumarDinero 40) unParticipante


enojarse :: Accion
enojarse unParticipante = (agregarAccion gritar . sumarDinero 50) unParticipante


tieneTactica :: Participante -> String -> Bool
tieneTactica unParticipante unaTactica = tactica unParticipante == unaTactica

adquirirPropiedad :: Propiedad -> Participante -> Participante
adquirirPropiedad unaPropiedad unParticipante= (agregarPropiedad unaPropiedad . sumarDinero (- precioDeUnaPropiedad unaPropiedad)) unParticipante

subastar :: Participante -> Propiedad -> Participante
subastar unParticipante unaPropiedad
    |   tieneTactica unParticipante "Oferente singular" || tieneTactica unParticipante "Accionista" =
            adquirirPropiedad unaPropiedad unParticipante
    |   otherwise = unParticipante


esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = precioDeUnaPropiedad unaPropiedad < 150

cantidadDePropiedadesSegun :: Participante -> (Propiedad -> Bool) -> Dinero
cantidadDePropiedadesSegun unParticipante condicion = (length.filter condicion.propiedades) unParticipante

montoAlquileres :: Participante -> Int
montoAlquileres unParticipante =    cantidadDePropiedadesSegun unParticipante esPropiedadBarata         * 10 +
                                    cantidadDePropiedadesSegun unParticipante (not.esPropiedadBarata)   * 20


cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = sumarDinero (montoAlquileres unParticipante) unParticipante


pagarAAccionistas :: Accion
pagarAAccionistas unParticipante
    |   tieneTactica unParticipante "Accionista" =  sumarDinero  200    unParticipante
    |   otherwise =                                 sumarDinero (-100)  unParticipante




carolina :: Participante
carolina = Participante {
    nombre      = "Carolina",
    dinero      = 500,
    tactica     = "Accionista",
    propiedades = [],
    acciones    = [pasarPorElBanco, pagarAAccionistas]
}

manuel :: Participante
manuel = Participante {
    nombre      = "Manuel",
    dinero      = 500,
    tactica     = "Oferente Singular",
    propiedades = [],
    acciones    = [pasarPorElBanco, enojarse]
}



tieneSuficienteDineroPara :: Participante -> Propiedad -> Bool
tieneSuficienteDineroPara unParticipante unaPropiedad = dinero unParticipante >= precioDeUnaPropiedad unaPropiedad


hacerBerrinchePor :: Propiedad -> Participante -> Participante
hacerBerrinchePor unaPropiedad unParticipante 
    |   tieneSuficienteDineroPara unParticipante unaPropiedad   =   adquirirPropiedad unaPropiedad unParticipante
    |   otherwise                                               =   ( hacerBerrinchePor unaPropiedad . sumarDinero 10 . gritar) unParticipante



ultimaRonda :: Participante -> Accion
ultimaRonda unParticipante = foldl (.) id (acciones unParticipante)

aplicarUltimaRonda :: Accion
aplicarUltimaRonda unParticipante = (ultimaRonda unParticipante) unParticipante

dineroFinal :: Participante -> Dinero
dineroFinal = dinero . aplicarUltimaRonda

juegoFinal :: Participante -> Participante -> Participante
juegoFinal unParticipante otroParticipante
    |   dineroFinal unParticipante > dineroFinal otroParticipante   =   aplicarUltimaRonda unParticipante
    |   otherwise                                                   =   aplicarUltimaRonda otroParticipante

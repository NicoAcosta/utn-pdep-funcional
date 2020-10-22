type Propiedad = (String, Int)

nombreDeUnaPropiedad :: Propiedad -> String
nombreDeUnaPropiedad unaPropiedad = fst unaPropiedad

precioDeUnaPropiedad :: Propiedad -> Int
precioDeUnaPropiedad unaPropiedad = snd unaPropiedad

type Accion = (Participante -> Participante)

data Participante = Participante {
    nombre :: String,
    dinero :: Int,
    tactica :: String,
    propiedades :: [Propiedad],
    acciones :: [Accion]
}

gritar :: Accion
gritar unParticipante = unParticipante {
    nombre = "AHHHH" ++ nombre unParticipante
}

pasarPorElBanco :: Accion
pasarPorElBanco unParticipante = unParticipante {
    dinero      =   dinero unParticipante + 40,
    tactica     =   "Comprador Compulsivo"
}

enojarse :: Accion
enojarse unParticipante = unParticipante {
    dinero      =   dinero unParticipante + 50,
    acciones    =   acciones unParticipante ++ [gritar]
}

es :: Participante -> String -> Bool
es unParticipante unaTactica = tactica unParticipante == unaTactica

subastar :: Participante -> Propiedad -> Participante
subastar unParticipante unaPropiedad
    |   es unParticipante "Oferente singular"   =   unParticipante {
            dinero = dinero unParticipante - precioDeUnaPropiedad unaPropiedad,
            propiedades = propiedades unParticipante ++ [unaPropiedad]
        }
    |   es unParticipante "Accionista"          =   unParticipante {
            dinero = dinero unParticipante - precioDeUnaPropiedad unaPropiedad,
            propiedades = propiedades unParticipante ++ [unaPropiedad]
        }
    |   otherwise = unParticipante

esPropiedadBarata :: Propiedad -> Bool
esPropiedadBarata unaPropiedad = precioDeUnaPropiedad unaPropiedad < 150

montoAlquileres :: Participante -> Int
montoAlquileres unParticipante =    (length.(filter         esPropiedadBarata).         propiedades) unParticipante * 10 +
                                    (length.(filter (not.   esPropiedadBarata)).        propiedades) unParticipante * 20

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = unParticipante {
    dinero = dinero unParticipante + montoAlquileres unParticipante
}

pagarAAccionistas :: Accion
pagarAAccionistas unParticipante
    |   es unParticipante "Accionista" = unParticipante {
            dinero = dinero unParticipante + 200
        }
    |   otherwise = unParticipante {
            dinero = dinero unParticipante - 100
        }



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
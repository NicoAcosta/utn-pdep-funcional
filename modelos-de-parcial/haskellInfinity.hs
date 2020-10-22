import Text.Show.Functions()

type Nombre = String
type Poder = Int
type Anio = Int

type Derrota = (Nombre, Anio)

data Personaje = Personaje {
    nombre :: Nombre,
    poder :: Poder,
    derrotas :: [Derrota],
    equipamientos :: [Equipamiento]
} deriving Show

spiderman :: Personaje
spiderman = Personaje {
    nombre = "Spiderman",
    poder = 100,
    derrotas = [],
    equipamientos = []
}

ironMan :: Personaje
ironMan = Personaje {
    nombre = "Iron Man",
    poder = 250,
    derrotas = [],
    equipamientos = []
}

vision :: Personaje
vision = Personaje {
    nombre = "Vision",
    poder = 150,
    derrotas = [],
    equipamientos = []
}

capitanAmerica :: Personaje
capitanAmerica = Personaje {
    nombre = "Capitan América",
    poder = 200,
    derrotas = [],
    equipamientos = []
}

thanos :: Personaje
thanos = Personaje {
    nombre = "Thanos",
    poder = 10000,
    derrotas = [],
    equipamientos = []
}


-----
----- Parte A
-----

type Modificacion = Personaje -> Personaje

cambiarPoder :: (Poder -> Poder) -> Modificacion
cambiarPoder funcion unPersonaje = unPersonaje{
    poder = (funcion.poder) unPersonaje
}

entrenamiento :: [Personaje] -> [Personaje]
entrenamiento unosPersonajes =
    map ( cambiarPoder (* (length unosPersonajes)) ) unosPersonajes


derrotaHijoDeThanos :: Personaje -> Bool
derrotaHijoDeThanos unPersonaje = elem "Hijo de Thanos" ((map fst . derrotas) unPersonaje)

evaluarPoder :: (Poder -> Bool) -> Personaje -> Bool
evaluarPoder condicion = condicion.poder

filterDosCondiciones :: (Personaje -> Bool) -> (Personaje -> Bool) -> [Personaje] -> [Personaje]
filterDosCondiciones condicion1 condicion2 =
    filter condicion1 . filter condicion2


rivalesDignos :: [Personaje] -> [Personaje]
rivalesDignos unosPersonajes =
    filterDosCondiciones derrotaHijoDeThanos (evaluarPoder (>500)) unosPersonajes


agregarDerrota :: Derrota -> Personaje -> Personaje
agregarDerrota unaDerrota unPersonaje = unPersonaje {
    derrotas = ( ( ++ [unaDerrota] ) . derrotas ) unPersonaje
}

agregarDerrotas :: [Derrota] -> Personaje -> Personaje
agregarDerrotas unasDerrotas unPersonaje = unPersonaje {
    derrotas = ( ( ++ unasDerrotas ) . derrotas ) unPersonaje
}

ganador :: Anio -> Personaje -> Personaje -> Personaje
ganador unAnio unPersonaje otroPersonaje
    |   poder unPersonaje > poder otroPersonaje     =   agregarDerrota (nombre otroPersonaje, unAnio) unPersonaje
    |   otherwise                                   =   agregarDerrota (nombre unPersonaje, unAnio)   otroPersonaje

guerraCivil :: Anio -> [Personaje] -> [Personaje] -> [Personaje]
guerraCivil unAnio unosPersonajes otrosPersonajes = zipWith (ganador unAnio) unosPersonajes otrosPersonajes

guerraCivil' :: Anio -> [Personaje] -> [Personaje] -> [Personaje]
guerraCivil' _ [] [] = []
guerraCivil' unAnio unosPersonajes otrosPersonajes =
    [ ganador unAnio (head unosPersonajes) (head otrosPersonajes) ] ++ guerraCivil unAnio (tail unosPersonajes) (tail otrosPersonajes)








-----
----- Parte B
-----

type Equipamiento = Personaje -> Personaje

tieneMenosDe5derrotas :: Personaje -> Bool
tieneMenosDe5derrotas = (<5).length.derrotas

escudo :: Equipamiento
escudo unPersonaje
    |   tieneMenosDe5derrotas unPersonaje   =   cambiarPoder (+50)      unPersonaje
    |   otherwise                           =   cambiarPoder ((-) 100)  unPersonaje

cambiarNombre :: (String -> String) -> Modificacion
cambiarNombre funcion unPersonaje = unPersonaje {
    nombre = (funcion.nombre) unPersonaje
}

trajeMecanizado :: Int -> Equipamiento
trajeMecanizado unaVersion = cambiarNombre ( (++ show unaVersion).(++ " V").("Iron " ++) )


exclusivo :: Nombre -> Personaje -> Modificacion -> Personaje
exclusivo unNombre unPersonaje funcion
    |   unNombre == (nombre unPersonaje)    =   funcion unPersonaje
    |   otherwise                           =   unPersonaje

limpiarDerrotas :: Modificacion
limpiarDerrotas unPersonaje = unPersonaje {
    derrotas = []
}

stormBreaker :: Equipamiento
stormBreaker unPersonaje = exclusivo "Thor" unPersonaje ( cambiarNombre (++ " dios del trueno") . limpiarDerrotas)

extras :: [Derrota]
extras = extras' 1

extras' :: Int -> [Derrota]
extras' n = ("extra numero " ++ show n, 2017 + n) : extras' (n + 1)


gemaDelAlma :: Equipamiento
gemaDelAlma unPersonaje =
    exclusivo "Thanos" unPersonaje (agregarDerrotas extras)

--guanteleteInfinito :: Equipamiento
--guanteleteInfinito unPersonaje = exclusivo "Thanos" unPersonaje ()





-----
----- Parte C
-----





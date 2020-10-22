module Library where
--import PdePreludat

--revisar Number, hay un tipo más específico
-- take :: Number -> String -> String

-- idem acá
-- drop :: Number -> String -> String

-- head :: String -> Char

-- elem :: Char -> String -> Bool

-- reverse :: String -> String


versionBarata :: String -> String
versionBarata nombre = reverse nombre

productoXL :: String -> String
productoXL nombre = nombre ++ "XL"

productoCorriente :: String -> Bool
productoCorriente nombre =
    head nombre == 'A' || head nombre == 'a' ||
    head nombre == 'E' || head nombre == 'e' || 
    head nombre == 'I' || head nombre == 'i' || 
    head nombre == 'O' || head nombre == 'o' || 
    head nombre == 'U' || head nombre == 'u'

-- se puede aprovechar composicion. se te ocurre como?
productoCodiciado :: String -> Bool
productoCodiciado nombre = length nombre > 10

aplicarCostoDeEnvio :: Num a => a -> a -> a
aplicarCostoDeEnvio precio costoDeEnvio = precio + costoDeEnvio

productoDeLujo :: String -> Bool
productoDeLujo nombre = elem 'x' nombre || elem 'z' nombre

descodiciarProducto :: String -> String
descodiciarProducto nombre = take 9 nombre

aplicarDescuento :: Fractional a => a -> a -> a
aplicarDescuento precio descuento = precio * (100 - descuento) / 100

productoDeElite :: String -> Bool
productoDeElite nombre =
    productoDeLujo nombre && productoCodiciado nombre && productoCorriente nombre

precioTotal :: Fractional a => a -> a -> a -> a -> a
precioTotal precioUnitario cantidad descuento costoDeEnvio =
    aplicarCostoDeEnvio ((aplicarDescuento (precioUnitario * cantidad) descuento)) costoDeEnvio

-- precioTotal precioUnitario cantidad descuento costoDeEnvio =
--     cantidad * precioUnitario * (1 - (descuento / 100)) + costoDeEnvio

entregaSencilla :: String -> Bool
-- acá también se puede usar composicion :)
entregaSencilla diaDeEntrega = even (length diaDeEntrega)



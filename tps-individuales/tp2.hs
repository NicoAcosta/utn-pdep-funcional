module Library where
--import PdePreludat

titulo :: (String, a, b) -> String
titulo (titulo, _, _) = titulo

autor :: (a, String, b) -> String
autor (_, autor, _) = autor

paginas :: (a, b, Number) -> Number
paginas (_, _, paginas) = paginas


promedioDeHojas :: [(String, String, Number)] -> Number 
promedioDeHojas biblioteca = ((/(length biblioteca)).sum.(map paginas)) biblioteca


lecturaObligatoria :: (String, String, Number) -> Bool
lecturaObligatoria libro =
    autor libro == "Stephen King" ||
    take 6 (titulo libro) == "Eragon" ||
    (titulo libro == "Fundación" && autor libro == "Isaac Asimov" && paginas libro == 230)


esPaoliniOGaiman :: (String, String, Number) -> Bool
esPaoliniOGaiman libro = autor libro == "Christopher Paolini" || autor libro == "Neil Gaiman"


fantasiosa :: [(String, String, Number)] -> Bool
fantasiosa = any esPaoliniOGaiman


esVocal :: Char -> Bool
esVocal l = (l == 'a') || (l == 'e') || (l == 'i') || (l == 'o') || (l == 'u') ||
            (l == 'A') || (l == 'E') || (l == 'I') || (l == 'O') || (l == 'U')


nombreDeLaBiblioteca :: [(String, String, Number)] -> String
nombreDeLaBiblioteca = (filter (not.esVocal)).concat.(map titulo)


bibliotecaLigera :: [(String, String, Number)] -> Bool
bibliotecaLigera = all ((40>=).paginas)

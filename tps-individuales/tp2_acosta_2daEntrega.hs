module Library where

type Libro = (String, String, Int)

titulo :: Libro -> String
titulo (titulo, _, _) = titulo


autor :: Libro -> String
autor (_, autor, _) = autor


paginas :: Libro -> Int
paginas (_, _, paginas) = paginas


promedioDeHojas :: [Libro] -> Int 
promedioDeHojas biblioteca = div ( (sum.map paginas) biblioteca ) (length biblioteca)


lecturaObligatoria :: Libro -> Bool
lecturaObligatoria (_, "Stephen King", _) = True
lecturaObligatoria ("Eragon", _, _) = True
lecturaObligatoria ("Fundación", "Isaac Asimov", 230) = True
lecturaObligatoria _ = False


esPaoliniOGaiman :: Libro -> Bool
esPaoliniOGaiman (_, "Christopher Paolini", _) = True
esPaoliniOGaiman (_, "Neil Gaiman", _) = True
esPaoliniOGaiman _ = False


fantasiosa :: [Libro] -> Bool
fantasiosa = any esPaoliniOGaiman


vocales :: [Char]
vocales = ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']


esVocal :: Char -> Bool
esVocal letra = elem letra vocales


nombreDeLaBiblioteca :: [Libro] -> String
nombreDeLaBiblioteca = (filter (not.esVocal)).(concatMap titulo)


bibliotecaLigera :: [Libro] -> Bool
bibliotecaLigera = all ((40>=).paginas)

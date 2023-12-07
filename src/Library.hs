module Library where
import PdePreludat

-- Primera parte

-- Defino mis alias
type Edad = Number
type Item = String
type Items = [Item]
type Experiencia = Number
type Peligrosidad = Number
type CondicionesParaDeshacerse = Persona -> Bool
type CantidadGnomos = Number
type Categoria = Number
type Enfrentamiento = Persona -> Criatura -> Persona
type Criaturas = [Criatura]

-- Defino mis tipos
data Persona = UnaPersona {
    edad :: Edad,
    items :: Items,
    experiencia :: Experiencia
} deriving Show

data Criatura = UnaCriatura {
    peligrosidad :: Peligrosidad,
    condicionesParaDeshacerse :: CondicionesParaDeshacerse
} deriving Show

-- Inicializo critaturas
nada :: CondicionesParaDeshacerse
nada persona = False
tieneItem :: Item -> Persona -> Bool
tieneItem item persona = elem item (items persona)
tieneMenosDe :: Edad -> Persona -> Bool
tieneMenosDe edadMayor persona = (< edadMayor) (edad persona)
esExperimentado :: Experiencia -> Persona -> Bool
esExperimentado experienciaMenor persona = (> experienciaMenor) (experiencia persona)

siempreDetras :: Criatura
siempreDetras = UnaCriatura 0 nada
gnomos :: CantidadGnomos -> Criatura
gnomos cantidadGnomos = UnaCriatura  ((^2) cantidadGnomos) (tieneItem "Soplador de Hojas")
fantasmas :: Categoria -> CondicionesParaDeshacerse -> Criatura
fantasmas categoria condicionesParaDeshacerse = UnaCriatura ((*20) categoria) condicionesParaDeshacerse

-- Inicializo personas
dipperPines :: Persona
dipperPines = UnaPersona 12 ["Soplador de Hojas"] 10

-- Defino que una persona enfrente a una criatura
escapar :: Persona -> Persona
escapar persona = persona {experiencia = (+ 1) (experiencia persona)} 

{-tieneMasDeUnaCondicion :: Criatura -> Bool
tieneMasDeUnaCondicion = (>1).length.condicionesParaDeshacerse

aplicarCondiciones :: Persona -> Criatura -> Bool
aplicarCondiciones persona criatura = any (condicionesParaDeshacerse criatura) persona

puedeDeshacerse :: Persona -> Criatura -> Bool
puedeDeshacerse persona criatura 
    | tieneMasDeUnaCondicion criatura = aplicarCondiciones persona criatura
    | otherwise = (condicionesParaDeshacerse criatura) persona
-} -- Idea descartada

puedeDeshacerse :: Persona -> Criatura -> Bool
puedeDeshacerse persona criatura = (condicionesParaDeshacerse criatura) persona

ganarExperiencia :: Persona -> Criatura -> Persona
ganarExperiencia persona criatura = persona {experiencia = (+ peligrosidad criatura) (experiencia persona)}

enfrentarCriatura :: Enfrentamiento
enfrentarCriatura persona criatura 
    | puedeDeshacerse persona criatura = ganarExperiencia persona criatura
    | otherwise = escapar persona

-- Defino cuanta experiencia gana una persona después de enfrentarse a un grupo de criaturas
grupoCriaturas = [siempreDetras, (gnomos 10), (fantasmas 3 (tieneMenosDe 13)), (fantasmas 1 (esExperimentado 10))]

enfrentarCriaturas :: Persona -> Criaturas -> Experiencia
enfrentarCriaturas persona criaturas = experiencia (foldl (enfrentarCriatura) persona grupoCriaturas)

-- Segunda parte

-- Defino la función recursiva
zipWithIf :: (a -> b -> b) -> (b -> Bool) -> [a] -> [b] -> [b] 
zipWithIf _ _ _ [] = []
zipWithIf funcion condicion (x1:xs1) (x2:xs2) 
    | condicion x2 = (funcion x1 x2):(zipWithIf funcion condicion (x1:xs1) xs2)
    | otherwise = x2:(zipWithIf funcion condicion (x1:xs1) xs2)

-- Defino la función
abecedarioDesde :: Char -> [Char]
abecedarioDesde letra = [letra..'z'] ++ (take ((length ['a'..letra]) - 1))['a'..letra]

-- Defino la función 
obtenerLetra :: [Char] -> [Char] -> Char -> Char
obtenerLetra (x:xs) (y:ys) letra 
    | x == letra = y
    | otherwise = obtenerLetra xs ys letra

desencriptarLetra :: Char -> Char -> Char
desencriptarLetra letraClave letraADesencriptar = obtenerLetra (abecedarioDesde letraClave) (abecedarioDesde 'a') letraADesencriptar

-- Defino la función 
esUnaLetra :: Char -> Bool
esUnaLetra letra = elem letra ['a'..'z']

cesar :: Char -> String -> String
cesar letraClave textoEncriptado = zipWithIf desencriptarLetra esUnaLetra (abecedarioDesde letraClave) textoEncriptado
--------------
-- Punto 01 --
--------------

data Personaje = Personaje {
    edad :: Int,
    energia :: Float,
    habilidades :: [Habilidad],
    nombre :: String,
    planeta :: String
}

data Guantele = Guantele {
    material :: Material,
    gemas :: [Gema]
}

type Habilidad = String
type Material = String
type Gema = Personaje -> Personaje
type Universo = [Personaje]

chasquidoUniverso :: Guantele -> Universo -> Universo
chasquidoUniverso unGuantele unUniverso
    | puedeUsarse unGuantele = reducirMitad unUniverso
    | otherwise              = unUniverso

puedeUsarse :: Guantele -> Bool
puedeUsarse unGuantele = ((==6) . length . gemas) unGuantele && ((== "uru") . material) unGuantele

reducirMitad :: Universo -> Universo
reducirMitad unUniverso = take (div (length unUniverso) 2) unUniverso

--------------
-- Punto 02 --
--------------

aptoParaPendex :: Universo -> Bool
aptoParaPendex = any $ (<= 45) . edad

energiaTotal :: Universo -> Float
energiaTotal = sum . map energia . filter ((> 1) . length . habilidades)

--------------
-- Punto 03 --
--------------

mente :: Float -> Gema
mente unValor unPersonaje = unPersonaje { energia = energia unPersonaje - unValor }

type HabilidadAEliminar = String

alma :: HabilidadAEliminar -> Gema
alma unaHabilidad = mente 10 . eliminarHabilidad unaHabilidad 

eliminarHabilidad :: HabilidadAEliminar -> Gema
eliminarHabilidad unaHabilidad = mapHabilidad (filter (/= unaHabilidad))

mapHabilidad :: ([Habilidad] -> [Habilidad]) -> Personaje -> Personaje
mapHabilidad unaFuncion unPersonaje = unPersonaje { habilidades = unaFuncion (habilidades unPersonaje)}

type Planeta = String

espacio :: Planeta -> Gema
espacio unPlaneta = cambiarPlaneta unPlaneta . mente 20 

cambiarPlaneta :: Planeta -> Gema
cambiarPlaneta unPlaneta unPersonaje = unPersonaje { planeta = unPlaneta} 

poder :: Gema
poder unPersonaje
    | cantidadDeHabilidades unPersonaje = eliminarTodasLasHabilidades . energiaEnCero $ unPersonaje
    | otherwise                         = energiaEnCero unPersonaje

cantidadDeHabilidades :: Personaje -> Bool
cantidadDeHabilidades = (<= 2) . length . habilidades

eliminarTodasLasHabilidades :: Personaje -> Personaje
eliminarTodasLasHabilidades unPersonaje = unPersonaje { habilidades = [] } 

energiaEnCero :: Personaje -> Personaje
energiaEnCero unPersonaje = unPersonaje { energia = 0 } 

tiempo :: Gema
tiempo unPersonaje = mente 50 . mitadEdad $ unPersonaje

mitadEdad :: Personaje -> Personaje
mitadEdad unPersonaje = unPersonaje { edad = (max 18 . div (edad unPersonaje)) 2 }

gemaLoca :: Gema -> Gema
gemaLoca unaGema = unaGema . unaGema

--------------
-- Punto 04 --
--------------


--------------
-- Punto 05 --
--------------

utilizar :: [Gema] -> Gema 
utilizar gemas unPersonaje = foldr ($) unPersonaje $ gemas

--------------
-- Punto 06 --
--------------

gemaMasPoderosa :: Personaje -> Guantele -> Gema
gemaMasPoderosa unPersonaje unGuantele = gemaMasPoderosaDe unPersonaje $ gemas unGuantele

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe unPersonaje (gema1:gema2:gemas)
    | (energia . gema1) unPersonaje < (energia . gema2) unPersonaje = gemaMasPoderosaDe unPersonaje (gema1:gemas)
    | otherwise                                                     = gemaMasPoderosaDe unPersonaje (gema2:gemas)

--------------
-- Punto 07 --
--------------

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantele
guanteleteDeLocos = Guantele "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantele -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- gemaMasPoderosa punisher guanteleteDeLocos
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher

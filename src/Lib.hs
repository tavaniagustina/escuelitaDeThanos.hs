--------------
-- Punto 01 --
--------------

data Personaje = Personaje {
    nombre :: String,
    edad :: Int,
    energia :: Int,
    habilidades :: [Habilidad],
    planeta :: String
}

data Guantelete = Guantelete {
    material :: Material,
    gemas :: [Gema]
}

type Material = String
type Gema = Personaje -> Personaje
type Habilidad = String
type Universo = [Personaje]

chasquidoDelUniverso :: Guantelete -> Universo -> Universo
chasquidoDelUniverso unGuantele unUniverso 
    | puedeUsarse unGuantele = reducirPersonajesMitad unUniverso
    | otherwise              = unUniverso

puedeUsarse :: Guantelete -> Bool
puedeUsarse unGuantele = ((== 6) . length . gemas) unGuantele && ((== "uru") . material) unGuantele

reducirPersonajesMitad :: Universo -> Universo
reducirPersonajesMitad unUniverso = take (div (length unUniverso) 2) unUniverso

--------------
-- Punto 02 --
--------------

aptoPendex :: Universo -> Bool
aptoPendex = any $ (<= 45) . edad  

energiaTotal :: Universo -> Int
energiaTotal = sum . map energia . filter ((> 1) . length.  habilidades)

--------------
-- Punto 03 --
--------------

mente :: Int -> Gema
mente unValor unPersonaje = unPersonaje { energia = energia unPersonaje - unValor}

alma :: String -> Gema
alma unaHabilidad unPersonaje = mente 10 . quitarHabilidad unaHabilidad $ unPersonaje

quitarHabilidad :: String -> Gema
quitarHabilidad unaHabilidad unPersonaje = unPersonaje { habilidades = filter (/= unaHabilidad) $ habilidades unPersonaje }

espacio :: String -> Gema
espacio unPlaneta unPersonaje = mente 20 unPersonaje {planeta = unPlaneta}

poder :: Gema
poder unPersonaje 
    | cantidadHabilidadesMenorADos unPersonaje = sacarTodasLasHabilidades . quitarTodaLaEnergia $ unPersonaje
    | otherwise                                = quitarTodaLaEnergia unPersonaje

cantidadHabilidadesMenorADos :: Personaje -> Bool
cantidadHabilidadesMenorADos = (<= 2) . length . habilidades 

sacarTodasLasHabilidades :: Personaje -> Personaje
sacarTodasLasHabilidades unPersonaje = unPersonaje { habilidades = [] }

quitarTodaLaEnergia :: Personaje -> Personaje
quitarTodaLaEnergia unPersonaje = unPersonaje { energia = 0 }

tiempo :: Gema
tiempo unPersonaje = mente 50 unPersonaje { edad = max 18 (div (edad unPersonaje) 2) } 

gemaLoca :: Gema -> Gema
gemaLoca gema = gema . gema

--------------
-- Punto 04 --
--------------


--------------
-- Punto 05 --
--------------

utilizar :: [Gema] -> Gema
utilizar gemas unPersonaje = foldr ($) unPersonaje gemas

--------------
-- Punto 06 --
--------------

gemaMasPoderosa :: Personaje -> Guantelete -> Gema
gemaMasPoderosa unPersonaje unGuantele = gemaMasPoderosaDe unPersonaje $ gemas unGuantele

gemaMasPoderosaDe :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDe _ [gema] = gema
gemaMasPoderosaDe unPersonaje (gema1:gema2:gemas) 
    | (energia . gema1) unPersonaje < (energia . gema2) unPersonaje = gemaMasPoderosaDe unPersonaje (gema2:gemas)
    | otherwise                                                     = gemaMasPoderosaDe unPersonaje (gema1:gemas)

--------------
-- Punto 07 --
--------------

infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete

-- gemaMasPoderosa punisher guanteleteDeLocos --> Loopea
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher 
        -- loopea ya que es una evaluacion perezosa y nunca termina con la lista infinitas de gemas 

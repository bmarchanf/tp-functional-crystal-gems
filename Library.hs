module Library where
import PdePreludat

-- Funciones predefinidas
data Aspecto = UnAspecto {
  tipoDeAspecto :: String,
  grado :: Number
} deriving (Show, Eq)

type Situacion = [Aspecto]

mejorAspecto :: Aspecto -> Aspecto -> Bool
mejorAspecto mejor peor = grado mejor < grado peor

mismoAspecto :: Aspecto -> Aspecto -> Bool
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto :: Aspecto -> [Aspecto] -> Aspecto
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)

buscarAspectoDeTipo :: String -> [Aspecto] -> Aspecto
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)

reemplazarAspecto :: Aspecto -> [Aspecto] -> [Aspecto]
reemplazarAspecto aspectoBuscado situacion =
    aspectoBuscado : filter (not.mismoAspecto aspectoBuscado) situacion


-- ###SITUACIONES

modificarAspecto :: (Number->Number) -> Aspecto -> Aspecto
modificarAspecto funcionModificarGrado aspecto = aspecto { grado = funcionModificarGrado (grado aspecto) }

esMejorSituacionQue :: Situacion -> Situacion -> Bool
esMejorSituacionQue situacion1 situacion2 = all (criterioEsMejorQue situacion2) situacion1
                                            where criterioEsMejorQue situacion2 aspecto = (mejorAspecto aspecto.buscarAspecto aspecto) situacion2

modificarSituacion :: (Number->Number) -> Aspecto -> Situacion -> Situacion
modificarSituacion funcionModificarGrado aspectoBuscado situacion = (flip reemplazarAspecto situacion.modificarAspecto funcionModificarGrado) aspectoBuscado


--- ###GEMAS Y PERSONALIDADES POSIBLES

type Personalidad = Situacion -> Situacion

data Gema = Gema {
  nombre :: String,
  fuerza :: Number,
  personalidad :: Personalidad
} deriving (Show)

instance Eq Gema where
        (==) gema1 gema2 = (nombre gema1 == nombre gema2) && (fuerza gema1 == fuerza gema2)


vidente :: Personalidad
vidente situacion = map (modificarAspecto (`div` 2)) situacion

relajada :: Number -> Personalidad
relajada _ [] = []
relajada nivelRelajo (aspecto:aspectos) = modificarAspectoGemaRelajada nivelRelajo aspecto : relajada nivelRelajo aspectos

restarle :: Number -> Number -> Number
restarle = flip (-)

modificarAspectoGemaRelajada :: Number-> Aspecto -> Aspecto
modificarAspectoGemaRelajada nivelRelajo aspecto 
            | (=="Tension").tipoDeAspecto $ aspecto = modificarAspecto (restarle 30) aspecto
            | (=="Peligro").tipoDeAspecto $ aspecto = modificarAspecto (+ nivelRelajo) aspecto 
            | otherwise = aspecto                                    

--Ejemplos de Gema Vidente y Gema Relajada:

nivelRelajo :: Number
nivelRelajo = 10

--Gema Vidente:
gemaVidente :: Gema
gemaVidente = Gema {
  nombre = "Zafiro",
  fuerza = 3,
  personalidad = vidente
}

--Gema relajada
gemaRelajada :: Gema
gemaRelajada = Gema {
  nombre = "Lapilazul",
  fuerza = 5,
  personalidad = relajada nivelRelajo
}

-- ###COMPETENCIA ENTRE GEMAS

leGanaA :: Gema -> Gema -> Situacion -> Bool
leGanaA gema1 gema2 situacion = (fuerza gema1 >= fuerza gema2) && competenciaDePersonalidades gema1 gema2 situacion

competenciaDePersonalidades :: Gema -> Gema -> Situacion -> Bool
competenciaDePersonalidades gema1 gema2 situacion = esMejorSituacionQue (personalidad gema1 situacion) (personalidad gema2 situacion)


-- ###FUSION

fusionGemas :: Situacion -> Gema -> Gema -> Gema
fusionGemas situacion gema1 gema2 = Gema {
  nombre = nombre gema1 ++ nombre gema2,
  personalidad = personalidadFusionada gema1 gema2,
  fuerza = fuerzaResultante situacion gema1 gema2
}

personalidadFusionada :: Gema -> Gema -> Personalidad
personalidadFusionada gema1 gema2 = personalidad gema2.personalidad gema1.map (modificarAspecto (restarle 10))

fuerzaResultante :: Situacion -> Gema -> Gema -> Number
fuerzaResultante situacion gema1 gema2  
        | sonGemasCompatibles situacion gema1 gema2 = (fuerza gema1 + fuerza gema2)*10
        | otherwise = fuerzaGemasNoCompatibles situacion gema1 gema2

sonGemasCompatibles :: Situacion -> Gema -> Gema -> Bool
sonGemasCompatibles situacion gema1 gema2 = compararSituacionesGeneran1eraGemaYFusion situacion gema1 gema2 && compararSituacionesGeneran1eraGemaYFusion situacion gema2 gema1

compararSituacionesGeneran1eraGemaYFusion :: Situacion -> Gema -> Gema -> Bool 
compararSituacionesGeneran1eraGemaYFusion situacion gemaAComparar gema2 = esMejorSituacionQue (personalidadFusionada gemaAComparar gema2 situacion) (personalidad gemaAComparar situacion)

fuerzaGemasNoCompatibles :: Situacion -> Gema -> Gema -> Number
fuerzaGemasNoCompatibles situacion gema1 gema2 
    | leGanaA gema1 gema2 situacion  = 7*fuerza gema1
    | otherwise = 7 *fuerza gema2


-- ### FUSION GRUPAL

fusionGrupal :: Situacion -> [Gema] -> Gema
fusionGrupal situacion gemasAFusionar = foldl1 (fusionGemas situacion) gemasAFusionar

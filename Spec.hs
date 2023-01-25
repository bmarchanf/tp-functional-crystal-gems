module Spec where
import PdePreludat
import Library
import Test.Hspec

-- Aspecto y Funcion para Pruebas de funcion modificarAspecto
aspectoPruebaModificarAspecto :: Aspecto
aspectoPruebaModificarAspecto = UnAspecto "Tension" 0

funcionModificarGradoAspecto :: Number -> Number
funcionModificarGradoAspecto = (+ 3)


-- Situaciones para Pruebas de funcion esMejorSituacionQue

situacionTodoEn0 :: Situacion
situacionTodoEn0 = [UnAspecto "Tension" 0,UnAspecto "Incertidumbre" 0, UnAspecto "Peligro" 0]

situacionTodoEn1 :: Situacion
situacionTodoEn1 = [UnAspecto "Tension" 1,UnAspecto "Incertidumbre" 1, UnAspecto "Peligro" 1]


-- Aspecto, situacion y funcion para probar modificarSituacion

--situacionTodoEn0
--funcionModificarGradoAspecto

aspectoAModificar :: Aspecto
aspectoAModificar = UnAspecto "Incertidumbre" 0

tipoAspectoAModificar :: String
tipoAspectoAModificar = "Incertidumbre"

aspectoModificado :: Aspecto
aspectoModificado = UnAspecto "Incertidumbre" 3


--Situaciones de Prueba para probar las personalidades: vidente y relajada

situacionTodoEn100 :: Situacion
situacionTodoEn100 = [UnAspecto "Tension" 100,UnAspecto "Incertidumbre" 100, UnAspecto "Peligro" 100]

situacionPosteriorVidente :: Situacion
situacionPosteriorVidente = [UnAspecto "Tension" 50,UnAspecto "Incertidumbre" 50, UnAspecto "Peligro" 50]

situacionPosteriorRelajada :: Situacion
situacionPosteriorRelajada = [UnAspecto "Tension" 70,UnAspecto "Incertidumbre" 100, UnAspecto "Peligro" (100 + nivelRelajo)]


-- Situacion y Gemas de Prueba de la funcion leGanaA

-- situacion Todo en 100

gemaFuerzaMayorSituacionMejor :: Gema
gemaFuerzaMayorSituacionMejor = Gema {
  nombre = "Diamante",
  fuerza = 100,
  personalidad = vidente
}

gemaFuerzaMenorSituacionPeor :: Gema
gemaFuerzaMenorSituacionPeor = Gema {
  nombre = "Ruby",
  fuerza = 10,
  personalidad = relajada nivelRelajo
}

gemaFuerzaMayorSituacionPeor :: Gema
gemaFuerzaMayorSituacionPeor = Gema {
  nombre = "Turquesa",
  fuerza = 100,
  personalidad = relajada nivelRelajo
}


--Gemas de Prueba para la funcion personalidadFusionada

--situacionTodoEn100

--personalidad vidente
gema1 :: Gema
gema1 = gemaFuerzaMayorSituacionMejor

-- personalidad relajada con nivelRelajo (10)
gema2 :: Gema
gema2 = gemaFuerzaMenorSituacionPeor

situacionResultantePersonalidadFusionada :: Situacion
situacionResultantePersonalidadFusionada = [UnAspecto "Tension" 15,UnAspecto "Incertidumbre" 45, UnAspecto "Peligro" 55]


--Gemas de Prueba para la funcion fusionGemas

--Gemas Compatibles
--fusiono gema 1 con si misma

gemaFusionCompatibles :: Gema
gemaFusionCompatibles = Gema {
  nombre = "DiamanteDiamante",
  fuerza = 2000,
  personalidad = personalidadFusionada gema1 gema1
}

--Gemas No Compatibles
--fusiono gema 1 y gema2, siendo gema1 dominante sobre la gema 2

gemaFusionNoCompatibles1eraGemaDomina :: Gema
gemaFusionNoCompatibles1eraGemaDomina = Gema {
  nombre = "DiamanteRuby",
  fuerza = 700,
  personalidad = personalidadFusionada gema1 gema2
}

--fusiono gema2 y gema1, uso como primera gema la gema2, que no es la Dominante
gemaFusionNoCompatibles2daGemaDomina :: Gema
gemaFusionNoCompatibles2daGemaDomina = Gema {
  nombre = "RubyDiamante",
  fuerza = 700,
  personalidad = personalidadFusionada gema2 gema1
}


-- Gemas de Prueba para la funcion fusionGrupal

-- tiene como nombre Turquesa, fuerza 100 y personalidad relajada con nivelRelajo (10)
gema3 :: Gema
gema3 = gemaFuerzaMayorSituacionPeor

-- gema 1 tiene como nombre Diamante, fuerza 100 y personalidad vidente
-- gema 2 tiene como nombre Ruby, fuerza 10 y personalidad relajada con nivelRelajo (10)

gemasAFusionar :: [Gema]
gemasAFusionar = [gema1,gema2,gema3]

gemaFusionDe3 :: Gema
gemaFusionDe3 = fusionGemas situacionTodoEn100 (fusionGemas situacionTodoEn100 gema1 gema2) gema3 


correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de la funcion modificarAspecto" $ do
    it "Si se modifica el grado del Aspecto" $ do
      (grado.modificarAspecto funcionModificarGradoAspecto) aspectoPruebaModificarAspecto `shouldBe` 3
  describe "Test de la funcion esMejorSituacionQue" $ do
    it "Si todos los aspectos de la primera situacion son mejores que los de la segunda situacion" $ do
      situacionTodoEn1 `shouldSatisfy` esMejorSituacionQue situacionTodoEn0
    it "Si algunos o todos los aspectos de la primera situacion no son mejores que los de la segunda situacion" $ do
      situacionTodoEn0 `shouldNotSatisfy` esMejorSituacionQue situacionTodoEn1
  describe "Test de la funcion modificarSituacion" $ do
    it "Si se modifica el aspecto buscado en la situacion dada segun la funcion que modifica el Grado del Aspecto" $ do
      (buscarAspectoDeTipo tipoAspectoAModificar.modificarSituacion funcionModificarGradoAspecto aspectoAModificar) situacionTodoEn0 `shouldBe` aspectoModificado
  describe "Test de la personalidad vidente" $ do
    it "Si se modifican todos los grados de los aspectos de la situacion a la mitad" $ do
      vidente situacionTodoEn100 `shouldBe` situacionPosteriorVidente
  describe "Test de la personalidad relajada" $ do
    it "Si se disminuye el grado de la Tension en 30 y se aumenta en nivel de relajo el Peligro de la situacion" $ do
      relajada nivelRelajo situacionTodoEn100 `shouldBe` situacionPosteriorRelajada
  describe "Test de la funcion leGanaA" $ do
    it "Si la fuerza de la 1ra gema es mayor o igual a la fuerza de la segunda y la situacion resultante que provoca su personalidad es mejor que la generada por la segunda gema" $ do
      situacionTodoEn100 `shouldSatisfy` leGanaA gemaFuerzaMayorSituacionMejor gemaFuerzaMenorSituacionPeor
    it "Si la fuerza de la 1ra gema es mayor o igual a la fuerza de la segunda y la situacion resultante que provoca su personalidad no es mejor que la generada por la segunda gema" $ do
      situacionTodoEn100 `shouldNotSatisfy` leGanaA gemaFuerzaMayorSituacionPeor gemaFuerzaMayorSituacionMejor
    it "Si la primera gema tiene menor fuerza que la segunda gema" $ do
      situacionTodoEn100 `shouldNotSatisfy` leGanaA gemaFuerzaMenorSituacionPeor gemaFuerzaMayorSituacionMejor
  describe "Test de la funcion personalidadFusionada" $ do
    it "Si se disminuye en 10 los aspectos de una situacion dada y se le aplican las 2 personalidades de las gemas consecutivamente" $ do 
      (personalidadFusionada gema1 gema2) situacionTodoEn100 `shouldBe` situacionResultantePersonalidadFusionada
  describe "Test de la funcion fusionGemas" $ do
    it "Si las gemas son compatibles" $ do 
      fusionGemas situacionTodoEn100 gema1 gema1 `shouldBe` gemaFusionCompatibles
      (flip personalidad situacionTodoEn0.fusionGemas situacionTodoEn100 gema1) gema1 `shouldBe` personalidad gemaFusionCompatibles situacionTodoEn0
    it "Si las gemas no son compatibles y la primera gema es dominante" $ do 
      fusionGemas situacionTodoEn100 gema1 gema2 `shouldBe` gemaFusionNoCompatibles1eraGemaDomina
      (flip personalidad situacionTodoEn0.fusionGemas situacionTodoEn100 gema1) gema2 `shouldBe` personalidad gemaFusionNoCompatibles1eraGemaDomina situacionTodoEn0
    it "Si las gemas no son compatibles y la segunda gema es dominante" $ do 
      fusionGemas situacionTodoEn100 gema2 gema1 `shouldBe` gemaFusionNoCompatibles2daGemaDomina
      (flip personalidad situacionTodoEn0.fusionGemas situacionTodoEn100 gema2) gema1 `shouldBe` personalidad gemaFusionNoCompatibles2daGemaDomina situacionTodoEn0
  describe "Test de la funcion fusionGrupal" $ do
    it "Si se fusionan varias gemas" $ do 
      fusionGrupal situacionTodoEn100 gemasAFusionar `shouldBe` gemaFusionDe3
      (flip personalidad situacionTodoEn0.fusionGrupal situacionTodoEn100) gemasAFusionar `shouldBe` personalidad gemaFusionDe3 situacionTodoEn0

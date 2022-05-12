type Onomatopeya = String
type Intensidad = Int
type MojoLaCama = Bool

type Grito = (Onomatopeya, Intensidad, MojoLaCama)

onomatopeya (o,_,_) = o
intensidad (_,i,_) = i
mojoLaCama (_,_,m) = m

grito1 :: Grito
grito1 = ("AAAAAHG", 10, True)

grito2 :: Grito
grito2 = ("uf", 2, False)

-- 1)

energiaDeGrito :: Grito -> Int
energiaDeGrito grito | mojoLaCama grito = (*) ((intensidad grito)^2).nivelDeTerror $ grito
                     | otherwise = (+) (intensidad grito) . (*3) . nivelDeTerror $ grito    

nivelDeTerror :: Grito -> Int
nivelDeTerror grito = length.onomatopeya $ grito

-- 2)

type Nombre = String
type Edad = Int
type Altura = Float

type Ninio = (Nombre, Edad, Altura)

nombre (nombreNiño, _, _) = nombreNiño
edad (_, edadNiño, _) = edadNiño
altura (_, _, alturaNiño) = alturaNiño

ninio1 :: Ninio
ninio1 = ("Franco", 15, 1.4)

type Mounstruo = (Ninio -> Grito)

sullivan :: Mounstruo
sullivan ninio = (gritoGeneradoPorSullivan (length.nombre $ ninio), intensidadGeneradaPorSullivan ninio, haceMojarLaCamaPorSullivan ninio)

gritoGeneradoPorSullivan :: Int -> Onomatopeya
gritoGeneradoPorSullivan longitudNombre = take longitudNombre (repeat 'A') ++ "GH"


intensidadGeneradaPorSullivan :: Ninio -> Intensidad
intensidadGeneradaPorSullivan ninio = (div 20).edad $ ninio

haceMojarLaCamaPorSullivan :: Ninio -> MojoLaCama
haceMojarLaCamaPorSullivan ninio = (<3).edad $ ninio

randallBoggs :: Mounstruo
randallBoggs ninio = ("Mamadera!", intensidadGeneradaPorRandall ninio, haceMojarLaCamaPorRandall ninio)

intensidadGeneradaPorRandall :: Ninio -> Intensidad
intensidadGeneradaPorRandall ninio = length . filter esVocal . nombre $ ninio

esVocal :: Char -> Bool
esVocal letra = elem letra "AEIOUaeiou"

haceMojarLaCamaPorRandall :: Ninio -> MojoLaCama
haceMojarLaCamaPorRandall ninio = ((>0.8).altura $ ninio) && ((<1.2).altura $ ninio)

chuckNorris :: Mounstruo
chuckNorris ninio = ("abcdefghijklmnopqrstuvwxyz", 1000, True)

ositoCarinioso :: Mounstruo
ositoCarinioso ninio = ("uf", edad ninio, False)

-- 3)

pam :: [(a -> b)] -> a -> [b]
pam funciones valor = map ($ valor) funciones

-- 4)

gritos :: [Mounstruo] -> Ninio -> [Grito]
gritos mounstruos ninio = pam mounstruos ninio

-- 5)

hacerGritarCampamento :: [Mounstruo] -> [Ninio] -> [Grito]
hacerGritarCampamento mounstruos ninios = concat(map (gritos mounstruos) ninios)

sumatoriaEnergiaGritos :: [Grito] -> Int
sumatoriaEnergiaGritos gritosCampamento = foldr ((+).energiaDeGrito) 0 gritosCampamento

produccionEnergeticaGritos :: [Mounstruo] -> [Ninio] -> Int
produccionEnergeticaGritos mounstruos ninios = sumatoriaEnergiaGritos . hacerGritarCampamento mounstruos $ ninios

-- 6)
type Duracion = Int

type Risa = (Duracion, Intensidad)

duracionRisa (d,_) = d 
intensidadRisa (_,i) = i

type Comediante = (Ninio -> Risa)

capusotto :: Comediante
capusotto ninio = ((*2).edad $ ninio, (*2).edad $ ninio)

energiaDeRisa :: Risa -> Int
energiaDeRisa risa = (duracionRisa risa) ^ (intensidadRisa risa)

sumatoriaEnergiaRisas :: [Risa] -> Int
sumatoriaEnergiaRisas risasCampamento = foldr ((+).energiaDeRisa) 0 risasCampamento

hacerReirCampamento :: [Comediante] -> [Ninio] -> [Risa]
hacerReirCampamento comediantes ninios = concat(map (risas comediantes) ninios)

risas :: [Comediante] -> Ninio -> [Risa]
risas comediantes ninio = pam comediantes ninio

produccionEnergeticaRisas :: [Comediante] -> [Ninio] -> Int
produccionEnergeticaRisas comediantes ninios = sumatoriaEnergiaRisas . hacerReirCampamento comediantes $ ninios

-- 7)

sumatoriaEnergia energiaProduccion produccionCampamento = foldr ((+).energiaProduccion) 0 produccionCampamento

extraerEnergiaCampamento productores ninios = concat(map (risas productores) ninios)

produccionEnergetica productores ninios energiaProduccion = sumatoriaEnergia energiaProduccion . extraerEnergiaCampamento productores $ ninios
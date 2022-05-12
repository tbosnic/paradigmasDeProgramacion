data Animal = Raton {nombre :: String, edad :: Float, peso :: Float, enfermedades :: [String]} deriving Show

enfermedadesInfecciosas = [ "brucelosis", "tuberculosis"]

cerebro = Raton "Cerebro" 9.0 0.2 ["brucelosis", "sarampión", "tuberculosis"]
orejudo = Raton "Orejudo" 4.0 10.0 ["obesidad", "sinusitis"]

 -- 1)

modificarNombre :: (String -> String) -> Animal -> Animal
modificarNombre funcionNombre raton = raton {nombre = funcionNombre.nombre $ raton)}

modificarEdad :: (Float -> Float) -> Animal -> Animal
modificarEdad funcionEdad raton = raton {edad = funcionEdad.edad $ raton}

modificarPeso :: (Float -> Float) -> Animal -> Animal
modificarPeso funcionPeso raton = raton {peso = funcionPeso.peso $ raton}

modificarEnfermedades :: ([String] -> [String]) -> Animal -> Animal
modificarEnfermedades funcionEnfermedades raton = raton {enefermedades = funcionEnfermedades.enfermedades $ raton}

-- 2)

hierbaBuena :: Animal -> Animal
hierbaBuena raton = modificarEdad sqrt raton

hierbaVerde :: String -> Animal -> Animal
hierbaVerde enfermedad raton = modificarEnfermedades (filter ((/=) enfermedad)) raton

alcachofa :: Animal -> Animal
alcachofa raton = modificarPeso . (perderPeso . peso $ raton) $ raton

perderPeso :: Float -> Float 
perderPeso peso | peso > 2 = (*0.9)
                | otherwise = (*0.95)

hierbaMagica :: Animal -> Animal
hierbaMagica raton = modificarEdad (*0).modificarEnfermedades (const []) $ raton

-- 3)

medicamento :: [(Animal -> Animal)] -> Animal -> Animal
medicamento hierbas raton = foldr $ raton hierbas

antiAge :: Animal -> Animal
antiAge raton = medicamento (replicate 3 hierbaBuena ++ [alcachofa]) raton

reduceFatFast :: Int -> Animal -> Animal
reduceFatFast potencia raton = medicamento ([hierbaVerde "obsedidad"] ++ (replicate potencia alcachofa)) raton   

-- CHEQUEAAAAR!!!!

hierbaMilagrosa :: Animal -> Animal
hierbaMilagrosa raton = foldr hierbaVerde raton enfermedadesInfecciosas

-- 4)

cantidadIdeal :: (Num a) => (a -> Bool) -> a
cantidadIdeal condicion = head.filter condicion $ [1..]

estanMejoresQueNunca :: [Animal] -> (Animal -> Animal) -> Bool
estanMejoresQueNunca ratones medicamento = all ((<1).peso.medicamento) ratones

experimento :: [Animales] -> Int
experimento ratones = cantidadIdeal (estanMejoresQueNunca ratones.reduceFatFast)

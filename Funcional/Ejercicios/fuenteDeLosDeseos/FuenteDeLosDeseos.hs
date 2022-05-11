import Text.Show.Functions()
type Habilidad = String
type NivelFelicidad = Int
type Suenio = Persona -> Persona
type Carrera = String
type Ciudad = String
type Cantidad = Int
type Fuente = Persona -> Persona
type Criterio = Persona -> Int

data Persona = Persona {
 edad :: Int,
 sueniosQueCumplir :: [Suenio],
 nombre :: String,
 felicidonios :: NivelFelicidad,
 habilidad :: [Habilidad]
} deriving Show

juan :: Persona 
juan = Persona {
    edad = 25,
    sueniosQueCumplir = [recibirseDeUnaCarrera "fisica", viajarCiudades ["La Paz", "Bogota"]],
    nombre = "juan",
    felicidonios = 101,
    habilidad = ["Pintar"]
}

evangelina :: Persona
evangelina = Persona {
    edad = 26,
    sueniosQueCumplir = [recibirseDeUnaCarrera "ingenieria", viajarCiudades ["Buenos Aires", "Madrid"]],
    nombre = "evangelina",
    felicidonios = 100,
    habilidad = ["Decir palindromos"]
}

maximiliano :: Persona
maximiliano = Persona {
    edad = 30,
    sueniosQueCumplir = [recibirseDeUnaCarrera "psicologia"],
    nombre = "maximiliano",
    felicidonios = 50,
    habilidad = ["Tocar la guitarra"]
}

melina :: Persona
melina = Persona {
    edad = 19,
    sueniosQueCumplir = [viajarCiudades ["Bogota"], enamorarse juan],
    nombre = "melina",
    felicidonios = 14,
    habilidad = ["Cantar"]
}

ariel :: Persona
ariel = Persona {
    edad = 19,
    sueniosQueCumplir = [enamorarse melina],
    nombre = "ariel",
    felicidonios = 12,
    habilidad = ["Programar", "Cocinar"]
}

-------------------------------------------------------------------------------------------------------
-- Punto 1 - Parte a (integrante 1): Coeficiente de satisfacción
-- Saber el coeficiente de satisfacción de una persona
-- ●	Si los felicidonios son > a 100, son los felicidonios * la edad
-- ●	Si los felicidonios son <= a 100 y > 50, son la cantidad de sueños * los felicidonios
-- ●	En caso contrario, es la división entera de los felicidonios por 2

totalSuenios :: Persona -> Int
totalSuenios persona = length (sueniosQueCumplir persona)

coeficienteDeSatisfaccion :: Persona -> Int
coeficienteDeSatisfaccion persona | (>100) (felicidonios persona) = (felicidonios persona * edad persona) 
                                  | (>50) (felicidonios persona) = (felicidonios persona * totalSuenios persona)
                                  | otherwise = div (felicidonios persona) 2

---------------------------------------------------------------------------------------------------------
-- Punto 1 - Parte b (integrante 2): Grado de ambición de una persona
-- Saber el grado de ambición de una persona
-- ●	Si los felicidonios son > 100, el grado de ambición son los felicidonios * la cantidad de sueños
-- ●	Si los felicidonios son <= 100 y > 50, será la edad * la cantidad de sueños
-- ●	En caso contrario, serán la cantidad de sueños * 2

gradoDeAmbicion :: Persona -> Int
gradoDeAmbicion persona   | (>100) (felicidonios persona) = (felicidonios persona  * totalSuenios persona)
                          | (>50) (felicidonios persona) = (edad persona * totalSuenios persona)
                          | otherwise = (totalSuenios persona) * 2 

-------------------------------------------------------------------------------------------------------------
-- Punto 2 - Parte a (Integrante 1): Nombre largo
-- Saber si una persona tiene un nombre largo, de más de 10 caracteres
    
nombreLargo :: Persona -> Bool
nombreLargo persona = (>10).length.nombre $ persona

----------------------------------------------------------------------------------------
-- Punto 2 - Parte b (Integrante 2): Persona suertuda
-- Saber si una persona es suertuda, que como todos sabemos esto
-- se cumple si el triple de su coeficiente de satisfacción es par.

personaSuertuda :: Persona -> Bool
personaSuertuda persona = even.(* 3).coeficienteDeSatisfaccion $ persona

-----------------------------------------------------------------------------------------
-- Punto 2 - Parte c (Integrante 3): Nombre lindo
-- Saber si una persona tiene un nombre lindo, esto es que su última 
-- letra termine en 'a'.

nombreLindo :: Persona -> Bool
nombreLindo persona = (== 'a').last.nombre $ persona

-----------------------------------------------------------------------------------------------
-- Punto 3 - Parte a (Integrante 1): Recibirse de una carrera
-- Esto le da 1000 felicidonios por cada letra de la carrera y le agrega la carrera como habilidad.
-- Ej: "arquitectura" le suma 12000 felicidonios.

sumoFelicidonio :: NivelFelicidad -> Persona -> Persona
sumoFelicidonio felicidoniosExtra persona = persona {felicidonios = (felicidonios persona) + felicidoniosExtra}

agregoHabilidad :: Habilidad -> Persona -> Persona
agregoHabilidad habilidadExtra persona = persona {habilidad = (habilidad persona) ++ [habilidadExtra]}

recibirseDeUnaCarrera :: Carrera -> Suenio
recibirseDeUnaCarrera carrera persona = sumoFelicidonio ((*1000).length $ carrera).(agregoHabilidad carrera) $ persona

-------------------------------------------------------------------------------------------------------------
-- Punto 3 - Parte b (Integrante 2): Viajar a una lista de ciudades
-- Suma 100 felicidonios por cada ciudad que visita,
-- en el interín pasa un año (la persona tendrá un año más luego de viajar).

cumplirUnAnio :: Persona -> Persona
cumplirUnAnio persona = persona {edad = (edad persona) + 1}

viajarCiudades ::  [Ciudad] -> Suenio 
viajarCiudades ciudades persona = sumoFelicidonio ((*100).length $ ciudades).cumplirUnAnio $ persona

---------------------------------------------------------------------------------------------------------------
-- Punto 3 - Parte c (Integrante 3): Enamorarse de otra persona
-- Por lo que suma los felicidonios que esta persona tenga. 
-- El sueño no es bidireccional, que X se enamore de Y no implica lo mismo para Y.

enamorarse :: Persona -> Suenio
enamorarse personaDeLaCualSeEnamoran personaQueSeEnamora = (sumoFelicidonio.felicidonios $ personaDeLaCualSeEnamoran) personaQueSeEnamora

-----------------------------------------------------------------------------------------------------------------------
-- Punto 3 - Común a todos los integrantes: Conformistas
-- Para los conformistas, el sueño “que todo siga igual”, que mantiene a la persona sin cambios.

todoSigueIgual :: Persona -> Persona
todoSigueIgual = id

--------------------------------------------------------------------------------------------------------------------
-- Punto 3 - Común a todos los integrantes: Combo perfecto
-- Se recibe de la carrera de "Medicina", viaja a "Berazategui" y "París"
-- y como bonus extra suma 100 felicidonios por el combo. Definirlo únicamente con funciones existentes.

comboPerfecto :: Persona -> Persona
comboPerfecto persona =  ((viajarCiudades ["Berazategui", "Paris"]).(recibirseDeUnaCarrera "medicina").(sumoFelicidonio 100)) $ persona

--------------------------------------------------------------------------------------------------------------------
-- Punto 4 - Parte a (Integrante 1): Fuente minimalista
-- La fuente minimalista le cumple el primer suenio a la persona, y lo quita de la lista de suenios de
-- esa persona.

quitoPrimerSuenio :: Persona -> Persona
quitoPrimerSuenio persona = persona {sueniosQueCumplir = tail (sueniosQueCumplir persona)}

tomoPrimerSuenio :: Persona -> Suenio
tomoPrimerSuenio persona = head.sueniosQueCumplir $ persona

fuenteMinimalista :: Persona -> Persona
fuenteMinimalista persona = quitoPrimerSuenio.(tomoPrimerSuenio persona) $ persona


--------------------------------------------------------------------------------------------------------------------
-- Punto 4 - Parte b (Integrante 2): Fuente copada
-- La fuente copada le cumple todos los suenios a la persona. La persona debe quedar sin suenios.

sinSuenios :: Fuente
sinSuenios persona = persona {sueniosQueCumplir = []}

cumplirSuenios :: Fuente
cumplirSuenios persona = (foldl (flip($)) persona).sueniosQueCumplir $ persona

fuenteCopada :: Fuente
fuenteCopada persona = sinSuenios.cumplirSuenios $ persona  

--------------------------------------------------------------------------------------------------------------------
-- Punto 4 - Parte c (Integrante 3): Fuente a pedido
-- La fuente a pedido le cumple el enesimo suenio a la persona, pero no lo quita de la lista de suenios.

fuenteAPedido :: Int -> Persona -> Persona
fuenteAPedido posicionEnesimoSuenio persona = (!!) (sueniosQueCumplir persona) (posicionEnesimoSuenio - 1) $ persona

--------------------------------------------------------------------------------------------------------------------
-- Punto 4 - Parte d (Comun a todos los integrantes): Fuente sorda
-- Como no entiende bien que suenio tiene que cumplir no le cumple ninguno. Incluir el o los ejemplos de invocacion
-- y respuesta que sean necesarios.

fuenteSorda :: Persona -> Persona
fuenteSorda persona = todoSigueIgual persona

--------------------------------------------------------------------------------------------------------------------
-- Punto 5:
-- Dada una lista de fuentes y una persona, saber cual es la fuente "ganadora" en base a un criterio.
-- Por ejemplo: 
--  1. el que mas felicidonios le de a esa persona cuando lo cumpla (integrante 1).
--  2. el que menos felicidonios le de a esa persona cuando lo cumpla (integrante 2).
--  3. el que mas habilidades le deje a esa persona cuando lo cumpla (integrante 3).
-- Cada integrante debe dar ejemplos de invocacion y respuesta de como invocar a esa funcion para resolver ese
-- requerimiento.

-- maximoSegun :: [Fuente] -> Persona -> Criterio -> Fuente
-- maximoSegun [fuente] _ _ = fuente
-- maximoSegun (fuente1:fuente2:fuentes) persona criterio | (criterio(fuente1 persona)) > (criterio(fuente2 persona)) = maximoSegun (fuente1:fuentes) persona criterio
--                                                       | otherwise = maximoSegun (fuente2:fuentes) persona criterio

fuenteMaximaSegun :: Persona -> Criterio -> Fuente -> Fuente -> Fuente
fuenteMaximaSegun persona criterio fuente1 fuente2 | ((criterio.fuente1) $ persona) > ((criterio.fuente2) $ persona) = fuente1
                                                   | otherwise = fuente2

fuenteGanadora :: [Fuente] -> Persona -> Criterio -> Fuente
fuenteGanadora fuentes persona criterio =  foldl1 (fuenteMaximaSegun persona criterio) fuentes

--------------------------------------------------------------------------------------------------------------------
-- Punto 6 - Integrante 1:
-- Saber que suenios son valiosos para una persona, son aquellos que al cumplirlos la persona queda con mas de
-- 100 felicidonios.

sueniosValiosos :: Persona -> [Suenio]
sueniosValiosos persona = filter ((>100).felicidonios.((flip($)) persona)) (sueniosQueCumplir persona)

--------------------------------------------------------------------------------------------------------------------
-- Punto 6 - Integrante 2:
-- Saber si algun suenio de una persona es raro, que es el que lo deja con la misma cantidad de felicidonios tras
-- cumplirlo.

suenioRaro :: Persona -> Bool
suenioRaro persona = any ((== (felicidonios persona)).felicidonios.((flip($)) persona)) (sueniosQueCumplir persona)

--------------------------------------------------------------------------------------------------------------------
-- Punto 6 - Integrante 3:
-- Dada una lista de personas, poder conocer la felicidad total de ese grupo si cumplen todos sus suenios. 
-- Tip: aprovecharse de alguna de las fuentes definidas anteriormente.


felicidadTotal :: [Persona] -> NivelFelicidad
felicidadTotal personas = foldr ((+).felicidonios.fuenteCopada) 0 personas

--------------------------------------------------------------------------------------------------------------------
-- Punto 7:
-- Modelar una persona con suenios infinitos. Para cada integrante, teniendo en cuenta el
-- requerimiento que le tocó al modelar la fuente en el punto 4, ¿es posible que la fuente pueda
-- satisfacer a esa persona que tiene infinitos suenios? Justifique su respuesta con un ejemplo
-- concreto: “a esta persona P0 con infinitos suenios S0 y la Fuente F1 la invoco en la consola y...
-- (etc. etc. etc.)” y relaciónelo con algún concepto visto en la cursada.

sueniosInfinitos :: [Suenio]
sueniosInfinitos = (recibirseDeUnaCarrera "abogacia") : sueniosInfinitos

personaSueniosInfinitos :: Persona -> Persona
personaSueniosInfinitos persona = persona {sueniosQueCumplir = (sueniosQueCumplir persona) ++ sueniosInfinitos}










-- EJEMPLOS DE INVOCACION Y RESPUESTA:
-- juan = Persona 25 [recibirseDeUnaCarrera "fisica", viajarCiudades ["La Paz", "Bogota"]] "juan" 101 ["Pintar"]
-- evangelina = Persona 26 [recibirseDeUnaCarrera "ingenieria", viajarCiudades ["Buenos Aires", "Madrid"]] "evangelina" 100 ["Decir palíndromos"]
-- maximiliano = Persona 30 [recibirseDeUnaCarrera "psicologia"] "maximiliano" 50 ["Tocar la guitarra"]
-- melina = Persona 19 [viajarCiudades ["Bogota"], enamorarse juan] "melina" 14 ["Cantar"]
-- ariel = Persona 19 [enamorarse melina] "ariel" 12 ["Programar", "Cocinar"]

--Punto 1

--Parte a: Coeficiente de satisfaccion

-- Invocacion = coeficienteDeSatisfaccion juan
-- Resultado = 2525

-- Invocacion = coeficienteDeSatisfaccion evangelina
-- Resultado = 200

-- Invocacion = coeficienteDeSatisfaccion maximiliano
-- Resultado = 25

-- --Parte b: Grado de ambicion de una persona

-- Invocacion = gradoDeAmbicion juan
-- Resultado = 202

-- Invocacion = gradoDeAmbicion evangelina
-- Resultado = 52

-- Invocacion = gradoDeAmbicion maximiliano
-- Resultado = 2

-- --Punto 2

-- --Parte a: Nombre largo

-- Invocacion = nombreLargo evangelina
-- Resultado = False

-- Invocacion = nombreLargo maximiliano
-- Resultado = True

-- --Parte b: Persona suertuda

-- Invocacion = personaSuertuda melina
-- Resultado = False 

-- Invocacion = personaSuertuda ariel
-- Resultado = True

-- --Parte c: Nombre lindo

-- Invocacion = nombreLindo ariel
-- Resultado = False

-- Invocacion = nombreLindo melina
-- Resultado = True

-- --Punto 3: Los sueños son...

-- --Parte a: Recibirse de una carrera

-- Invocacion = recibirseDeUnaCarrera "ingenieria" juan
-- Resultado = Persona {edad = 25, sueniosQueCumplir = [<function>,<function>], nombre = "juan", felicidonios = 10101, habilidades = ["Pintura","ingenieria"]}

-- --Parte b: Viajar a una lista de ciudades

-- Invocacion = viajarCiudades ["La Plata", "Nueva York", "Amsterdam"] evangelina
-- Resultado = Persona {edad = 27, sueniosQueCumplir = [<function>,<function>], nombre = "evangelina", felicidonios = 400, habilidades = ["Decir palindromos"]}

-- --Parte c: Enamorarse de otra persona 

-- Invocacion = enamorarse melina maximiliano
-- Resultado = Persona {edad = 30, sueniosQueCumplir = [<function>], nombre = "maximiliano", felicidonios = 64, habilidades = ["Tocar la guitarra"]}

-- --Comun a todos: que todo siga igual

-- Invocacion = todoSigueIgual melina
-- Resultado = {edad = 19, sueniosQueCumplir = [<function>,<function>], nombre = "melina", felicidonios = 14, habilidades = ["Cantar"]}

-- --Comun a todos: combo perfecto

-- Invocacion = comboPerfecto ariel
-- Resultado = {edad = 20, sueniosQueCumplir = [<function>], nombre = "ariel", felicidonios = 8312, habilidades = ["Programar","Cocinar","medicina"]}

--Punto 4 

--Parte a: Fuente minimalista

-- *Main> melina
-- Persona {edad = 19, sueniosQueCumplir = [<function>,<function>], nombre = "melina", felicidonios = 14, habilidad = ["Cantar"]}

-- Invocacion = fuenteMinimalista melina
-- Resultado = Persona {edad = 20, sueniosQueCumplir = [<function>], nombre = "melina", felicidonios = 114, habilidad = ["Cantar"]}
-- ----------------------------------------------------------------------------------------------------------------------------------------------------
-- *Main> juan
-- Persona {edad = 25, sueniosQueCumplir = [<function>,<function>], nombre = "juan", felicidonios = 101, habilidad = ["Pintar"]}

-- Invocacion = fuenteMinimalista juan
-- Resultado = Persona {edad = 25, sueniosQueCumplir = [<function>], nombre = "juan", felicidonios = 6101, habilidad = ["Pintar","fisica"]}

-- --Parte b: Fuente copada

-- *Main> evangelina
-- Persona {edad = 26, sueniosQueCumplir = [<function>,<function>], nombre = "evangelina", felicidonios = 100, habilidad = ["Decir palindromos"]}

-- Invocacion = fuenteCopada evangelina
-- Resultado = Persona {edad = 27, sueniosQueCumplir = [], nombre = "evangelina", felicidonios = 10300, habilidad = ["Decir palindromos","ingenieria"]}
-- ----------------------------------------------------------------------------------------------------------------------------------------------------
-- *Main> maximiliano
-- Persona {edad = 30, sueniosQueCumplir = [<function>], nombre = "maximiliano", felicidonios = 50, habilidad = ["Tocar la guitarra"]}

-- Invocacion = fuenteCopada maximiliano
-- Resultado = Persona {edad = 30, sueniosQueCumplir = [], nombre = "maximiliano", felicidonios = 10050, habilidad = ["Tocar la guitarra","psicologia"]}

-- --Parte c: Fuente a pedido

-- *Main> melina
-- Persona {edad = 19, sueniosQueCumplir = [<function>,<function>], nombre = "melina", felicidonios = 14, habilidad = ["Cantar"]}

-- Invocacion = fuenteAPedido 1 melina (El primer sueño es "viajarCiudades ["Bogota"]")
-- Resultado = Persona {edad = 20, sueniosQueCumplir = [<function>,<function>], nombre = "melina", felicidonios = 114, habilidad = ["Cantar"]}
-- ----------------------------------------------------------------------------------------------------------------------------------------------------
-- Invocacion = fuenteAPedido 2 melina (El segundo sueño es "enamorarse juan")
-- Resultado = Persona {edad = 25, sueniosQueCumplir = [<function>,<function>], nombre = "juan", felicidonios = 6101, habilidad = ["Pintar","fisica"]}

-- --Parte d: Fuente sorda

-- *Main> ariel
-- Persona {edad = 19, sueniosQueCumplir = [<function>], nombre = "ariel", felicidonios = 12, habilidad = ["Programar","Cocinar"]}

-- Invocacion = fuenteSorda ariel
-- Resultado = Persona {edad = 19, sueniosQueCumplir = [<function>], nombre = "ariel", felicidonios = 12, habilidad = ["Programar","Cocinar"]}
-- ----------------------------------------------------------------------------------------------------------------------------------------------------
-- *Main> juan
-- Persona {edad = 25, sueniosQueCumplir = [<function>,<function>], nombre = "juan", felicidonios = 101, habilidad = ["Pintar"]}

-- Invocacion = fuenteSorda juan
-- Resultado = Persona {edad = 25, sueniosQueCumplir = [<function>,<function>], nombre = "juan", felicidonios = 101, habilidad = ["Pintar"]}

-- --Punto 5

-- --Integrante 1:  el que mas felicidonios le de a esa persona cuando lo cumpla

-- Invocacion = maximoSegun [fuenteMinimalista, fuenteCopada, fuenteSorda] juan felicidonios
-- Resultado = <function>

-- --Integrante 2:  el que menos felicidonios le de a esa persona cuando lo cumpla

-- Invocacion = maximoSegun [fuenteMinimalista, fuenteCopada, fuenteSorda] juan (negate.felicidonios)
-- Resultado = <function>

-- --Integrante 3:  el que mas habilidades le deje a esa persona cuando lo cumpla

-- Invocacion = maximoSegun [fuenteMinimalista, fuenteCopada, fuenteSorda] juan (length.habilidad) 
-- Resultado = <function>

-- --Punto 6: Reportes

-- --Integrante 1: Saber qué sueños son valiosos

-- *Main> maximiliano
-- Persona {edad = 30, sueniosQueCumplir = [<function>], nombre = "maximiliano", felicidonios = 50, habilidad = ["Tocar la guitarra"]}

-- Invocacion = sueniosValiosos maximiliano
-- Resultado = [<function>]
-- ----------------------------------------------------------------------------------------------------------------------------------------------------
-- *Main> juan
-- Persona {edad = 25, sueniosQueCumplir = [<function>,<function>], nombre = "juan", felicidonios = 101, habilidad = ["Pintar"]}

-- Invocacion = sueniosValiosos juan
-- Resultado = [<function>,<function>]

-- --Integrante 2: Saber si algún sueño de una persona es raro

-- *Main> evangelina
-- Persona {edad = 26, sueniosQueCumplir = [<function>,<function>], nombre = "evangelina", felicidonios = 100, habilidad = ["Decir palindromos"]}

-- Invocacion = suenioRaro evangelina
-- Resultado = False
-- ----------------------------------------------------------------------------------------------------------------------------------------------------
-- *Main> juan
-- Persona {edad = 25, sueniosQueCumplir = [<function>,<function>], nombre = "juan", felicidonios = 101, habilidad = ["Pintar"]}

-- Invocacion = suenioRaro juan
-- Resultado = False

-- --Integrante 3: Dada una lista de personas, poder conocer la felicidad total de ese grupo si cumplen todos sus sueños.  

-- Invocacion = felicidadTotal [juan]
-- Resultado = 6301

-- Invocacion = felicidadTotal [melina]
-- Resultado = 215

-- Invocacion = felicidadTotal [evangelina]
-- Resultado = 10300

-- Invocacion = felicidadTotal  [juan, melina, evangelina]
-- Resultado = 16816

-- --Punto 7: Modelar una persona con sueños infinitos

-- Concepto visto en clase relacionado con este punto:
-- EVALUACION DIFERIDA. Los argumentos se evaluan a medida que se los va necesitando.

-- --Parte a: Fuente minimalista

-- *Main> melina
-- Persona {edad = 19, sueniosQueCumplir = [<function>,<function>], nombre = "melina", felicidonios = 14, habilidad = ["Cantar"]}

-- Invocacion = fuenteMinimalista (personaSueniosInfinitos melina)
-- Resultado = Persona {edad = 20, sueniosQueCumplir = [<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function> ...

-- Conclusion: La fuente satisface a la persona con suenios infinitos. Se modela a la persona "melina" con suenios infinitos, y cuando se le aplica la fuente 
-- "fuenteMinimalista", la misma toma el primer suenio de melina, se lo cumple, y devuelve a "melina" con su primer suenio cumplido, el cual era viajar a Bogota (se
-- puede comprobar que se cumplio el suenio observando que la edad de melina aumenta). Sin embargo, la ejecucion del programa tiene que ser interrumpida por el usuario,
-- ya que se queda imprimiendo en pantalla los infinitos suenios restantes de melina.

-- --Parte b: Fuente copada

-- Invocacion = fuenteMinimalista (personaSueniosInfinitos melina)
-- Resultado = -

-- Conclusion: La fuente no puede satisfacer a la persona con suenios infinitos.  Se modela a la persona "melina" con suenios infinitos, y cuando se le aplica la fuente 
-- "fuenteCopada", la misma se queda cumpliendo infinitamente los infinitos suenios de "melina", por lo tanto, el programa nunca imprime nada en pantalla, y tiene que ser
-- interrumpido por el usuario.

-- --Parte c: Fuente a pedido

-- Invocacion = fuenteAPedido 99 (personaSueniosInfinitos melina)
-- Resultado = Persona {edad = 19, sueniosQueCumplir = [<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function> ...

-- Conclusion: La fuente satisface a la persona con suenios infinitos. Se modela a la persona "melina" con suenios infinitos, y cuando se le aplica la fuente 
-- "fuenteAPedido", la misma toma el suenio numero 99 de "melina", el cual es uno de los suenios que se agrego a "melina" al modelarla con suenios infinitos 
-- (recibirseDeUnaCarrera "abogacia"), se lo cumple, y devuelve a "melina" con ese suenio cumplido. Sin embargo, la ejecucion del programa tiene que ser interrumpida por 
-- el usuario, ya que se queda imprimiendo en pantalla los infinitos suenios restantes de melina.

-- --Parte d: fuente sorda

-- Invocacion = fuenteSorda (personaSueniosInfinitos melina)
-- Resultado = Persona {edad = 19, sueniosQueCumplir = [<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function>,<function> ...

-- Conclusion: La fuente satisface a la persona con suenios infinitos. Se modela a la persona "melina" con suenios infinitos, y cuando se le aplica la fuente 
-- "fuenteSorda", la misma no cumple ninguno de los suenios, pero devuelve a la misma persona con suenios infinitos. Sin embargo, la ejecucion del programa tiene que 
-- ser interrumpida por el usuario, ya que se queda imprimiendo en pantalla los infinitos suenios restantes de melina.
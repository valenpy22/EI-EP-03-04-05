# 1. En su desquiciada investigación para acabar con los vampiros, Van Helsing ha descubierto que sus
# enemigos tienen predilección por la sangre humana tipo AII+. El cazador sospecha que estos monstruos
# tienen preferencia por la sangre de los adultos, pero aún no está convencido. Por esta razón, mediante
# artimañas, ha encerrado a 14 niños y 20 adultos con este tipo de sangre en un reducto de vampiros. Tras
# 3 noches, 4 de los niños y 11 de los adultos fueron atacados. ¿Existe relación entre el ataque de vampiros
# y la edad de la víctima?

# Variables:
# Mayoría De edad: dicotómica (mayor_de_edad, menor_de_edad)
# Mordida: dicotómica (mordido, no_mordido)

# Se hace uso del test de fisher ya que se trata de una prueba no paramétrica, 
# y se considera que las variables son independientes. Se asume la independencia
# de los datos debido a que el tamaño de la muestra es menor al 10% de la población
# de niños y adultos.
# Además de que las variables son dicotómicas y se trata de una muestra pequeña, 
# el valor de las frecuencias esperadas de chi cuadradro es equivalente a 6 -lo que es
# cercano a 5- en uno de los cuadrantes de la tabla, y como se busca realizar una prueba rigurosa
# se determina realizar un test de fisher.

# datos
no_mordido <- c(10,9)
mordido <- c(4,11)
tabla <- as.table (rbind (no_mordido,mordido))

# chi-square
prueba = chisq.test(tabla)
esperados = prueba[["expected"]]
print(esperados)

# Primeramente se postulan las hipótesis nula y alternativa:
# H0: No existe relación entre la edad de la víctima y el ataque de vampiros.
# Ha: Existe relación entre la edad de la víctima y el ataque de vampiros.
# Se considera un nivel de significación alfa = 0.05.

dimnames (tabla) <- list (
  Mordedura = c("No Mordido", "Mordido"),
  Mayoria_de_edad= c("Niño", "Adulto"))

Mordedura = c("No Mordido", "Mordido")


prueba <- fisher.test (tabla, 1-alfa)
print(prueba)

# Como p-value = 0.17
# se falla al rechazar la hipótesis nula y se concluye que no hay evidencia suficiente 
# para creer que existe una relación entre la edad de la víctima y el ataque de vampiros.


# 2. Una Universidad ha detectado que muchos de sus nuevos estudiantes ingresan 
# con elevados niveles de ansiedad. Para ello, han decidido evaluar un nuevo 
# programa de bienvenida que busca facilitar la adaptación a la vida universitaria. 
# Para ello, han reclutado a un grupo de 15 voluntarios a quienes se les
# midió el nivel de ansiedad (alto o bajo) antes y después de participar en el 
# programa de bienvenida:
# - 2 estudiantes no presentaron ansiedad ni antes ni después.
# - 9 estudiantes inicialmente ansiosos dejaron de estarlo.
# - 3 estudiantes mantuvieron un elevado nivel de ansiedad.
# - El estudiante restante desarrolló síntomas de ansiedad tras participar en el programa.

# ¿Qué se puede concluir acerca del nuevo programa de bienvenida?
 
# Se empleará la prueba de mcNemar, debido a que la muestra es pequeña
# y se mide en dos ocasiones diferentes cierta respuesta dicotómica para los
# mismos sujetos, queriendo ver si se produce un cambio significativo entre ambas
# mediciones.

# H0: No hay cambios significativos en las respuestas
# Ha: Sí hay cambios significativos en las respuestas


# Tabla de referencia:

#           modelo_1
#           Alta Baja
#     Alta   3    1 
#     Baja   9    2


estudiante <- seq(1:15)
modelo_1 <- c(rep("Alta", 12), rep("Baja", 3))
modelo_2 <- c(rep("Alta", 3), rep("Baja", 11), rep("Alta", 1))
datos <- data.frame(estudiante, modelo_2, modelo_1)
tabla <- table(modelo_2, modelo_1)
print(tabla)

# Aplicar prueba de mcNemar
prueba <- mcnemar.test(tabla)
print(prueba)

# Teniendo alfa = 0.05 y p = 0.02686, al ser p < alfa
# se rechaza la hipótesis nula en favor de 
# la hipótesis alternativa con un 95% de confianza. 
# Por lo tanto, se concluye que hay evidencia suficiente para 
# creer que existe una diferencia de niveles de ansiedad
# antes y después de que los estudiantes participaran en el
# programa de bienvenida.


# 3. En noviembre de 2019, se realizó un estudio acerca de la aprobación al presidente Sebastián Piñera entre
# 440 profesores y estudiantes de una prestigiosa universidad, obteniéndose los resultados que se muestran
# en la tabla. ¿Son similares las opiniones de ambos segmentos de la comunidad universitaria?  

# Primeramente se determina el valor de las frecuencias esperadas.
# En base al análisis de las frecuencias esperados se determina que se puede utilizar el test de chi cuadrado,
# ya que se cumple la condición de que este valor es mayor a 5 en todos los cuadrantes de la tabla y 
# además se está realizando una prueba de homogeneidad para determinar si dos poblaciones tienen la 
# misma proporción de aprobación del presidente Sebastián Piñera.

# Hipótesis nula y alternativa:
# H0: Profesores y estudiantes presentan la misma preferencia.
# H1: Profesores y estudiantes no presentan la misma preferencia.

# Se considera un nivel de significación de 0.05.


# datos
esperados <- round(prueba[["expected"]], 3)  

estudiantes <- c(35,208,17)
profesores <- c(20,157,3)

tabla <- as.table(rbind(estudiantes, profesores))  
print(tabla)

dimnames(tabla) <- list(rol= c("Estudiantes", "Profesores"), opciones = c("Aprueba", "Desaprueba", "Ninguna"))

# prueba chi cuadrado
prueba = chisq.test(tabla)
print(prueba)

# Como p-value = 0.03521
# Se rechaza la hipótesis nula en favor de la hipótesis alternativa y se concluye que hay evidencia
# suficiente para creer que existe una diferencia en la preferencia de
# ambos segmentos de la comunidad universitaria.


# 4.La Facultad de Ingeniería desea saber si existe diferencia significativa en el desempeño de los estudiantes 
# en asignaturas críticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3 asignaturas, 
# indica si una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir la Facultad? Indicación: 
# obtenga la muestra a partir del archivo “EP04 Datos.csv” que se encuentra en el directorio compartido, 
# usando la semilla 555. Considere un nivel de significación α=0,05.

# Debido a que la variable independiente (cada alumno) tiene más de 2 observaciones
# pareadas (3 asignaturas) se utilizará la prueba Q de Cochran

# H0: No hay diferencia significativa en el desempeño de los estudiantes
# Ha: Sí hay diferencia significativa en el desempeño de los estudiantes

# librerías y semilla
library(tidyverse)
library(readxl)
library(RVAideMemoire)
library(rcompanion)
set.seed(555)

# obtener datos
excel <- read_excel("EP04 Datos.xls")
muestra <- excel[sample(x = nrow(excel), size =50),] # muestra de 50 alumnos elegidos al azar

calculo <- muestra$Calculo
fisica <- muestra$Fisica
algebra <- muestra$Algebra

calculo <- ifelse(calculo=="R",0,1)
fisica <- ifelse(fisica=="R",0,1)
algebra <- ifelse(algebra=="R",0,1)

instancia <- 1:50

datos <- data.frame(instancia,calculo,fisica,algebra)
datos <- datos %>% pivot_longer(c('calculo','fisica','algebra'),names_to="estudiantes",values_to="resultado")

datos[["instancia"]] <- factor(datos[["instancia"]])
datos[["estudiantes"]] <- factor(datos[["estudiantes"]])

# prueba Q de Cochran
prueba <- cochran.qtest(resultado ~ estudiantes | instancia, data=datos, alpha=0.05)
print(prueba)

# Con un p-value = 0,11 mayor a alfa de 0,05 se falla en rechazar la hipótesis nula
# de que no hay diferencia significativa entre estudiantes.
# Por lo tanto, no hay evidencia suficiente para decir que sí hay diferencia significativa 
# en el desempeño de los estudiantes.

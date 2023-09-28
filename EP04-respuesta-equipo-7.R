
# 2. Una Universidad ha detectado que muchos de sus nuevos estudiantes ingresan 
#con elevados niveles de ansiedad. Para ello, han decidido evaluar un nuevo 
# programa de bienvenida que busca facilitar la adaptación a la vida universitaria. 
# Para ello, han reclutado a un grupo de 15 voluntarios a quienes se les
#midió el nivel de ansiedad (alto o bajo) antes y después de participar en el 
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

# Ho: NO hay cambios significativos en las respuestas
# Ha: SÍ hay cambios significativos en las respuestas

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
# Teniendo como alfa = 0.05 y p como 0.02686, como p < alfa,
# Se rechaza la hipótesis nula en favor de 
# la hipótesis alternativa con un 95% de confianza. 
# Por lo tanto, se concluye que hay evidencia suficiente para 
# creer que existe una diferencia de niveles de ansiedad
# antes y después de que los estudiantes participaran en el
# programa de bienvenida.

# 4.La Facultad de Ingeniería desea saber si existe diferencia significativa en el desempeño de los estudiantes 
# en asignaturas críticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3 asignaturas, 
# indica si una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir la Facultad? Indicación: 
# obtenga la muestra a partir del archivo “EP04 Datos.csv” que se encuentra en el directorio compartido, 
# usando la semilla 555. Considere un nivel de significación α=0,05.
library(tidyverse)
library(readxl)
library(RVAideMemoire)
library(rcompanion)
set.seed(555)

excel <- read_excel("EP04 Datos.xls")
muestra <- excel[sample(x = nrow(excel), size =50),] # muestra de 50 alumnos elegidos al azar
#print(muestra)

calculo <- muestra$Calculo
fisica <- muestra$Fisica
algebra <- muestra$Algebra
#print(c)

#print("")
calculo <- ifelse(calculo=="R",0,1)
fisica <- ifelse(fisica=="R",0,1)
algebra <- ifelse(algebra=="R",0,1)

instancia <- 1:50

datos <- data.frame(instancia,calculo,fisica,algebra)
datos <- datos %>% pivot_longer(c('calculo','fisica','algebra'),names_to="estudiantes",values_to="resultado")

datos[["instancia"]] <- factor(datos[["instancia"]])
datos[["estudiantes"]] <- factor(datos[["estudiantes"]])

prueba <- cochran.qtest(resltado ~ estudiantes | instancia, data=datos,alpha=0.05)
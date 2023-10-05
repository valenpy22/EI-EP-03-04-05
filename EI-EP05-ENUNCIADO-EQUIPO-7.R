# Un equipo de investigadores del área de interacción humano-información está 
# estudiando si el área temática y el nivel de dificultad del problema de 
# información influyen en el tiempo (en segundos) que toma un usuario en 
# formular una consulta de búsqueda para resolver dicho problema. Para ello, 
#han reclutado a un grupo de participantes voluntarios, asignados 
# aleatoriamente a distintos grupos. Cada participante debe resolver 
# tres problemas de información con diferentes niveles de dificultad: 
# baja, media y alta. A su vez, cada grupo debe resolver problemas 
# relacionados a una temática diferente. 

# Los datos recolectados contemplan las siguientes variables:
# - id: 	identificador único de cada participante.
# - area: 	Área temática de los problemas que el participante debe 
# responder. Variable categórica con los niveles Arquitectura, Biología, 
# Computación, Economía, Física, Leyes, Literatura, Matemáticas, Música, 
# Pedagogía, Psicología, Química.
# - dificultad 	Nivel de dificultad del problema resuelto. Variable 
# categórica con los niveles Baja, Media y Alta.
# - tiempo 	Tiempo, en segundos, que toma al participante formular la consulta.

# En este momento, los investigadores buscan determinar si existen diferencias 
# en el tiempo que tardan los usuarios en formular una consulta para un 
# problema de dificultad fácil en las áreas de biología, leyes y psicología.

libraries <- c("ggpubr", "PASWR2", "tidyverse","dplyr")

lapply(libraries, function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
})

# Se importan los datos
data <- read.csv("Desktop/EP03/EP05 Datos.csv")

# Formulación de hipótesis
# H0: No hay diferencias entre las medias de los grupos.
#     Es decir, mu_a = mu_b = mu_c
# Ha: Sí hay diferencias entre las medias de los grupos.
#     Es decir, mu_a != mu_b o mu_a = mu_c o mu_b = mu_c

# Se filtran los datos según la dificultad de la prueba. En este caso, 
# se buscan las pruebas de dificultad baja.
filtered_by_low <- data[data$dificultad == "Baja",]

# Se filtra nuevamente según el área, consiguiéndose un vector de los tiempos
# en que se demoró cada usuario en realizar la prueba.
biology <- filtered_by_low[filtered_by_low$area == "Biología", ]$tiempo
law <- filtered_by_low[filtered_by_low$area == "Leyes", ]$tiempo
psychology <- filtered_by_low[filtered_by_low$area == "Psicología", ]$tiempo

# Se hace el dataframe donde se colocan los tiempos ya extraídos de cada área
datos <- data.frame(biology, law, psychology)

datos <- datos  %>% pivot_longer(c("biology", "law", "psychology"),
                                 names_to = "area",
                                 values_to = "tiempo")

datos[["area"]] <- factor(datos[["area"]])
datos[["usuario"]] <- factor(1:nrow(datos))

# Se hace la prueba ANOVA debido a que se cumplen las siguientes condiciones:
# - 1. La escala con que se mide la variable dependiente tiene las propiedades
#      de una escala de intervalos iguales.
# - 2. Las k muestras son obtenidas de manera aleatoria e independiente desde
#      sus respectivas poblaciones.
# - 3. Las k muestras son obtenidas desde poblaciones que siguen una distribución normal.
#      Esto se comprueba utilizando el método eda(), donde se puede ver que las 
#      3 muestras siguen una distribución normal.
# - 4. Las k muestras tienen varianzas iguales en el caso de que provengan de distintas 
#      poblaciones.

# Según los gráficos, se puede ver que existen pocos valores atípicos por lo que
# se elige usar un nivel de significancia del 0.025.
eda(biology)
eda(psychology)
eda(law)

# Se realiza la prueba de homocedasticidad:
# H0: Las varianzas son iguales.
# Ha: Las varianzas no son iguales.

# Sacar las varianzas de cada grupo
var_1 <- var(biology)
var_2 <- var(law)
var_3 <- var(psychology)

# Se calcula el máximo y el mínimo, y se saca la razón entre ellos. Si es menor
# a 1.5, se acepta la hipótesis nula, de lo contrario se rechaza.

max_var <- max(var_1, var_2, var_3)
min_var <- min(var_1, var_2, var_3)

ratio <- max_var / min_var

# Se imprime el ratio
print(ratio)

# Se puede ver que el ratio es menor a 1.5 (1.177047), por lo que se acepta la hipótesis
# nula y se puede proceder a realizar la prueba ANOVA.s
anova <- aov(tiempo ~ area, data = datos)

# Se imprime el resumen de la prueba
print(summary(anova))

# Debido a que el p = 3.19e-09 < alfa, se rechaza la hipotesis nula a 
# favor de la hipotesis alternativa y esto a la vez quiere decir que 
# existen diferencias entre los grupos, pero no se sabe exactamente dónde.
# Es por esto que se procede a realizar un análisis post-hoc.

alfa = 0.025

# Se escoge la prueba HSD de Tukey debido a que es más poderosa que los factores
# de corrección de Bonferroni y Holm, y además no es tan conservadora como la
# prueba de Scheffé.
post_hoc <- TukeyHSD(anova,
                     "area",
                     ordered = TRUE,
                     conf.level = 1 - alfa)

# Entre leyes y psicología no hay una diferencia significativa de tiempos ya que
# p es mayor al nivel de significancia escogido, sin embargo se tiene que
# entre las áreas de biología y leyes existe una gran diferencia, y también entre
# biología y psicología al tener el valor de p mucho menor que el nivel de
# significancia.
print(post_hoc)

print(summary(biology))
print(summary(law))
print(summary(psychology))
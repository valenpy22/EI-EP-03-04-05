
# ENUNCIADO

# Se sabe que una máquina que envasa detergentes industriales llena bidones con un
# volumen de producto que sigue una distribución normal con desviación estándar de
# 1 litro. Usando una muestra aleatoria de 100 envases, el ingeniero a cargo de la
# planta requiere determinar si la máquina está llenando los bidones con una media
# de 10 litros.

#1.Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra presente 
#una media menor a 9,8 litros o mayor a 10,2 litros, ¿cuál es la probabilidad de 
#que cometa un error de tipo I? Para responder, generen un gráfico de la 
#distribución muestral de las medias hipotetizada en donde se marquen las zonas 
#correspondientes a la probabilidad solicitada, para luego, basándose en este 
#gráfico, calcular el área correspondiente. Tome como ejemplo los scripts 
#presentados en la lectura sobre poder estadístico.

# Hipótesis:
# H0: La máquina llena en promedio 10 litros en cada envase.
# Ha: La máquina llena en promedio una cantidad distinta de 10 litros por envase.

# Ho: mu = mu0, es decir, mu = 10 [L].
# Ha: mu != mu0, es decir, mu != 10 [L].

library(pwr)
library(ggpubr)
library(tidyverse)
library(ggplot2)

# Datos
mu0 <- 10     # hipotesis nula
sigma <- 1    # desviación estandar
n <- 100      # tamaño de la muestra
alfa <- 0.05  # nivel de significancia

# Calcular error estándar
se <- sigma/sqrt(n) # = 0.1

# Graficar la distribución muestral de la media de las muestras
x <- seq(90 * se, 110 * se, 0.01)
y <- dnorm(x, mean= mu0, sd = se)

df <- data.frame(x, y)

g <- ggplot(data = df, aes(x = x, y = y))
g <- g + stat_function(fun = dnorm,
                       args = list(mean = mu0, sd = se),
                       colour = "red",
                       size = 1)

g <- g + ylab("")
g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_x_continuous(name = "Diferencia en tiempos de ejecución [ms]",
                             breaks = seq(9, 11, 0.1))

g <- g + theme_pubr()

# Colorear la región de rechazo de la hipótesis nula
q_critico_inferior <- 9.8
q_critico_superior <- 10.2

g <- g + geom_area(data = subset(df, x < q_critico_inferior),
                   aes(x = x, y = y),
                   colour = "red",
                   fill =  "red",
                   alpha = 0.5)
g <- g + geom_area(data = subset(df, x > q_critico_superior),
                   aes(x = x, y = y),
                   colour = "#ff5733",
                   fill = "#ff5733",
                   alpha = 0.5)

print(g)

# Calcular el área a la izquierda de 9.8 litros
area_izquierda <- integrate(function(x) dnorm(x, mean = mu0, sd = se), 
                             lower = -Inf, upper = 9.8)$value

# Calcular el área a la derecha de 10.2 litros
area_derecha <- integrate(function(x) dnorm(x, mean = mu0, sd = se), 
                           lower = 10.2, upper = Inf)$value

# Calcular el área total sombreada
alfa <- (area_izquierda + area_derecha)*100 # = 4,550026

# La probabilidad de que cometa un error de tipo I es 4,55%


#2.Si el verdadero volumen medio de los bidones fuera de 10,1 litros, ¿cuál sería 
#la probabilidad de que el ingeniero, que obviamente no conoce este dato, cometa 
#un error de tipo II? Para responder, agregue al gráfico anterior la verdadera 
#distribución muestral de las medias y marquen (con otro color) la zona 
#correspondiente a la probabilidad solicitada, para luego, basándose en este 
#gráfico, calcular el área correspondiente. También hay ejemplos de este 
#procedimiento en la lectura sobre poder estadístico.

# Superponer la distribución muestral de la media de las diferencias si la diferencia de medias fuera 10,1
media_efecto <- 10.1
g <- g + stat_function(fun = dnorm,
                       args = list(mean = media_efecto, sd = se),
                       colour = "blue", size = 1)

# Colorear la región de la nueva curva situada en la región de rechazo de la curva original
x1 <- seq(90 * se, 110 * se, 0.01)
y1 <- dnorm(x1, mean = media_efecto, sd = se)  # Usar x1 en lugar de x
g <- g + geom_area(data = subset(data.frame(x1, y1), x1 < q_critico_inferior),  # Usar x1 en lugar de x
                   aes(x = x1, y = y1),
                   colour = "blue",
                   fill = "blue",
                   alpha = 0.5)
g <- g + geom_area(data = subset(data.frame(x1, y1), x1 > q_critico_superior),  # Usar x1 en lugar de x
                   aes(x = x1, y = y1),
                   colour = "blue",
                   fill = "blue",
                   alpha = 0.5)

print(g)

# Calcular el poder de acuerdo al análisis teórico
poder <- pnorm(q_critico_inferior,
               mean = media_efecto,
               sd = se,
               lower.tail = TRUE) +
         pnorm(q_critico_superior,
               mean = media_efecto,
               sd = se,
               lower.tail = FALSE)


# Calcular la probabilidad de cometer un error de tipo II
beta <- (1 - poder)*100 # = 0,8399948

# La probabilidad de cometer un error tipo II es 84%, dada por el área bajo las curvas sin pintar.


#3.Como no se conoce el verdadero volumen medio, genere un gráfico del poder 
#estadístico con las condiciones anteriores, pero suponiendo que el verdadero 
#volumen medio podría variar de 9,6 a 10,4 litros. Hay un ejemplo de este tipo 
#de gráfico en la lectura sobre poder estadístico.

efecto <- seq(0, 20, 0.01)

n100_alpha0455 = power.t.test(n = 100,
                              delta = efecto,
                              sd = sigma,
                              sig.level = alfa/100,
                              type = "one.sample",
                              alternative = "two.sided")$power
                              
# Construir matriz de datos en formato ancho
datos <- data.frame(efecto, n100_alpha0455)

# Llevar a formato largo
datos <- datos %>% pivot_longer(!"efecto", names_to = "fuente", values_to = "poder")

# Formatear fuente como variable categórica
niveles <- c("n100_alpha0455")

etiquetas <- c("n = 100, alfa = 0.0455")

datos[["fuente"]] <- factor(datos[["fuente"]], levels = niveles, labels = etiquetas)

# Graficar la curva de poder
g <- ggplot(datos, aes(efecto, poder, colour = factor(fuente)))
g <- g + geom_line()
g <- g + labs(colour = "")
g <- g + ylab("Poder estadístico")
g <- g + xlab("Tamaño del efecto")
g <- g + scale_color_manual(values = c("red"))
g <- g + theme_pubr()
g <- g + ggtitle("Curvas de poder para prueba t bilateral")
g <- g + geom_vline(xintercept = 9.6, linetype = "dashed")
g <- g + geom_vline(xintercept = 10.4, linetype = "dashed")
g <- g + ylim(0, 1)
g <- g + xlim(0, 11)
print(g)

#4.Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían 
#revisarse para conseguir un poder estadístico de 0,7 y un nivel de significación de 0,05

poder <- 0.7
alpha <- 0.05
diferencia <- 10.1 - mu0 #Diferencia entre la media real y la hipotética

resultado <- power.t.test(n = NULL,
                        delta = diferencia,
                        sd = sigma,
                        sig.level = alpha,
                        power = poder,
                        type = "one.sample",
                        alternative = "two.sided")

n_new <- ceiling(resultado[["n"]])

print(n_new)

# Teniendo que la verdadera media es 10.1 (indicada en la pregunta 2) se tiene lo siguiente:
# Para conseguir un poder estadístico de 0,7 y un nivel de significación de 0,05 se necesitarían 620 bidones

#5.¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de 
#cometer un error de tipo I a un 1% solamente?

poder <- 0.7
alpha_2 <- 0.01
diferencia <- 10.1 - mu0 #Diferencia entre la media real y la hipotética

resultado <- power.t.test(n = NULL,
                        delta = diferencia,
                        sd = sigma,
                        sig.level = alpha_2,
                        power = poder,
                        type = "one.sample",
                        alternative = "two.sided")

n_new <- ceiling(resultado[["n"]])

print(n_new)

# Teniendo que la verdadera media es 10.1 (indicada en la pregunta 2) se tiene lo siguiente:
# Para conseguir un poder estadístico de 0,7 y un nivel de significación de 0,01 se necesitarían 965 bidones.

library(pwr)

#2.Si el verdadero volumen medio de los bidones fuera de 10,1 litros, ¿cuál sería 
#la probabilidad de que el ingeniero, que obviamente no conoce este dato, cometa 
#un error de tipo II? Para responder, agregue al gráfico anterior la verdadera 
#distribución muestral de las medias y marquen (con otro color) la zona 
#correspondiente a la probabilidad solicitada, para luego, basándose en este 
#gráfico, calcular el área correspondiente. También hay ejemplos de este 
#procedimiento en la lectura sobre poder estadístico.

#3.Como no se conoce el verdadero volumen medio, genere un gráfico del poder 
#estadístico con las condiciones anteriores, pero suponiendo que el verdadero 
#volumen medio podría variar de 9,6 a 10,4 litros. Hay un ejemplo de este tipo 
#de gráfico en la lectura sobre poder estadístico.

#4.Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían 
#revisarse para conseguir un poder estadístico de 0,7 y un nivel de significación de 0,05?

#5.¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de 
#cometer un error de tipo I a un 1% solamente?
   

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


#Datos 

#H0: La máquina llena en promedio 10 litros en cada envase.
#Ha: La máquina llena en promedio una cantidad distinta de 10 litros por envase.

#Ho: mu = mu0, es decir, mu = 10 [L].
#Ha: mu != mu0, es decir, mu != 10 [L].

#La probabilidad de cometer un error de tipo I se relaciona al valor de alfa, teniendo
#en este caso un valor de 0.01, debido a que se ve la diferencia entre los valores entregados
#y la media, dando 0.02. Pero como se trata de una prueba bilateral, este valor debe dividirse por 2,
#dando como resultado 0.01.

#z1: (9.8 - 10)/1 = -0.2
#z1: (10.2 - 10)/1 = 0.2

inferior <- 9.8
superior <- 10.2
media <- 10
sd <- 1
n <- 100

z1 <- (inferior - media)/sd
z2 <- (superior - media)/sd

#Probabilidad de error tipo I
#I = P(Z < -0.2) + P(Z > 0.2)

library(ggpubr)
library(pwr)
n <- 100
sigma <- 1
null_mean <- 10


SE <- sigma/sqrt(n)
print(SE)

x<- seq(93*SE, 107*SE, 0.1)
y<- dnorm(x, mean=null_mean, sd = SE)
df <- data.frame(x,y)
g <- ggplot(data = df, aes(x))
g <- g + stat_function(
  fun = dnorm,
  args = list(mean= null_mean, sd = SE),
  colour = "red", size = 1
)
#bug

g <- g + ylab("")
g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_x_continuous(name ="Diferencia en tiempos de ejecuci ó n [ ms ]",
                                  breaks = seq(-6, 12, 0.1) )
#bug
g <- g + theme_pubr()
print(g)

media_nula <- 9.8
Z_critico <- qnorm(alfa /2 , mean = media_nula , sd = SE , lower.tail = FALSE )
q_critico_inferior <- media_nula - Z_critico
q_critico_superior <- media_nula + Z_critico

g <- g + geom_area(data = subset(df, x < q_critico_inferior),
                       aes(y = y) ,
                       colour = "red" ,
                       fill = "red" ,
                       alpha = 0.5)

print(g)
# 
# efecto <- seq(-2.5, 2.5, 0.01)
# alpha <- 0.01
# 
# n_100_alfa_01 <- power.t.test(n = n,
#                             delta = efecto,
#                             sd = dev,
#                             sig.level = alpha,
#                             type = "one.sample",
#                             alternative = "two.sided")$power
# 
# datos <- data.frame(efecto, n_100_alfa_01)
# 
# datos <- datos %>% pivot_longer(!"efecto",
#                                 names_to = "fuente",
#                                 values_to = "poder")
# 
# nivel <- c("n_100_alfa_01")
# 
# datos[["fuente"]] <- factor(datos[["fuente"]], levels = nivel)
# 
# #Graficar
# g <- ggplot(datos, )
# 










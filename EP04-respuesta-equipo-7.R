
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
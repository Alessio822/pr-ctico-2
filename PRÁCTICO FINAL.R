#PRÁCTICO NÚMERO 2 
# UNIVERSIDAD TECNOLOGICA NACIONAL BUENOS AIRES
#ALEJANDRO WALTER OLLIVA
#CURSO ENTORNO R
#

# PREGUNTAS

#1)DATOS
#¿Qué información brinda el dataset ?¿Qué tipos de datos tiene el dataset?¿Están completos?
# ¿Cuántas filas y columnas contiene?

#2) Edad  (columna age)
#¿Cómo se disribuye la columnna edad en  la muestra?
#¿Cuál es el máximo y mínimo?¿Cuál es rango de edad que presenta
#mayor frecuencia absoluta?

#3)Género y habito de fumar (columna gender y smoking )
#¿Cómo se relacione el género con el hábito de fumar en la muestra?
#¿Hay mas fumadores hombres que mujeres? 
#¿Es significativa la diferencia?
 
#4) Fumadores  (columna smoking,triglyceride y Cholesterol )
#¿Cómo se relaciona los trigliceridos con el colesterol
#dento del grupo de fumadores?
#¿Cómo se relaciona la estatura con el peso
#dento del grupo de fumadores?

#5) Fumadores y no fumadores y la presión sistólilca.
# ¿Que diferencias podemos encontrar entre la presión
#sistólica entre fumadores y no fumadores.


# Cargo el dateset propuesto por el profesor en el drive
# llamado 7_smoking_prediction.csv
# cargo el dataset y lo llamo smoking
smoking <- "C:\\Users\\Alejandro Oliva\\Desktop\\utn curso R\\MODULO 2\\7_smoking_prediction.csv"
smoking<- read.table(smoking,header=T, sep=",",stringsAsFactors = T)

#1)DATOS
#¿Qué información brinda el dataset ?¿Qué tipos de datos tiene el dataset?¿Están completos?
# ¿Cuántas filas y columnas contiene?


str(smoking)
nrow( smoking  )

#Utilizo la  función str() para una vista general de la estructura del dataset,
#incluidos los tipos de datos de cada columna.
#Obtengo 55692 observaciones que corresponden a 55691 personas
#que fueron estudidadas meidante 27 variables relacionandaa
# con el hábito de fumar.
#Se encuentra variables de tipo enteros, factores y númericas.
#

sum(is.na(smoking))
# [1] 0
#El resultado 0 indica que no hay valores faltantes en el dataset.
summary(smoking)

#2) Edad  (columna age)
#¿Cómo se disribuye la columnna edad en  la muestra?
#¿Cuál es el máximo y mínimo?¿Cuál es rango de edad que presenta
#mayor frecuencia absoluta?

# voy a trabajar con la columna de edades en el dataset
#
age_data <- smoking %>% select(age)

# verifico la edad minima y máxima para obterner el rango.
min_age <- min(age_data$age, na.rm = TRUE)  # Mínimo valor
max_age <- max(age_data$age, na.rm = TRUE)  # Máximo valor

min_age
max_age

#La edad mininma es 20 y maxima 85 con un rango de 65.
# Grafico el histograma
hist(age_data$age, 
     breaks = seq(20,85, by=5)   , 
     col = "lightblue",  
     border = "black",  
     main = "Histograma de Edades ", 
     xlab = "Edad", 
     ylab = "Frecuencia")

aprox_densidad<-density(age_data$age,bw=3 )


plot(aprox_densidad,main="Edades ",col="blue",lwd = 2  )


#3) ¿Cómo se relacione el genero con el hábito de fumar en la muestra?

# Para ello voy a crear la tabla de contingencia
tabla_contingencia <- table(smoking$gender, smoking$smoking)

# Mostrar la tabla
print(tabla_contingencia)

# Asignano nombres a las categorías para mejor visualización
rownames(tabla_contingencia  ) <- c("Femenino", "Masculino")
colnames( tabla_contingencia) <- c("No Fumador", "Fumador")

# Muestro la tabla personalizada.
print(  tabla_contingencia )

# Voy a aplicar test de diferencia de proporciones 
#parar saber si hay diferencia entre la proporción de fumadores
#entre hombres y mujeres con el estadístico de prueba
#chi cuadrado
#chi cuadrado
# Ho (Hipótesis Nula) No hay diferencia de proporciones
# Ha (Hipótesis Nula) Hay diferencia de proporciones
# Como p es muchísimo menor a 0.05, rechazamos la hipótesis nula 

# Datos
fumadores <- c(859, 19596)# Fumadores en mujeres y hombres
total <- c(20291, 35401)   # Total de personas en cada grupo

# Test de diferencia de proporciones
test <- prop.test(fumadores, total, correct=FALSE)

# Resultados
print(test)

# La proporción de fumadores es significativamente mayor en hombres que en mujeres.
# La diferencia entre las proporciones es grande (alrededor de un 51% más de fumadores en hombres).
# La hipótesis nula se rechaza con alta certeza.

#Esta conclusión la podemos afirmarr tambien con una representación
#gráfica para el casso  de una tabla de contingencia eess preferrible
#
mosaicplot(tabla_contingencia , main = "Relación entre Género y Hábito de Fumar", col = c("lightblue", "pink"))
# Vemos en el mosaico el rectángulo  rosado correspondiente
# a fumadores es significativamente mayor en los hombres.

# Ahora voy a filtrar la columna smoking ( mismo nombre datasset)
#  para obttener solo los fumadores.

library(dplyr)
df_filtrado <-smoking %>%
  filter(smoking == 1)

print( df_filtrado    )

correlacion <- cor(df_filtrado$Cholesterol   , df_filtrado$triglyceride , use = "complete.obs")
print(correlacion)

correlacion1 <- cor(df_filtrado$Cholesterol   , df_filtrado$fasting.blood.sugar    , use = "complete.obs")
print(correlacion1)

correlacion2 <- cor(df_filtrado$height.cm.   , df_filtrado$weight.kg.   , use = "complete.obs")
print(correlacion2)
# resumen, puedes concluir que hay una 
# relación moderada entre el peso y la estatura en el grupo de datos analizado

library(ggplot2)

# Crear el gráfico de dispersión
ggplot(df_filtrado, aes(x = height.cm., y = weight.kg.)) +
  geom_point() +  # Añadir puntos
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  # Línea de regresión
  labs(
    title = "Relación entre Estatura y Peso Fumadores",
    x = "Estatura (cm)",
    y = "Peso (kg)"
  ) +
  theme_minimal()

# Cargar librería
library(dplyr)

# Voy a  realizar un resumen esttadistico de Fumadoress
resumen_fumadores <- smoking %>%
  filter(smoking == 1) %>%  
  summary()  

# Mostrar el resumen
print(resumen_fumadores)

#Voy a realilzar un reesumen estadistico de no fumadores
resumen_nofumadores <- smoking %>%
  filter(smoking == 0) %>%  
  summary()  

# Mostrar el resumen
print(resumen_nofumadores)

plot(smoking$age, smoking$systolic ,
     main = "Relación Edad y Presión Sistólica",
   xlab = "Edad de  Fumadores y no Fumadores(años) ",
   ylab="Presión sistólica fumadoresy no fumadores(mm/Hg)",
     pch= 2,
   col="orange")



# Mostrar resultados
print(resumen_systolic)
# Instalar y cargar ggplot2 si no lo tienes ya
# install.packages("ggplot2")
library(ggplot2)



# este esta muy bien 

# Agrupar por fumadores y no fumadores y calcular los valores necesarios
resumen_systolic <- smoking %>%
  group_by(smoking) %>%
  summarise(
    Q1 = quantile(systolic, 0.25, na.rm = TRUE),
    mediana = median(systolic, na.rm = TRUE),
    promedio=mean( systolic, na.rm = TRUE     ),
    Q3 = quantile(systolic, 0.75, na.rm = TRUE),
    min = min(systolic, na.rm = TRUE),
    max = max(systolic, na.rm = TRUE)
  )

print(resumen_systolic)


# Crear el boxplot

ggplot(smoking, aes(x = factor(smoking), y = systolic)) +
     geom_boxplot(color="red")+ 
  labs(x = "Fumador (0 = No, 1 = Sí)", y = "Presión Sistólica") +
     theme( panel.background = element_rect(fill = "lightblue" ))



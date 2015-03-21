############################ ANALISIS ESTADISTICO - MODULO 10 DATA SCIENCE - UTAD ###############################

# Para esta práctica, he elegido el dataset "diamonds" que contendrá el precio (entre otras variables
# interesantes) de unos 54.000 diamantes.

# Parece un dataset tipo bastante interesante con el que poder realizar distintos estudios estadísticos.

# El objetivo será por lo tanto, realizar distintos tipos de análisis estadísticos de sus variables para intentar
# averiguar algún tipo de comportamiento oculto aparentemente en los datos.

# Los diferentes indicadores presentes en el dataset "diamonds" son los siguientes:

# price: Precio en dolares americanos ($326–$18,823)
# carat: peso del diamante (0.2–5.01)
# cut: calidad del corte (Fair, Good, Very Good, Premium, Ideal)
# colour: color del diamante (desde D el mejor hasta J el peor)
# clarity: mide como de claro es el diamante (desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)
# x: longitud en mm (0–10.74)
# y: ancho en  mm (0–58.9)
# z: profundidad en mm (0–31.8)
# depth: porcentaje total de profundidad (total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43–79))
# table: anchura de la parte superior de diamante con relación al punto más ancho (43-95)

################################# TRATAMIENTO DE LOS DATOS INICIALES ############################################

# 0- Instalacion de librerias.

library(ggplot2) # necesaria para cargar el dataset diamonds
library(stats) # stadistica
library(prettyR)
library(manipulate) # para dibujar graficos
library(rgl) #visualizaciones 3d
library(e1071)
library(modeest) # contiene estimadores en el modo unimodal y alguno multimodal
library(quantmod)
library(dplyr)
library(nortest)

# 1- Obtenemos el dataset diamonds.
data(diamonds)

# 2- Analizamos de forma rapida los datos presentes en el dataset.
summary(diamonds) # obtenemos un resumen de los datos

# 3 - Modificacion de datos
# Debido a que hay 3 variables muy importantes y que no son numericas, deberemos de realizar una reclasificacion para incluirlas
# El objetivo sera cambiar las variables cut, colour y clarity para pasarlas a tipo "double".

# Creamos un nuevo data frame que modificaremos 
df.diamonds <- diamonds


# cambiamos los valores de la variable cut, y las renombramos del 1 al 5, de mejor(Ideal) a peor(Fair):

df.diamonds$cut2 <- as.numeric(df.diamonds$cut)

df.diamonds$cut2[df.diamonds$cut == "Ideal"] <- 1
df.diamonds$cut2[df.diamonds$cut == "Premium"] <- 2
df.diamonds$cut2[df.diamonds$cut == "Very Good"] <- 3
df.diamonds$cut2[df.diamonds$cut == "Good"] <- 4
df.diamonds$cut2[df.diamonds$cut == "Fair"] <- 5

# hacemos lo mismo para la variable color:

df.diamonds$color2 <- as.numeric(df.diamonds$color)

df.diamonds$color2[df.diamonds$color == "D"] <- 1
df.diamonds$color2[df.diamonds$color == "E"] <- 2
df.diamonds$color2[df.diamonds$color == "F"] <- 3
df.diamonds$color2[df.diamonds$color == "G"] <- 4
df.diamonds$color2[df.diamonds$color == "H"] <- 5
df.diamonds$color2[df.diamonds$color == "I"] <- 6
df.diamonds$color2[df.diamonds$color == "J"] <- 7


# hacemos lo mismo para la variable clarity:

df.diamonds$clarity2 <- as.numeric(df.diamonds$clarity)

df.diamonds$clarity2[df.diamonds$clarity == "IF"] <- 1
df.diamonds$clarity2[df.diamonds$clarity == "VVS1"] <- 2
df.diamonds$clarity2[df.diamonds$clarity == "VVS2"] <- 3
df.diamonds$clarity2[df.diamonds$clarity == "VS1"] <- 4
df.diamonds$clarity2[df.diamonds$clarity == "VS2"] <- 5
df.diamonds$clarity2[df.diamonds$clarity == "SI1"] <- 6
df.diamonds$clarity2[df.diamonds$clarity == "SI2"] <- 7
df.diamonds$clarity2[df.diamonds$clarity == "I1"] <- 8


# Una vez tenemos todas las variables como numericas, creo un data frame con las columnas que me interesan:
df.estudio <- df.diamonds[, c(1,5:13)]
class(df.estudio) # es un dataframe

# Podremos realizar otro "summary" para ver cómo quedarian los datos con estas modificaciones:
summary(df.estudio)


##################################### INFORMACION DATAFRAME ####################################################

# Obtenemos sus dimensiones:
dimension<-dim(df.estudio) # Es un dataframe de 53.940 x 10
nobs<-dimension[1] 
nvar<-dimension[2]
nobs # Tenemos 53.940 observaciones
nvar # Hay 10 variables


# Realizamos un attach al dataframe, de forma que podamos acceder a sus valores directamente con el nombre
# de la variable.

attach(df.estudio) # para activar
# detach(df.estudio) # para desactivar


################################### ANALISIS DE VARIABLES #####################################################

# Realizamos un análisis de las variables del dataset Diamonds. He elegido para el analisis las variables
# carat, table, price y cut2. (para el resto de variables el proceso seria igual)


######### Variable "carat" (peso del diamante)- VARIABLE CUANTITATIVA CONTINUA

nobs <- length(carat) # tiene 53.940 observaciones

# Posibles valores vacios
sum(is.na(carat))  # comprobamos que no presenta valores vacios

# Análisis de frecuencias
## Frecuencia absoluta:
fabscar<-table(carat)
fabscar

## Frecuencia relativa:
frelcar<-fabscar/sum(fabscar)
frelcar

## Frecuencia absoluta acumulada:
fabsacumcar<-as.table(cumsum(fabscar))
fabsacumcar

## Frecuencia relativa acumulada:
frelacumcar<-as.table(cumsum(frelcar))
frelacumcar # Comprobamos que la suma de las frecuencia relativas acumuladas es 1.

# Resumen 
summary(carat) #media: 0.7979, mediana: 0.7
mfv(carat) # moda 0.3
quantile(carat) # visualizacio de los cuantiles. Q1 = 0.40, Q3 = 1.04 (la diferencia entre esos cuantiles
                # es el rango intercuartilico)
# El rango intercuartilico es de 0.64. Calculamos los dos bigotes.
# Bigote superior: 1.04 + 1.5*0.64 = 2
# Bigote inferior: 0.40 - 1.5*0.64 = 0.08

var(carat)   # varianza de 0.2246867
sd(carat)    # desviacion tipica 0.4740112

# Representacion
pie(frelcar)
hist(carat)
boxplot(carat) # existen varios valores extremos


# funcion densidad
densCarat<-density(carat)
plot(densCarat, lwd=3,col="blue") # Se observa que la funcion de densidad no presenta una distribucion normal

# asimetria
skew(carat) # 1.116, proxima a uno, practicamente asimetrica

# Procedemos a buscar los valores extremos y elminarlos del análisis
# eliminacion de valores extremos
max(carat)  # el valor maximo es de 5.01
carat2<-subset(carat, carat < 2)
nobscarat2<-length(carat2) # Tiene 51.786 observaciones (53.940 eran las obs. iniciales)


# Nuevo análisis de frecuencias sin valores extremos para la variable "carat".

## Frecuencia absoluta:
fabscar2<-table(carat2)
fabscar2

## Frecuencia relativa:
frelcar2<-fabscar2/sum(fabscar2)
frelcar2

## Frecuencia absoluta acumulada:
fabsacumcar2<-as.table(cumsum(fabscar2))
fabsacumcar2

## Frecuencia relativa acumulada:
frelacumcar2<-as.table(cumsum(frelcar2))
frelacumcar2 # Comprobamos que la suma de las frecuencia relativas acumuladas es 1.

# Resumen de dataframe sin valores extremos
summary(carat2) #media: 0.7423, mediana: 0.7 , mantiene unos valores similares a antes
mfv(carat) # moda 0.3, misma moda que antes

# cuartiles
quantile(carat2)

# representaci?n
boxplot(carat2) # se comprueba que no hay valores extremos como antes
var(carat2)   # 0.1544
sd(carat2)   # 0.39300
hist(carat2) 

#funcion densidad
densCarat2<-density(carat2)
plot(densCarat2, lwd=3,col="blue") # Sigue sin mantener una distribucion normal

# asimetria
skew(carat2) #proxima a uno, practicamente asimetrica

# contraste de Kolmogorov-Smirnoff Lilliefors. Comprobaremos la hipotesis de la normal:
sort(carat)
lillie.test(carat) # p-value 2.2e-16,  se rechaza la Hipotesis de normalidad
lillie.test(carat2) # p-value 2.2e-16, se rechaza la Hipotesis de normalidad
# En ambos casos, tanto usando todos los valores de la variable, como descartando los valores extremos,
# no pasa la hipotesis de la normal.

# Se puede hacer otro test más eficaz para verificar si es una distribucion normal, mediante Shapiro-Wilk:
# Para ello, primero realizamos una muestra de los datos:

muestra.estudio <- df.estudio[1:4000, ]
shapiro.test(muestra.estudio$carat) # obtenemos 2.2e-16, por lo que no pasa el test de la normal.


-----------------------------------------------------------------
######### Variable "table" (anchura de la parte superior)- VARIABLE CUANTITATIVA DISCRETA

length(table) # tiene 53.940 observaciones

# Posibles valores vacios
sum(is.na(table))  # comprobamos que no presenta valores vacios

# Análisis de frecuencias
## Frecuencia absoluta:
fabstab<-table(table)
fabstab
## Frecuencia relativa:
freltab<-fabstab/sum(fabstab)
freltab
## Frecuencia absoluta acumulada:
fabsacumtab<-as.table(cumsum(fabstab))
fabsacumtab
## Frecuencia relativa acumulada:
frelacumtab<-as.table(cumsum(freltab))
frelacumtab # Comprobamos que la suma de las frecuencia relativas acumuladas es 1.

# Resumen 
summary(table) #media: 57.46, mediana: 57
mfv(table) # moda 56
quantile(table) # visualizacio de los cuantiles. Q1 = 56, Q3 = 59 (la diferencia entre esos cuantiles
# es el rango intercuartilico)
# El rango intercuartilico es de 3. Calculamos los dos bigotes.
# Bigote superior: 59+ 1.5*3 = 63.5
# Bigote inferior: 56 - 1.5*3 = 51.5

var(table)   # varianza de 4.9929
sd(table)    # desviacion tipica 2.234

# Representacion
pie(freltab)
hist(table)
boxplot(table) # existen varios valores extremos, tanto por arriba como por abajo


# funcion densidad
densTable<-density(table)
plot(densTable, lwd=3,col="blue") # Se observa que la funcion de densidad no presenta una distribucion normal
                                  # presenta muchas crestas
# asimetria
skew(table) # 0.7968, proxima a uno, practicamente asimetrica

# Procedemos a buscar los valores extremos y elminarlos del análisis
# eliminacion de valores extremos
max(table)# el valor maximo es de 95
min(table)# el valor minimo es 43
table2<-subset(table, table<63.5 & table>51.5)

length(table2) # Tiene 53.334 observaciones (53.940 eran las obs. iniciales), por lo que a sido bueno quitar
# esos valores extremos.


# Nuevo análisis de frecuencias sin valores extremos para la variable "table".

## Frecuencia absoluta:
fabstab2<-table(table2)
fabstab2
## Frecuencia relativa:
freltab2<-fabstab2/sum(fabstab2)
freltab2
## Frecuencia absoluta acumulada:
fabsacumtab2<-as.table(cumsum(fabstab2))
fabsacumtab2
## Frecuencia relativa acumulada:
frelacumtab2<-as.table(cumsum(freltab2))
frelacumtab2 # Comprobamos que la suma de las frecuencia relativas acumuladas es 1.

# Resumen de dataframe sin valores extremos
summary(table2) #media: 57, mediana: 57.37 , mantiene unos valores similares a antes
mfv(table2) # moda 56, misma moda que antes

# cuartiles
quantile(table2)

# representaci?n
boxplot(table2) # se comprueba que no hay valores extremos como antes
var(table2)   # 4.3023
sd(table2)   # 2.0741
hist(table2) 

#funcion densidad
densTable2<-density(table2)
plot(densTable2, lwd=3,col="blue") # Sigue sin mantener una distribucion normal, aunque parece mas simetrica.

# asimetria
skew(table2) # 0.3668, proxima a cero, practicamente simetrica. Al eliminar los valores extremos
# hemos conseguido dejar más simetrica a la variable.

# contraste de Kolmogorov-Smirnoff Lilliefors. Comprobaremos la hipotesis de la normal:
sort(table)
lillie.test(table) # p-value 2.2e-16,  se rechaza la Hipotesis de normalidad
lillie.test(table2) # p-value 2.2e-16, se rechaza la Hipotesis de normalidad
# En ambos casos, tanto usando todos los valores de la variable, como descartando los valores extremos,
# no pasa la hipotesis de la normal.

# Se puede hacer otro test más eficaz para verificar si es una distribucion normal, mediante Shapiro-Wilk:
# Para ello, primero realizamos una muestra de los datos:

muestra.estudio <- df.estudio[1:4000, ]
shapiro.test(muestra.estudio$table) # obtenemos 2.2e-16, por lo que no pasa el test de la normal.


-----------------------------------------------------------------
######### Variable "price" (precio del diamante)- VARIABLE CUANTITATIVA DISCRETA
  
length(price) # tiene 53.940 observaciones

# Posibles valores vacios
sum(is.na(price))  # comprobamos que no presenta valores vacios

# Análisis de frecuencias
## Frecuencia absoluta:
fabspri<-table(price)
fabspri
## Frecuencia relativa:
frelpri<-fabspri/sum(fabspri)
frelpri
## Frecuencia absoluta acumulada:
fabsacumpri<-as.table(cumsum(fabspri))
fabsacumpri
## Frecuencia relativa acumulada:
frelacumpri<-as.table(cumsum(frelpri))
frelacumpri # Comprobamos que la suma de las frecuencia relativas acumuladas es proxima a 1.

# Resumen 
summary(price) # media: 3933, mediana: 2401
mfv(price) # moda 605
quantile(price) # visualizacio de los cuantiles. Q1 = 950, Q3 = 5324.25 (la diferencia entre esos cuantiles
# es el rango intercuartilico)
# El rango intercuartilico es de 4374.25. Calculamos los dos bigotes.
# Bigote superior: 5324.25 + 1.5*4374.25 = 11885.625
# Bigote inferior: 950 - 1.5*4374.25 = -5611.37

var(price)   # varianza de 15915629
sd(price)    # desviacion tipica 3989.44

# Representacion
hist(price)
boxplot(price) # existen varios valores extremos bien diferenciados.


# funcion densidad
densPrice<-density(price)
plot(densPrice, lwd=3,col="blue") # Se observa que la funcion de densidad no presenta una distribucion normal

# asimetria
skew(price) # 1.618, presenta asimetria positiva.

# Procedemos a buscar los valores extremos y elminarlos del análisis
# eliminacion de valores extremos
max(price)# el valor maximo es de 18823
price2<-subset(price, price<11885.625)

length(price2) # Tiene 50.400 observaciones (53.940 eran las obs. iniciales), por lo que a sido bueno quitar
# esos valores extremos.


# Nuevo análisis de frecuencias sin valores extremos para la variable "price".

## Frecuencia absoluta:
fabspri2<-table(price2)
fabspri2
## Frecuencia relativa:
frelpri2<-fabspri2/sum(fabspri2)
frelpri2
## Frecuencia absoluta acumulada:
fabsacumpri2<-as.table(cumsum(fabspri2))
fabsacumpri2
## Frecuencia relativa acumulada:
frelacumpri2<-as.table(cumsum(frelpri2))
frelacumpri2 # Comprobamos que la suma de las frecuencia relativas acumuladas es 1.

# Resumen de dataframe sin valores extremos
summary(price2) #media: 3159, mediana: 2155 , mantiene unos valores similares a antes
mfv(price2) # moda 605, igual a la anterior.

# cuartiles
quantile(price2)

# representaci?n
boxplot(price2) # se comprueba que no hay valores extremos como antes
var(price2)   # 7643568
sd(price2)   # 2764.7
hist(price2) 

#funcion densidad
densPrice2<-density(price2)
plot(densPrice2, lwd=3,col="blue") # Sigue sin mantener una distribucion normal.

# asimetria
skew(price2) # 1.1893, presenta asimetria positiva.

# contraste de Kolmogorov-Smirnoff Lilliefors. Comprobaremos la hipotesis de la normal:
sort(price)
lillie.test(price) # p-value 2.2e-16,  se rechaza la Hipotesis de normalidad
lillie.test(price2) # p-value 2.2e-16, se rechaza la Hipotesis de normalidad
# En ambos casos, tanto usando todos los valores de la variable, como descartando los valores extremos,
# no pasa la hipotesis de la normal.

# Se puede hacer otro test más eficaz para verificar si es una distribucion normal, mediante Shapiro-Wilk:
# Para ello, primero realizamos una muestra de los datos:

muestra.estudio <- df.estudio[1:4000, ]
shapiro.test(muestra.estudio$price) # obtenemos 2.2e-16, por lo que no pasa el test de la normal.


-----------------------------------------------------------------
######### Variable "cut" (calidad)- VARIABLE CUALITATIVA ORDINAL
#### se utiliza la variable cut2 creada al principio.
  
  
length(cut2) # tiene 53.940 observaciones

# Posibles valores vacios
sum(is.na(cut2))  # comprobamos que no presenta valores vacios

# Análisis de frecuencias
## Frecuencia absoluta:
fabscut<-table(cut2)
fabscut
## Frecuencia relativa:
frelcut<-fabscut/sum(fabscut)
frelcut
## Frecuencia absoluta acumulada:
fabsacumcut<-as.table(cumsum(fabscut))
fabsacumcut
## Frecuencia relativa acumulada:
frelacumcut<-as.table(cumsum(frelcut))
frelacumcut # Comprobamos que la suma de las frecuencia relativas acumuladas es  1.

# Resumen 
summary(cut2) # media: 2.096, mediana: 2
mfv(cut2) # moda 1
quantile(cut2) # visualizacio de los cuantiles. Q1 = 1, Q3 = 3 (la diferencia entre esos cuantiles
# es el rango intercuartilico)
# El rango intercuartilico es de 2. Calculamos los dos bigotes.
# Bigote superior: 3 + 1.5*2 = 6
# Bigote inferior: 1 - 1.5*2 = -3

var(cut2)   # varianza de 1.2467
sd(cut2)    # desviacion tipica 1.1166

# Representacion
hist(cut2)
boxplot(cut2) # no presenta valores extremos.

# funcion densidad
densCut<-density(cut2)
plot(densCut, lwd=3,col="blue") # Se observa que la funcion de densidad no presenta una distribucion normal, y presenta muchas crestas.

# asimetria
skew(cut2) # 0.717, presenta asimetria.

# contraste de Kolmogorov-Smirnoff Lilliefors. Comprobaremos la hipotesis de la normal:
sort(cut2)
lillie.test(cut2) # p-value 2.2e-16,  se rechaza la Hipotesis de normalidad
# En ambos casos, tanto usando todos los valores de la variable, como descartando los valores extremos,
# no pasa la hipotesis de la normal.

# Se puede hacer otro test más eficaz para verificar si es una distribucion normal, mediante Shapiro-Wilk:
# Para ello, primero realizamos una muestra de los datos:

muestra.estudio <- df.estudio[1:4000, ]
shapiro.test(muestra.estudio$cut2) # obtenemos 2.2e-16, por lo que no pasa el test de la normal.

-------------------------------------------------------------------------------------------------
  
### APLICACION DE LA INFERENCIA AL CONJUNTO DE DATOS.
  
# Vamos a realizar la inferencia entre dos muestras de datos independientes de nuestro dataset.
# Para ello, primero comprobamos qué variables presentan mayor independencia.
# Realizamos un test de correlacion entre todas las variables:

# El estudio de la inferencia lo realizamos sobre una muestra del dataset, de forma que
# intentaremos conocer ciertas caracteristicas de la poblacion a partir de esta muestra:

muestra.estudio <- df.estudio[500:1000, ]  
cor(muestra.estudio)
  
# Entre todos los valores obtenidos, eligo para este ejemplo, las variables "price" y "cut2" ya que
# presentan una alta independencia lineal entre ellas.

# Recordamos que: los valores de la variable cut, los renombramos del 1 al 5, de mejor(Ideal) a peor(Fair)
# Vamos a elegir los dos  valores extremos el 1 y el 5 para realizar el estudio.

CutIdeal = muestra.estudio$cut2 == 1
CutFair = muestra.estudio$cut2 == 5
 
Price.CutIdeal = muestra.estudio[CutIdeal,]$price
Price.CutFair = muestra.estudio[CutFair,]$price

# Obtenemos el test de t.student:
t.test(Price.CutIdeal, Price.CutFair)

# obtenemos un p-value = 3.415e -05, es decir < 0.05 por lo que se acepta la hipotesis alternativa.

# El resultado obtenido lo podemos interpretar como:

# En la muestra , precio medio del diamante con un corte "ideal" es de 2654.86 $ y el precio medio del diamante
# con un corte "fair" es de 2862.306 $.
# El intervalo de confianza del 95 % de la diferencia en el precio medio del diamante está entre 111.13 y 303.74.

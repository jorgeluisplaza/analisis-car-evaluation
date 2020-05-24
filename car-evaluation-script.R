# Instala paquetes si se requieren
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(gplots)) install.packages("gplots", repos = "http://cran.us.r-project.org")

# Carga las librerias 
library(tidyverse)
library(gplots)

# Guardamos la URL de la base de datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"

# Leemos directamente sobre los datos y se guarda en un data frame
# El separador de columas es una coma ","
cars <- read.table(url, sep = ",")

# Colocamos los nombres a las columnas
colnames(cars) <- c("buyingPrice", 
                    "maintenanceCost", 
                    "numberOfDoors", 
                    "numberOfPersons",
                    "sizeOfLuggageBoot",
                    "safety",
                    "decision")

# Mostramos los primeros datos
head(cars)

# Calculo de valores nulos 
sum(is.na(cars))

# Clase de cada variable
class(cars$buyingPrice)
class(cars$maintenanceCost)
class(cars$numberOfDoors)
class(cars$numberOfPersons)
class(cars$sizeOfLuggageBoot)
class(cars$safety)
class(cars$decision)

# Alternativa a las instrucciones de arriba
# Es un pequeño resumen de cada variable
summary(cars)

# Grafico distribucion de precios segmentados
# Por decision
cars %>% 
  ggplot(aes(buyingPrice, fill = decision)) +
  geom_bar() +
  xlab("Precio del auto") +
  ylab("Numero total de precios") +
  ggtitle("Distribución de precios segmentados por decisión")

# Grafico de distribucion de precios de mantencion
# Segmentados por decision
cars %>% 
  ggplot(aes(maintenanceCost, fill = decision)) +
  geom_bar() +
  xlab("Precio de mantención") +
  ylab("Número total de precios") +
  ggtitle("Distribución de precios de mantención segmentados por decisión")

# Graficos distribucion de numero de puertas
# segmentados por decision
cars %>% 
  ggplot(aes(numberOfDoors, fill = decision)) +
  geom_bar() +
  xlab("Número de puertas") +
  ylab("Distribución") +
  ggtitle("Distribución de N° de puertas segmentados por decisión")

# Graficos de distribucion de numero de personas
# Segmentados por decision
cars %>% 
  ggplot(aes(numberOfPersons, fill = decision)) +
  geom_bar() +
  xlab("Número de personas") +
  ylab("Distribución") +
  ggtitle("Distribución de N° de personas segmentados por decisión")

# Graficos de distribucion de tamaño del maletero
# Segmentado por decision
cars %>% 
  ggplot(aes(sizeOfLuggageBoot, fill = decision)) +
  geom_bar() +
  xlab("Tamaño del maletero") +
  ylab("Distribución") +
  ggtitle("Distribución del tamaño del maletero segmentados por decisión")

# Grafico de distribucion de la seguridad estimada del auto
# Segmentado por decision
cars %>% 
  ggplot(aes(safety, fill = decision)) +
  geom_bar() +
  xlab("Seguridad estimada del auto") +
  ylab("Distribución") +
  ggtitle("Distribución de la seguridad del auto segmentado por decisión")

# Grafico de distribucion de la decision 
cars %>%
  ggplot(aes(decision)) +
  geom_bar() +
  xlab("Decisión") +
  ylab("Distribución") +
  ggtitle("Distribución de la decisión")

# Tablas contingencia de ejemplo en el informe
table(cars$buyingPrice, cars$maintenanceCost)
table(cars$numberOfPersons, cars$sizeOfLuggageBoot)
table(cars$buyingPrice, cars$numberOfPersons)

# Otras tablas de ejemplo
# Todas equilibradas
table(cars$buyingPrice, cars$numberOfDoors)
table(cars$buyingPrice, cars$safety)
table(cars$sizeOfLuggageBoot, cars$numberOfDoors)
table(cars$maintenanceCost, cars$safety)

# Tablas de contingencia por cada variable y la decision
tabla.precios.decision <- table(cars$buyingPrice, cars$decision)
tabla.maletero.decision <- table(cars$sizeOfLuggageBoot, cars$decision)
tabla.mantencion.decision <- table(cars$maintenanceCost, cars$decision)
tabla.seguridad.decision <- table(cars$safety, cars$decision)
tabla.puertas.decision <- table(cars$numberOfDoors, cars$decision)
tabla.personas.decision <- table(cars$numberOfPersons, cars$decision)

# Mostrar los datos como tablas graficamente
# Tabla precio - decision
precios.decision <- as.table(as.matrix(tabla.precios.decision))
balloonplot(t(precios.decision), main = "precios/decision", xlab = "", ylab = "",
            label = FALSE, show.margins = FALSE)

# Tabla maletero - decision
maletero.decision <- as.table(as.matrix(tabla.maletero.decision))
balloonplot(t(maletero.decision), main = "maletero/decision", xlab = "", ylab = "",
            label = FALSE, show.margins = FALSE)

# Tabla costo mantencion - decision
mantencion.decision <- as.table(as.matrix(tabla.mantencion.decision))
balloonplot(t(mantencion.decision), main = "mantencion/decision", xlab = "", ylab = "",
            label = FALSE, show.margins = FALSE)

# Tabla seguridad - decision
seguridad.decision <- as.table(as.matrix(tabla.seguridad.decision))
balloonplot(t(seguridad.decision), main = "seguridad/decision", xlab = "", ylab = "",
            label = FALSE, show.margins = FALSE)

# Tabla numero puertas - decision
puertas.decision <- as.table(as.matrix(tabla.puertas.decision))
balloonplot(t(puertas.decision), main = "puertas/decision", xlab = "", ylab = "",
            label = FALSE, show.margins = FALSE)

# Tabla numero de personas - decision
personas.decision <- as.table(as.matrix(tabla.personas.decision))
balloonplot(t(personas.decision), main = "personas/decision", xlab = "", ylab = "",
            label = FALSE, show.margins = FALSE)

#Se procede a realizar el test de independencia chi cuadrado a cada tabla de contingencia previamente realizada.
#Planteamiento de hipotesis:

#H0: Las variables de precio y decision son independientes.
#Ha: Las variables de precio y decision no son independientes
chi1 <- chisq.test(tabla.precios.decision)
#Conclusion: Como el p valor es menor a 0.05 se rechaza la hipotesis nula en favor de la alternativa, 
#esxistiendo una asociacion entre el precio y la decision.

#Planteamiento de hipotesis:
#H0: Las variables de maletero y decision son independientes.
#Ha: Las variables de maletero y decision no son independientes
chi2 <- chisq.test(tabla.maletero.decision)
#Conclusion: Como el p valor es menor a 0.05 se rechaza la hipotesis nula en favor de la alternativa, 
#esxistiendo una asociacion entre el maletero y la decision.

#Planteamiento de hipotesis:
#H0: Las variables de mantencion y decision son independientes.
#Ha: Las variables de mantencion y decision no son independientes
chi3 <- chisq.test(tabla.mantencion.decision)
#Conclusion: Como el p valor es menor a 0.05 se rechaza la hipotesis nula en favor de la alternativa, 
#esxistiendo una asociacion entre la mantencion y la decision.

#Planteamiento de hipotesis:
#H0: Las variables de seguridad y decision son independientes.
#Ha: Las variables de seguridad y decision no son independientes
chi4 <- chisq.test(tabla.seguridad.decision)
#Conclusion: Como el p valor es menor a 0.05 se rechaza la hipotesis nula en favor de la alternativa, 
#esxistiendo una asociacion entre la seguridad y la decision.

#Planteamiento de hipotesis:
#H0: Las variables de puertas y decision son independientes.
#Ha: Las variables de puertas y decision no son independientes
chi5 <- chisq.test(tabla.puertas.decision)
#Conclusion: Como el p valor es mayor a 0.05 se falla en rechazar la hipotesis nula en favor de la alternativa, 
#NO esxistiendo una asociacion entre las puertas y la decision.

#Planteamiento de hipotesis:
#H0: Las variables de personas y decision son independientes.
#Ha: Las variables de personas y decision no son independientes
chi6 <- chisq.test(tabla.personas.decision)
#Conclusion: Como el p valor es menor a 0.05 se rechaza la hipotesis nula en favor de la alternativa, 
#esxistiendo una asociacion entre la cantidad de personas y la decision.

chi1
chi2
chi3
chi4
chi5
chi6


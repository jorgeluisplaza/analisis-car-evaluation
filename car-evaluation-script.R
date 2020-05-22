library(tidyverse)

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

# Tablas de contingencia por cada variable y la decision
tabla.precios.decision <- table(cars$buyingPrice, cars$decision)
tabla.maletero.decision <- table(cars$sizeOfLuggageBoot, cars$decision)
tabla.mantencion.decision <- table(cars$maintenanceCost, cars$decision)
tabla.seguridad.decision <- table(cars$safety, cars$decision)
tabla.puertas.decision <- table(cars$numberOfDoors, cars$decision)
tabla.personas.decision <- table(cars$numberOfPersons, cars$decision)



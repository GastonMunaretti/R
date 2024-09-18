# Ejemplo de Arbol de Decisión

# Paso 1: Importar los datos
# Paso 2: Limpiar los datos
# Paso 3: Crear los conjuntos de entranamieto y test
# Paso 4: Construir el modelo
# Paso 5: Hacer la predicción
# Paso 6: Medir el rendimiento del modelo
# Paso 7: Ajustar los hyper-parámetros

"El propósito del siguiente conjunto de datos titanic es
predecir que personas son más propensas a sobrevivir la 
colisión con el iceberg. El conjunto de datos contiene 13
variables y 1309 observaciones. Finalmente, este se 
encuentra ordenado por la variable X."



#limpiar variables
rm(list=ls())

#-----------------------------------------------------------------
set.seed(678)

# Importar los datos


# carga el path
path <- 'S:/Mineria de datos/ARBOLES/Titanic.csv'


# carga los datos
titanic <-read.csv(path)

# muestra la estructura de datos
str(titanic)

# ver tabla, devuelve las primeras filas de la matriz
head(titanic)

# ver tabla, devuelve las ultimas filas de la matriz
tail(titanic)

#-----------------------------------------------------------------

"Los datos no están ordenados aleatoriamente sino secuencialmente
de acuerdo a la variable categórica de interés. 
Esto es un problema importante y se debe corregir antes de dividir
los datos en entrenamiento y test. 
Para desordenar la lista de observaciones, se puede usar
la función sample()."

shuffle_index <- sample(1:nrow(titanic))

head(shuffle_index)


#Ahora se usa estos índices para generar un ordenamiento aleatorio del conjunto de datos.


titanic <- titanic[shuffle_index, ]
head(titanic)

#----------------------------------------------------------------------------

# Limpiar el conjunto de datos

"Existen valores NA's, por lo tanto deben ser eliminados.
 Prescindir de variables innecesarias
 Crear-convertir variables a tipo factor de ser necesario (e.g., Pclass y Survived)"

library(dplyr)

# %>% composicion de funciones de izquierda a derecha



# Drop variables
clean_titanic <- titanic %>%
  
  # se selecionan las variables Cabin, Name, Ticket. Se descartan. 
  select(-c(Cabin, Name, Ticket)) %>% 
  
  
  #Convierte un nivel a factor

  mutate(Pclass = factor(Pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         Survived = factor(Survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  
  # Omite las filas que contienen NA's . vacias
  na.omit()


#imprime datos transpuestos
glimpse(clean_titanic)


#----------------------------------------------------------------------------------------------

#Dividir en conjuntos de entrenamiento y test

# Carga libreria
library(dplyr)

# Toma el 80% de los datos para 
# data <- dplyr::sample_frac(clean_titanic,.8)
data_train <- clean_titanic %>% dplyr::sample_frac(.8)


# En clean_titanic se quitan los datos de  data_train de acuerdo a la columna PassengerId para obtener data_test
data_test  <- dplyr::anti_join(clean_titanic, data_train, by = 'PassengerId') # se debe tener un id

# Se eliman los indices de las tablas. ya no son datos necesarios
data_train <- dplyr::select(data_train, -PassengerId)

data_test <- dplyr::select(data_test, -PassengerId)



# Se muestra la tabla
head(data_train)

#---------------------------------------------------------------------------------

# Dimensiones de datos, matrices

dim(data_train)
dim(data_test)



# Ahora verificamos el proceso de aleatoriedad a través de las funciones prop.table() combinada con table().

# Proporcion de sobrevivientes
prop.table(table(data_train$Survived))
prop.table(table(data_test$Survived))

#---------------------------------------------------------------------------------

# Instalacion de paquetes
install.packages("rpart.plot")  

# Construir el modelo

" El comando para generar un modelo de árbol de decisión, usando la librería rpart lleva el mismo nombre."

# Cargar librerias
library(rpart)
library(rpart.plot)


# Se obtiene/estima el modelo
fit <- rpart(Survived~., data = data_train, method = 'class')


# Se grafica el arbol
rpart.plot(fit, extra = 106)

#Cada nodo muestra

"La clase predecida (died o survived),
 La probabilidad predecida de survival,
 El porcentaje de observaciones en el nodo"

# Diferentes vistas de grafico
rpart.plot(fit, extra = 4)

rpart.plot(fit, extra = 9)


#--------------------------------------------------------------------

# Hacer la predicción
predict_unseen <-predict(fit, data_test, type = 'class')

table_mat <- table(data_test$Survived, predict_unseen)

table_mat

"predict_unseen
 No     Yes
 No  76   9
 Yes 14  44 "





# Medir el rendimiento del modelo
# Accuracy = TP + TN / (TP + TN + FP + FN)

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


#-------------------------------------------------------------------------

# Ajustar los hyper-parámetros


accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$Survived, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}


# determinar parametro de complejidad cp optimo para el modelo
control <- rpart.control(minsplit = 2,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0.5)



# Se calcula el modelo a partir del parametro cp=0 , es el arbol completo, el mas complejo
tune_fit <- rpart(Survived~., data = data_train, method = 'class', control = control)

accuracy_tune(tune_fit)



#Base Model

hr_base_model <- rpart(Survived~., data = data_train, method = 'class', control = rpart.control(cp = 0))
summary(hr_base_model)

# Se grafica el modelo con el arbol completo, cp=0
rpart.plot(hr_base_model)

# Tabla CP , nsplit, error
printcp(hr_base_model)

# Se grafica ramas del arbol en funcion del cp a fin de determinar el cp optimo.
plotcp(hr_base_model)



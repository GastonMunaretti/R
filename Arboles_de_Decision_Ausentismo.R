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
path <- 'Q:/R/Ausentismo.csv'

# carga los datos
ausentismo <-read.csv(path, sep = ';')

# muestra la estructura de datos
str(ausentismo)

# ver tabla, devuelve las primeras filas de la matriz
head(ausentismo)

# ver tabla, devuelve las ultimas filas de la matriz
tail(ausentismo)

#-----------------------------------------------------------------



shuffle_index <- sample(1:nrow(ausentismo))

head(shuffle_index)


#Ahora se usa estos índices para generar un ordenamiento aleatorio del conjunto de datos.


ausentismo <- ausentismo[shuffle_index, ]
head(ausentismo)

#----------------------------------------------------------------------------

# Limpiar el conjunto de datos

library(dplyr)



ausentismo <- ausentismo #%>%
#select(-c(Ciudad))

#imprime datos transpuestos


ausentismo <- na.omit(ausentismo)
glimpse(ausentismo)


#----------------------------------------------------------------------------------------------

#Dividir en conjuntos de entrenamiento y test

# Carga libreria
library(dplyr)

# Toma el 80% de los datos para 
# data <- dplyr::sample_frac(clean_titanic,.8)
data_train <- ausentismo %>% dplyr::sample_frac(.8)


# En clean_titanic se quitan los datos de  data_train de acuerdo a la columna PassengerId para obtener data_test
data_test  <- dplyr::anti_join(ausentismo, data_train, by = 'IdUsuario') # se debe tener un id


# Se eliman los indices de las tablas. ya no son datos necesarios
data_train <- dplyr::select(data_train, -IdUsuario)

data_test <- dplyr::select(data_test, -IdUsuario)



head(data_train)

#---------------------------------------------------------------------------------

# Dimensiones de datos, matrices

dim(data_train)
dim(data_test)



# Aleatoriedad a través de las funciones prop.table() combinada con table().

# Proporcion de faltadores
prop.table(table(data_train$Faltador))
prop.table(table(data_test$Faltador))

#---------------------------------------------------------------------------------

# Instalacion de paquetes
#install.packages("rpart.plot")  

# Construir el modelo

" El comando para generar un modelo de árbol de decisión, usando la librería rpart lleva el mismo nombre."

# Cargar librerias
library(rpart)
library(rpart.plot)


# Se obtiene/estima el modelo
fit <- rpart(Faltador~., data = data_train, method = 'class')


# Se grafica el arbol
rpart.plot(fit, extra = 9)

#Cada nodo muestra



# Diferentes vistas de grafico
rpart.plot(fit, extra = 4)

rpart.plot(fit, extra = 9)


#--------------------------------------------------------------------

# predicción
predict_unseen <-predict(fit, data_test, type = 'class')

table_mat <- table(data_test$Faltador, predict_unseen)

table_mat




# Rendimiento del modelo
# Accuracy = TP + TN / (TP + TN + FP + FN)

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy', accuracy_Test))



# Ajustar los hyper-parámetros


accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$Faltador, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}


# determinar parametro de complejidad cp optimo para el modelo
control <- rpart.control(minsplit = 2,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0.5)



#Base Model

hr_base_model <- rpart(Faltador~., data = data_train, method = 'class', control = rpart.control(cp = 0))
summary(hr_base_model)

# Se grafica el modelo con el arbol completo, cp=0
rpart.plot(hr_base_model)

# Tabla CP , nsplit, error
printcp(hr_base_model)

# Se grafica ramas del arbol en funcion del cp a fin de determinar el cp optimo.
plotcp(hr_base_model)




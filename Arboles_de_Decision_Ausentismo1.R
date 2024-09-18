
rm(list=ls())



#install.packages("readxl")                                       # Install readxl R package
library("readxl")      



#-----------------------------------------------------------------
set.seed(678)

path <- 'Q:/R/Ausentismo.csv'

ausentismo <-read.csv(path, sep = ';')

#path <- 'Q:/R/Ausentismo.xlsx'

#ausentismo <- read_excel(path)


str(ausentismo)


head(ausentismo)

tail(ausentismo)

#-----------------------------------------------------------------



le_index <- sample(1:nrow(ausentismo))
head(le_index)
ausentismo <- ausentismo[le_index, ]
head(ausentismo)

#----------------------------------------------------------------------------

library(dplyr)



ausentismo <- ausentismo 
ausentismo <- na.omit(ausentismo)
glimpse(ausentismo)


#----------------------------------------------------------------------------------------------

library(dplyr)

data_train <- ausentismo %>% dplyr::sample_frac(.8)

data_test  <- dplyr::anti_join(ausentismo, data_train, by = 'IdUsuario') # se debe tener un id

data_train <- dplyr::select(data_train, -IdUsuario)

data_test <- dplyr::select(data_test, -IdUsuario)

head(data_train)

#---------------------------------------------------------------------------------

dim(data_train)
dim(data_test)
prop.table(table(data_train$Faltador))
prop.table(table(data_test$Faltador))

#---------------------------------------------------------------------------------


library(rpart)
library(rpart.plot)

fit <- rpart(Faltador~., data = data_train, method = 'class')

rpart.plot(fit, extra = 9)

rpart.plot(fit, extra = 4)

rpart.plot(fit, extra = 9)





predict_unseen <-predict(fit, data_test, type = 'class')

table_mat <- table(data_test$Faltador, predict_unseen)

table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy', accuracy_Test))



accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$Faltador, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}


control <- rpart.control(minsplit = 2,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)




hr_base_model <- rpart(Faltador~., data = data_train, method = 'class', control = control)
summary(hr_base_model)

rpart.plot(hr_base_model)

printcp(hr_base_model)

plotcp(hr_base_model)




library("e1071")

data <- read.csv("C:\\Users\\diego\\Dropbox\\Usach\\ANÁLISIS DE DATOS\\Lab\\allhypo.data",
                 col.names = c("age","sex", "on.thyroxine", "query.thyroxine","on.antithyroid.med", 
                               "sick","pregnant", "thyroid.surgery", "I131", "query.hypothyroid", 
                               "query.hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary",
                               "psych", "TSH.measured", "TSH", "T3.measured", "T3", "TT4.measured",
                               "TT4","T4U.measured", "T4U", "FTI.measured","FTI", "TBG.measured",
                               "TBG", "referral.source","state"), header=FALSE, sep=",", stringsAsFactors=FALSE)

# LIMPIEZA DE LA BASE DE DATOS

# Se limpia la variable de estado del paciente eliminando numeros
state <- data$state
state <- strsplit(state, ".", fixed = TRUE)
state <- lapply(state, `[[`, 1)
state <- unlist(state)
data$state <- state

# Se eliminan los pacientes que contengan algun NA
# Sex
error = data$sex == '?'
data = data[!error,]
# Age
error = data$age == '?'
data = data[!error,]
# On thyroxine
error = data$on.thyroxine == '?'
data = data[!error,]
# Query on antithyroid thyroxine
error = data$query.thyroxine == '?'
data = data[!error,]
# On antithyroid medication
error = data$on.antithyroid.med == '?'
data = data[!error,]
# Sick
error = data$sick == '?'
data = data[!error,]
# Pregnant
error = data$pregnant == '?'
data = data[!error,]
# Thyroid surgery
error = data$thyroid.surgery == '?'
data = data[!error,]
# I131 treatment
error = data$I131 == '?'
data = data[!error,]
# Query hypothyroid
error = data$query.hypothyroid == '?'
data = data[!error,]
# Query hyperthyroid
error = data$query.hyperthyroid == '?'
data = data[!error,]
# Lithium
error = data$lithium == '?'
data = data[!error,]
# Goitre
error = data$goitre == '?'
data = data[!error,]
# Tumor
error = data$tumor == '?'
data = data[!error,]
# Hypopituitary
error = data$hypopituitary == '?'
data = data[!error,]
# Psych
error = data$psych == '?'
data = data[!error,]
# TSH
error = data$TSH == '?'
data = data[!error,]
# T3
error = data$T3 == '?'
data = data[!error,]
# TT4
error = data$TT4 == '?'
data = data[!error,]
# T4U
error = data$T4U == '?'
data = data[!error,]
# FTI
error = data$FTI == '?'
data = data[!error,]

# Se eliminan las variables que indican si se realizo la medicion de la hormona
data$T3.measured <- NULL
data$TSH.measured <- NULL
data$TT4.measured <- NULL
data$T4U.measured <- NULL
data$FTI.measured <- NULL
data$TBG.measured <- NULL
#Se elimina la variable TBG que no fue medida para ningun paciente
data$TBG <- NULL
#Se elimina la variable referral.source
data$referral.source <- NULL

# -------------------------------------------------------------------------------------------------

# Una vez limpiada la base de datos, se transforman los datos correspondientes

# Variables continuas
data$T3 <- as.numeric(data$T3)
data$age <- as.numeric(data$age)
data$TSH <- as.numeric(data$TSH)
data$TT4 <- as.numeric(data$TT4)
data$T4U <- as.numeric(data$T4U)
data$FTI <- as.numeric(data$FTI)

# Variables binarias
data$sex <- as.factor(data$sex)
data$sick <- as.factor(data$sick)
data$I131 <- as.factor(data$I131)
data$state <- as.factor(data$state)
data$tumor <- as.factor(data$tumor)
data$psych <- as.factor(data$psych)
data$goitre <- as.factor(data$goitre)
data$lithium <- as.factor(data$lithium)
data$pregnant <- as.factor(data$pregnant)
data$on.thyroxine <- as.factor(data$on.thyroxine)
data$hypopituitary <- as.factor(data$hypopituitary)
data$query.thyroxine <- as.factor(data$query.thyroxine)
data$thyroid.surgery <- as.factor(data$thyroid.surgery)
data$query.hypothyroid <- as.factor(data$query.hypothyroid)
data$query.hyperthyroid <- as.factor(data$query.hyperthyroid)
data$on.antithyroid.med <- as.factor(data$on.antithyroid.med)

data_train <- data
data_test <- data
  
#Entrenamiento
model <- naiveBayes(state ~., data = data_train)

#Data predict
results <- predict(object = model, newdata=data_test, type = "class")

#Confusion Matrix - Predicted vs Trained
cm <- table(results,data_test$state)

#Percentage of hit (Accuracy)
overall_accuracy <- sum(diag(cm)) / sum(cm)
pv_compensated <- sum(cm[1,1]) / sum(cm[1,])
pv_negative <- sum(cm[2,2]) / sum(cm[2,])
pv_primary <- sum(cm[3,3]) / sum(cm[3,])


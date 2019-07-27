library(C50)

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

#Se obtiene la media de los datos continuos para establecerla de manear binaria
mediaAge <- mean(data$age)
mediaT3 <- mean(data$T3)
mediaT4U <- mean(data$T4U)
mediaTT4 <- mean(data$TT4)
mediaTSH <- mean(data$TSH)
mediaFTI <- mean(data$FTI)

#Se declaran los arreglos  con las nuevas variables binarias
newAge <- integer(length(data[[1]]))
newT3 <- integer(length(data[[1]]))
newT4U <- integer(length(data[[1]]))
newTT4 <- integer(length(data[[1]]))
newTSH <- integer(length(data[[1]]))
newFTI <- integer(length(data[[1]])) 

#Se transforman los datos continuos a binarios
for(i in 1:length(data[[1]])){
  #si la edad del paciente es mayor que la media, entonces es un paciente de edad avanzada
  if(data$age[i] >= mediaAge){newAge[i] <- "t"}else{newAge[i] <- "f"}
  #si la cantidad de hormona presente en el paciente es mayor que la media se considera como que posee altos niveles de esta
  if(data$T3[i] >= mediaT3){newT3[i] <- "t"}else{newT3[i] <- "f"}
  if(data$TSH[i] >= mediaTSH){newTSH[i] <- "t"}else{newTSH[i] <- "f"}
  if(data$TT4[i] >= mediaTT4){newTT4[i] <- "t"}else{newTT4[i] <- "f"}
  if(data$T4U[i] >= mediaT4U){newT4U[i] <- "t"}else{newT4U[i] <- "f"}
  if(data$FTI[i] >= mediaFTI){newFTI[i] <- "t"}else{newFTI[i] <- "f"} 
}
#Se binariza la columna state, aquellos valores falsos son lo que poseen negative y todos los demás son verdaderos, es decir, presentan hipotiroidismo
data$state <- ifelse(data$state %in% c("primary hypothyroid", "secondary hypothyroid", "compensated hypothyroid"), "t", "f")
#Se reemplazan las columnas correspondientes
data$state <- as.factor(data$state)
data$age <- as.factor(newAge)
data$T3 <- as.factor(newT3)
data$TT4 <- as.factor(newTT4)
data$T4U <- as.factor(newT4U)
data$TSH <- as.factor(newTSH)
data$FTI <- as.factor(newFTI)

#Se obtienen los datos que se utilizan para construir el arbol,
#es decir, todas las columnas menos state(la que define si el paciente presenta hipotiroidismo)
data_tree <- subset(data, select = -state)

#Se modela el arbol con la función C5.0 con los datos para el arbol y la variable data$state
model <-C5.0(data_tree, data$state)

#Se plotea el arbol
tree <- plot(model)
# Paquetes usados
require(ggplot2)
require(cluster)
require(Rtsne)

#Se obtiene los datos
data <- read.csv("C:\\Users\\diego\\Dropbox\\Usach\\ANÁLISIS DE DATOS\\Lab\\allhypo.data",
                 col.names = c("age","sex", "on.thyroxine", "query.thyroxine","on.antithyroid.med", 
                               "sick","pregnant", "thyroid.surgery", "I131", "query.hypothyroid", 
                               "query.hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary",
                               "psych", "TSH.measured", "TSH", "T3.measured", "T3", "TT4.measured",
                               "TT4","T4U.measured", "T4U", "FTI.measured","FTI", "TBG.measured",
                               "TBG", "referral.source","state"), 
                 header=FALSE, sep=",", stringsAsFactors=FALSE)

# LIMPIEZA DE LA BASE DE DATOS

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
# Dentro del clustering no se va a usar el state y el referral.source
data$state <- NULL
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


# Se vio en la base de datos que hay un caso en particular de un sujeto con 455 años de edad,
# por lo tanto lo que se realiza es eliminar los datos que pueden ser muy extremos para que 
# no afecten posteriormente al agrupamiento
# Age
error = data$age > 100
data = data[!error,]
# TSH
error = data$TSH > 15
data = data[!error,]
# T3
error = data$T3 > 10
data = data[!error,]
# TT4
error = data$TT4 > 460
data = data[!error,]
# T4U
error = data$T4U > 3.45
data = data[!error,]
# FTI
error = data$FTI > 405.0
data = data[!error,]

# Finalmente queda una muestra con 1874 sujetos de un total de 2800 lo que 
# corresponde al %66.92 aprox del total de datos 

# -------------------------------------------------------------------------------------------------

# CLUSTERING

# Matriz de disimilaridades con Distancia de Gower para variables continuas y binarias
disimilaridades.uno <- daisy(data, metric = "gower")

# METODO DE LAS SILUETAS 

# Numero de grupos para el clustering, el cual se observara el comportamiento de 2 a 10 grupos
cantidad.cluster = 2:10
siluetas <- c()

for (i in cantidad.cluster){
  kmedia <- pam(disimilaridades.uno, diss = TRUE, k = i)
  siluetas[i] <- kmedia$silinfo$avg.width
}
# Se grafica el resultado de los 2 a 10 grupos para saber con la cantidad de grupos que hay
# que quedarse
plot <- ggplot(data.frame(numero = cantidad.cluster, ancho = siluetas[2:10]), 
               aes(x = numero, y = ancho))+ labs(x = "Number of Clusters", y = "Silhouette Width")+
  geom_line(color = "seagreen3")+geom_point(color = "red")+
  ggtitle("Silhouette Width per number of clusters")+
  theme(plot.title = element_text(hjust = 0.5))

# Como resultado se aprecia que con un K = 4 es lo recomendable para realizar el agrupamiento

# CLUSTER PARA VARIABLES CONTINUAS Y BINARIAS EN CONJUNTO

# Se realiza el cluster 
clusters <- pam(disimilaridades.uno, diss = TRUE, k = 4)

# Se agrega la columna del cluster al cual pertenece el sujeto
data["grupo"] <- clusters$clustering

# Plot para graficar los grupos en plano de dos dimensiones
tsne <- Rtsne(disimilaridades.uno, is_distance = TRUE)
plot.clusters <- ggplot(data.frame(tsne$Y), aes(x = X1, y = X2))+
  labs(x = "x", y = "y")+
  geom_point(aes(color = factor(clusters$clustering)))+
  scale_colour_manual(values = c("red4", "seagreen3","yellow3","royalblue3"))

# Resumen del cluster realizado para cada grupo
resumen.c1 <- summary(data[data$grupo == 1, ])
resumen.c2 <- summary(data[data$grupo == 2, ])
resumen.c3 <- summary(data[data$grupo == 3, ])
resumen.c4 <- summary(data[data$grupo == 4, ])

# CLUSTER PARA VARIABLES CONTINUAS

# Usando solo variables continuas (numericas que tienen que ver con las hormonas)
data.continuo <- data[c(1, 17:21)]
 
# Matriz de disimilaridades con Distancia Euclideana 
disimilaridades.dos <- daisy(data.continuo, metric = "euclidean")

# Se realiza el cluster con K = 4
clusters.continuo <- pam(disimilaridades.dos, diss = TRUE, k = 4)

# Plot para graficar los grupos en plano de dos dimensiones
tsne.c <- Rtsne(disimilaridades.dos, is_distance = TRUE)
plot.clusters.continuo <- ggplot(data.frame(tsne.c$Y), aes(x = X1, y = X2))+
  labs(x = "x", y = "y")+
  geom_point(aes(color = factor(clusters.continuo$clustering)))+
  scale_colour_manual(values = c("red4", "seagreen3","yellow3", "royalblue3"))

# Se agrega la columna del cluster al cual pertenece el sujeto
data.continuo["grupo"] <- clusters.continuo$clustering

# Resumen de los clusters para cada grupo 
resumen.continuo.c1 <- summary(data.continuo[data.continuo$grupo == 1, ])
resumen.continuo.c2 <- summary(data.continuo[data.continuo$grupo == 2, ])
resumen.continuo.c3 <- summary(data.continuo[data.continuo$grupo == 3, ])
resumen.continuo.c4 <- summary(data.continuo[data.continuo$grupo == 4, ])
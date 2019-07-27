# Paquete
require(ggplot2)
# Funcion que realiza un qqplot utilizando ggplot 2
# Argumento: vector que corresponde a los datos a graficar
qqplot.data <- function (vec) 
{
  # Utilizando QQline
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  d <- data.frame(resids = vec)
  ggplot(d, aes(sample = resids)) + stat_qq() + 
    geom_abline(slope = slope, intercept = int) +
    labs(x = "Theoretical Quantiles" , y = "ordenated sample")
}

# Se leen los datos (desde mi direccion de archivos)
data <- read.csv("/home/diegoaguilat/Dropbox/Usach/ANÁLISIS DE DATOS/Lab/allhypo.data",
        col.names = c("age","sex", "on_thyroxine", "query_Thyroxine","on_antithyroid_med", "sick","pregnant", 
                      "thyroid_surgery", "I131", "query_hypothyroid", "query_hyperthyroid", "lithium", "goitre", 
                      "tumor", "hypopituitary", "psych", "TSH_measured", "TSH", "T3_measured", "T3", "TT4_measured", 
                      "TT4","T4U_measured", "T4U", "FTI_measured","FTI", "TBG_measured","TBG", "referral_source","state"), 
        header=FALSE, stringsAsFactors=FALSE)

# Se limpia la variable de estado del paciente eliminando numeros
state <- data$state
state <- strsplit(state, ".", fixed = TRUE)
state <- lapply(state, `[[`, 1)
state <- unlist(state)
data$state <- state
# La variable TGB no fue medida para ningún sujeto, asi que la descartamos
data$TBG <- NULL

# Se limpian los datos, eliminando los sujetos que contengan algun NA

# Sex
error = data$sex == '?'
data = data[!error,]
# Age
error = data$age == '?'
data = data[!error,]
# On thyroxine
error = data$on_thyroxine == '?'
data = data[!error,]
# Query on antithyroid thyroxine
error = data$query_Thyroxine == '?'
data = data[!error,]
# On antithyroid medication
error = data$on_antithyroid_med == '?'
data = data[!error,]
# Sick
error = data$sick == '?'
data = data[!error,]
# Pregnant
error = data$pregnant == '?'
data = data[!error,]
# Thyroid surgery
error = data$thyroid_surgery == '?'
data = data[!error,]
# I131 treatment
error = data$I131 == '?'
data = data[!error,]
# Query hypothyroid
error = data$query_hypothyroid == '?'
data = data[!error,]
# Query hyperthyroid
error = data$query_hyperthyroid == '?'
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
# TSH measured
error = data$TSH_measured == '?'
data = data[!error,]
# TSH
error = data$TSH == '?'
data = data[!error,]
# T3 measured
error = data$T3_measured == '?'
data = data[!error,]
# T3
error = data$T3 == '?'
data = data[!error,]
# TT4 measured
error = data$TT4_measured == '?'
data = data[!error,]
# TT4
error = data$TT4 == '?'
data = data[!error,]
# T4U measured
error = data$T4U_measured == '?'
data = data[!error,]
# T4U
error = data$T4U == '?'
data = data[!error,]
# FTI measured
error = data$FTI_measured == '?'
data = data[!error,]
# FTI
error = data$FTI == '?'
data = data[!error,]
# TBG measured
error = data$TBG_measured == '?'
data = data[!error,]
# Referral source
error = data$referral_source == '?'
data = data[!error,]
# State
error = data$state == '?'
data = data[!error,]

# Se obtienen las variables numericas
age <- as.numeric(data$age)
TSH <- as.numeric(data$TSH)
T3 <- as.numeric(data$T3)
TT4 <- as.numeric(data$TT4)
T4U <- as.numeric(data$T4U)
FTI <- as.numeric(data$FTI)

# Se obtienen las medidas de tendencia central y dispersion
# Funcion para obtener la moda de un vector de datos 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Se obtiene la media, moda, mediana y varianza
# Age
mean_age <- mean(age)
median_age <- median(age)
mode_age <- getmode(age)
var_age <- var(age)
# TSH
mean_TSH <- mean(TSH)
median_TSH <- median(TSH)
mode_TSH <- getmode(TSH)
var_TSH <- var(TSH)
# T3
mean_T3 <- mean(T3)
median_T3 <- median(T3)
mode_T3 <- getmode(T3)
var_T3 <- var(T3)
# TT4
mean_TT4 <- mean(TT4)
median_TT4 <- median(TT4)
mode_TT4 <- getmode(TT4)
var_TT4 <- var(TT4)
# T4U
mean_T4U <- mean(T4U)
median_T4U <- median(T4U)
mode_T4U  <- getmode(T4U)
var_T4U <- var(T4U)
# FTI
mean_FTI <- mean(FTI)
median_FTI <- median(FTI)
mode_FTI <- getmode(FTI)
var_FTI <- var(FTI)

#Se hace un dataframe con los valores de la media, moda, mediana y varianza
mean_values <- c(mean_age, mean_TSH, mean_T3, mean_TT4, mean_T4U, mean_FTI)
median_values <- c(median_age, median_TSH, median_T3, median_TT4, median_T4U, median_FTI)
var_values <- c(var_age, var_TSH, var_T3, var_TT4, var_T4U, var_FTI)
mode_values <- c(mode_age, mode_TSH, mode_T3, mode_TT4, mode_T4U, mode_FTI)
summary <- data.frame(col.names = c("Age","TSH","T3","TT4", "T4U", "FTI"), mean_values, 
                      median_values, mode_values, var_values, row.names = 1)
# Se muestra el dataframe con los valores de la media, medana y varianza
print(summary)

# Se realizo un test de normalidad de las variables numéricas utilizando la funcion en R shapiro.test
age_test <- shapiro.test(age)
tsh_test <- shapiro.test(TSH)
t3_test <- shapiro.test(T3)
tt4_test <- shapiro.test(TT4)
t4u_test <- shapiro.test(T4U)
fti_test <- shapiro.test(FTI)

#Se hace un dataframe con los p valores
p_values <- c(age_test$p.value, tsh_test$p.value, t3_test$p.value, tt4_test$p.value, t4u_test$p.value, fti_test$p.value)
normal_test <- data.frame(col.names = c("age","TSH","T3","TT4", "T4U", "FTI"), p_values, row.names = 1)

# Se interpreton las normalidades
result_test <- normal_test
result_test[result_test > 0.05] <- "Normal"
result_test[result_test <= 0.05] <- "No Normal"

# Se colocan los resultados en un frame final
normality_result <- normal_test
normality_result["normality"] <- NA
normality_result$normality <- result_test
# Se muestran los resultados 
print(normality_result)
# Se grafica dos ejemplo para ver como se comportan los datos
qqplot_age <- qqplot.data(age)
qqplot_tt4 <- qqplot.data(TT4)

# Graficos necesarios para en análisis estadístico de las variables

# Grafico general del estado de todos los sujetos
g1 <- ggplot(data, aes(state, fill=state)) +
      geom_bar(fill = "#FF6666") + 
  ggtitle("Number of People per Condition") +
  theme(plot.title = element_text(hjust = 0.5))

# Grafico general del estado de todos los sujetos separados por sexo
g2 <- ggplot(data, aes(state ,fill = sex))+
  geom_bar(stat = "count", position = position_dodge()) +
  scale_fill_manual("Sex", values = c("#FF6666","#52CBD9")) +
  ggtitle("Number of People per Condition per Sex")+
  theme(plot.title = element_text(hjust = 0.5))

# Grafico general del estado de todos los sujetes de acuerdo a si resiben terapia de reemplazo de Tiroxina
g3 <- ggplot(data, aes(state ,fill = on_thyroxine))+
  geom_bar(stat = "count", position = position_dodge()) +
  scale_fill_manual("Is On thryroxine Replacement?", values = c("#FF6666","#52CBD9")) +
  ggtitle("Number of People per Condition per On thyroxine")+
  theme(plot.title = element_text(hjust = 0.5))

# Grafico general del estado de todos los sujetos de acuerd a si reciben medicacion antitiroidea
g4 <- ggplot(data, aes(state ,fill = on_antithyroid_med))+
  geom_bar(stat = "count", position = position_dodge()) +
  scale_fill_manual("Is On antithyroid medication?", values = c("#FF6666","#52CBD9")) +
  ggtitle("Number of People per Condition per On antithyroid medication")+
  theme(plot.title = element_text(hjust = 0.5))

# Grafico general del estudio de todos los sujetos separados por estado de embarazo
g5 <- ggplot(data, aes(state ,fill = pregnant))+
  geom_bar(stat = "count", position = position_dodge()) +
  scale_fill_manual("Is pregnant?", values = c("#FF6666","#52CBD9")) +
  ggtitle("Number of People per Condition per Pregnant Status")+
  theme(plot.title = element_text(hjust = 0.5))

# Grafico general del estudio de todos los sujetos separados por si alguna vez se ha realizado una cirugia a la tiroides
g6 <- ggplot(data, aes(state ,fill = thyroid_surgery))+
  geom_bar(stat = "count", position = position_dodge()) +
  scale_fill_manual("Already had a thyroid surgery?", values = c("#FF6666","#52CBD9")) +
  ggtitle("Number of People per Condition per Surgery Status")+
  theme(plot.title = element_text(hjust = 0.5))

# Grafico general del estudio de todos los sujetos separados por si estan bajo el tratamiento I131
g7 <- ggplot(data, aes(state ,fill = I131))+
  geom_bar(stat = "count", position = position_dodge()) +
  scale_fill_manual("Is on I131 treatment?", values = c("#FF6666","#52CBD9")) +
  ggtitle("Number of People per Condition per I131 Treatment")+
  theme(plot.title = element_text(hjust = 0.5))

# Grafico general del estudio de todos los sujetos separados por si esta bajo el tratamiento de litio
g8 <- ggplot(data, aes(state ,fill = lithium))+
  geom_bar(stat = "count", position = position_dodge()) +
  scale_fill_manual("Is on lithium treatment?", values = c("#FF6666","#52CBD9")) +
  ggtitle("Number of People per Condition per Lithium Treatment")+
  theme(plot.title = element_text(hjust = 0.5))

  # Grafico general del estudio de todos los sujetos separados por si sufre de bocio
  g9 <- ggplot(data, aes(state ,fill = goitre))+
    geom_bar(stat = "count", position = position_dodge()) +
    scale_fill_manual("Has goitre condition?", values = c("#FF6666","#52CBD9")) +
    ggtitle("Number of People per Condition per Goitre Status")+
    theme(plot.title = element_text(hjust = 0.5))

# Grafico general del estudio de todos los sujetos separados por si sufre alguna enfermedad psiquiatrica 
g10 <- ggplot(data, aes(state ,fill = psych))+
  geom_bar(stat = "count", position = position_dodge()) +
  scale_fill_manual("Has psych condition?", values = c("#FF6666","#52CBD9")) +
  ggtitle("Number of People per Condition per psych Status")+
  theme(plot.title = element_text(hjust = 0.5))

# Correlacion
corr_frame <- data.frame(age, TSH, T3, TT4, T4U, FTI)
corr_frame <- cor(corr_frame, method = "pearson")
# Se muestran los valores de la correlacion
print(corr_frame)
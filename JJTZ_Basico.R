# options(echo = FALSE) # ECHO OFF
print('#######################################################################################################')
print('# Knocktober - 2016')
print('#######################################################################################################')
### Inicialización (setwd() y rm() y packages):

# setwd(getwd())
try(setwd('C:/Users/jtoharia/Dropbox/AFI_JOSE/Kaggle/Knocktober2016'), silent=TRUE)
try(setwd('C:/Personal/Dropbox/AFI_JOSE/Kaggle/Knocktober2016'), silent=TRUE)
rm(list = ls()) # Borra todos los elementos del entorno de R.

# install.packages("stringr")
library(stringr)
# install.packages("MASS")
library(MASS)
library(class)
# install.packages("e1071")
library(e1071)
# install.packages("kernlab")
library(kernlab)
# install.packages("glmnet")
library(Matrix)
p<-capture.output(
  library(foreach)
  , type = c("message")) # Capturamos los mensajes que devuelve esta función (para no mostrarlos)
p<-capture.output(
  library(glmnet)
  , type = c("message")) # Capturamos los mensajes que devuelve esta función (para no mostrarlos)
# install.packages("ROCR")
p<-capture.output(
  library(gplots)
  , type = c("message")) # Capturamos los mensajes que devuelve esta función (para no mostrarlos)
library(ROCR)
# install.packages("leaps")
library(leaps)
# install.packages("randomForest")
p<-capture.output(
  library(randomForest)
  , type = c("message")) # Capturamos los mensajes que devuelve esta función (para no mostrarlos)

# install.packages("monmlp") # Multi Layer Perceptron
library(monmlp)
# install.packages("RSNNS") # Multi Layer Perceptron
library(Rcpp)
library(RSNNS)
# # Importamos la función plot.nnet() desde Github (para el Multi Layer Perceptron)
# # Cf. https://beckmw.wordpress.com/tag/rsnns/
# # install.packages('devtools')
# library(devtools)
# # install.packages('reshape')
# library(reshape)
# source(file = 'nnet_plot_update.r') # La he descargado en local, por si acaso
# # source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

# Process in parallel:
# install.packages("doParallel")
p<-capture.output(
  library(foreach)
  , type = c("message")) # Capturamos los mensajes que devuelve esta función (para no mostrarlos)
library(iterators)
library(parallel)
library(doParallel)
# # Process in parallel: Ejemplo de uso:
# cl <- makeCluster(detectCores(), type='PSOCK') # library(doParallel) [turn parallel processing on]
# registerDoParallel(cl) # library(doParallel) [turn parallel processing on]
# registerDoSEQ() # library(doParallel) [turn parallel processing off and run sequentially again]
# #

rm(p) # eliminamos variable temporal para evitar mensajes al cargar libraries...

cl <- makeCluster(detectCores(), type='PSOCK') # library(doParallel) [turn parallel processing on]
registerDoParallel(cl) # library(doParallel) [turn parallel processing on]
# registerDoSEQ() # library(doParallel) [turn parallel processing off and run sequentially again]

# Para debug:
options(echo = TRUE)
# ##################################################
# ## Funciones:
# ##################################################
check_Go <- function()
{ if(!file.exists(paste0(Proyecto.s, '.GO')))
    stop(paste0('Fichero <', Proyecto.s, '.GO', '> NO encontrado. Detenemos el proceso...'))
}

Calcular_Outcome.merge_all_to_df_Full <- function(train_df, atend_camp_1, atend_camp_2, atend_camp_3, camps_df)
{
  train_df_camp_1 <- merge.data.frame(train_df, atend_camp_1, by = c("Patient_ID", "Health_Camp_ID"))
  train_df_camp_2 <- merge.data.frame(train_df, atend_camp_2, by = c("Patient_ID", "Health_Camp_ID"))
  train_df_camp_3 <- merge.data.frame(train_df, atend_camp_3, by = c("Patient_ID", "Health_Camp_ID"))
  train_df_camp_1$Number_of_stall_visited <- NA
  train_df_camp_2$Number_of_stall_visited <- NA
  train_df_camp_1$Last_Stall_Visited_Number <- NA
  train_df_camp_2$Last_Stall_Visited_Number <- NA
  train_df_camp_2$Donation <- NA
  train_df_camp_3$Donation <- NA
  train_df_camp_3$Health_Score <- NA
  colnames(train_df_camp_2)[colnames(train_df_camp_2)=="Health.Score"] <- "Health_Score"
  train_df_camp_1 <- cbind(train_df_camp_1[,1:(ncol(train_df))], Donation=train_df_camp_1$Donation, Health_Score=train_df_camp_1$Health_Score, Number_of_stall_visited=train_df_camp_1$Number_of_stall_visited, Last_Stall_Visited_Number=train_df_camp_1$Last_Stall_Visited_Number)
  train_df_camp_2 <- cbind(train_df_camp_2[,1:(ncol(train_df))], Donation=train_df_camp_2$Donation, Health_Score=train_df_camp_2$Health_Score, Number_of_stall_visited=train_df_camp_2$Number_of_stall_visited, Last_Stall_Visited_Number=train_df_camp_2$Last_Stall_Visited_Number)
  train_df_camp_3 <- cbind(train_df_camp_3[,1:(ncol(train_df))], Donation=train_df_camp_3$Donation, Health_Score=train_df_camp_3$Health_Score, Number_of_stall_visited=train_df_camp_3$Number_of_stall_visited, Last_Stall_Visited_Number=train_df_camp_3$Last_Stall_Visited_Number)
  names(train_df_camp_1)
  names(train_df_camp_2)
  names(train_df_camp_3)
  train_df_Full <- rbind(train_df_camp_1, train_df_camp_2, train_df_camp_3)
  
  # Incluimos el resto (los que no han asistido a ningún camp):
  train_df_Full <- merge.data.frame(train_df_Full, train_df, by = names(train_df), all = TRUE)
  
  return(train_df_Full)
}

Calcular_Outcome <- function(train_df, atend_camp_1, atend_camp_2, atend_camp_3, camps_df)
{
  # Incluimos los datos de los 3 camps:
  train_df_Full <- Calcular_Outcome.merge_all_to_df_Full(train_df, atend_camp_1, atend_camp_2, atend_camp_3, camps_df)
  # Agregamos algunos de estos datos por paciente:
  Patient_Donations <- aggregate(!is.na(train_df_Full[,"Donation"]), by = list(Patient_ID = train_df_Full$Patient_ID), FUN = sum)
  Patient_Scores <- aggregate(!is.na(train_df_Full[,"Health_Score"]), by = list(Patient_ID = train_df_Full$Patient_ID), FUN = sum)
  Patient_Stalls <- aggregate(!is.na(train_df_Full[,"Number_of_stall_visited"]), by = list(Patient_ID = train_df_Full$Patient_ID), FUN = sum)
  Patient_Stalls_Ok <- aggregate(!is.na(train_df_Full[,"Number_of_stall_visited"]) & train_df_Full[,"Number_of_stall_visited"] > 1, by = list(Patient_ID = train_df_Full$Patient_ID), FUN = sum)
  # Añadimos estos datos absolutos al df_Full (donde cada paciente está repetido):
  train_df_Full <- merge.data.frame(train_df_Full, Patient_Donations, by = c("Patient_ID"), all.x = TRUE)
  colnames(train_df_Full)[colnames(train_df_Full)=="x"] <- "Patient_Donations"
  train_df_Full <- merge.data.frame(train_df_Full, Patient_Scores, by = c("Patient_ID"), all.x = TRUE)
  colnames(train_df_Full)[colnames(train_df_Full)=="x"] <- "Patient_Scores"
  train_df_Full <- merge.data.frame(train_df_Full, Patient_Stalls, by = c("Patient_ID"), all.x = TRUE)
  colnames(train_df_Full)[colnames(train_df_Full)=="x"] <- "Patient_Stalls"
  train_df_Full <- merge.data.frame(train_df_Full, Patient_Stalls_Ok, by = c("Patient_ID"), all.x = TRUE)
  colnames(train_df_Full)[colnames(train_df_Full)=="x"] <- "Patient_Stalls_Ok"
  
  # Cálculo sencillo (para empezar):
  tip_camp <- as.integer(substr(train_df_Full$camp_cat, 1, 1))
  train_df_Full$Outcome[tip_camp==1 & !is.na(train_df_Full$Health_Score)] <- 1
  train_df_Full$Outcome[tip_camp==2 & !is.na(train_df_Full$Health_Score)] <- 1
  train_df_Full$Outcome[tip_camp==3 & !is.na(train_df_Full$Number_of_stall_visited)
                        & train_df_Full$Number_of_stall_visited > 1] <- 1
  summary(train_df_Full$Outcome)
  
  # Cálculo incluyendo los datos del Paciente: * PENDIENTE *
  
  # Ahora ponemos en train_df el Outcome obtenido (cruzando por Patient_ID,Health_Camp_ID):
  # Ordenamos train_df_Full por Patient_ID,Health_Camp_ID (seguramente ya esté ordenado por los joins)
  train_df_Full.ord <- train_df_Full[order(train_df_Full$Patient_ID, train_df_Full$Health_Camp_ID),]
  # y luego lo ponemos en train_df (con el orden que tenga train_df):
  train_df[order(train_df$Patient_ID, train_df$Health_Camp_ID),]$Outcome <- train_df_Full.ord$Outcome
  return(train_df$Outcome)
}

tratar_fechas <- function(fechas_vector, c_locale_format = "%d-%b-%y")
{
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  fechas_vector_out <- as.POSIXct(fechas_vector, format = c_locale_format)
  Sys.setlocale("LC_TIME", lct)
  return(fechas_vector_out)
}

# ########################################################################
# Función curvaROC() para dibujar la curva ROC y algunos valores del AUC de una prediction:
# ########################################################################
curvaROC <- function(prediction, titulo_grafico = "ROC")
{
  perf <- performance(prediction, measure = "tpr", x.measure = "fpr") 
  
  auc_100 <- performance(prediction, measure = "auc", x.measure = "fpr",  fpr.stop=1) 
  auc_100.y <- auc_100@y.values[[1]]
  auc_100.y.t <- round(auc_100.y, 4) # Para escribirlo en el plot
  
  auc_20 <- performance(prediction, measure = "auc", x.measure = "fpr",  fpr.stop=.2) 
  auc_20.y <- auc_20@y.values[[1]]
  auc_20.y.t <- round(auc_20.y, 4) # Para escribirlo en el plot
  
  cost.perf <- performance(prediction, "cost", cost.fp = 1, cost.fn = 1) # Falsos negativos y Falsos positivos cuestan lo mismo.
  cost.cutoff <- prediction@cutoffs[[1]][which.min(cost.perf@y.values[[1]])] # X > cost.cutoff => 1
  cost.cutoff.t <- paste0(100 * round(cost.cutoff,4), "%")
  # Error cometido (cost.cutoff):
  err.cost.cutoff <- prediction@tp[[1]][prediction@cutoffs[[1]] == cost.cutoff]
  err.cost.cutoff <- err.cost.cutoff + prediction@tn[[1]][prediction@cutoffs[[1]] == cost.cutoff]
  err.cost.cutoff <- err.cost.cutoff / length(prediction@labels[[1]])
  err.cost.cutoff <- 1 - err.cost.cutoff
  print(paste("Error cometido (opt.fpr) (testset) =", 100 * round(err.cost.cutoff,4), "%"))
  # plot(cost.perf)
  
  # fpr correspondiente a cost.cutoff:
  # perf <- performance(prediction, measure = "fpr") 
  # str(perf)
  fpr.stop.calc <- max(perf@x.values[[1]][perf@alpha.values[[1]] >= cost.cutoff])
  fpr.stop.calc.t <- round(fpr.stop.calc, 2) # Para escribirlo en el plot
  
  auc_opt <- performance(prediction, measure = "auc", x.measure = "fpr",  fpr.stop=fpr.stop.calc) 
  auc_opt.y <- auc_opt@y.values[[1]]
  auc_opt.y.t <- round(auc_opt.y, 2) # Para escribirlo en el plot
  
  par(mfrow = c(1,1))
  plot(perf, lty=1, col="orange", main = titulo_grafico)
  abline(v = .2) # Mostramos el AUC al 20%
  text(x = .2, y = 0, labels = paste("AUC =", auc_20.y.t, "(0.20)"), col = "gray60", pos = 3) # adj = c(0, -.1))
  abline(v = fpr.stop.calc) # Mostramos el frp opt calculado (fpr.stop.calc)
  text(x = fpr.stop.calc, y = .8, labels = paste0("AUC = ", auc_opt.y.t), col = "gray60", pos = 3) # adj = c(0, -.1))
  text(x = fpr.stop.calc, y = .7, labels = paste0("(opt=",fpr.stop.calc.t,")"), col = "gray60", pos = 3) # adj = c(0, -.1))
  text(x = fpr.stop.calc, y = .6, labels = paste0("(corte=", cost.cutoff.t, ")"), col = "gray60", pos = 3) # adj = c(0, -.1))
  text(x = (1.05 - (1 - fpr.stop.calc)/2), y = .3, labels = paste0("AUC = ", auc_100.y.t), col = "gray60", pos = 3) # adj = c(0, -.1))
}

# ########################################################################
# 1.a.i.- LDA - Linear Discriminant Analysis:
# ########################################################################
entrenar_lda <- function(train_df, test_df, curvaROC)
{
  # Dividimos train_df en trainset y testset: [La variable a predecir debe ser la primera columna y se llama Outcome]
  # Quitamos columna Outcome y la ponemos en primera posición:
  train_df <- cbind(Outcome = train_df$Outcome, train_df[ ,colnames(train_df) != "Outcome"])
  set.seed(1)
  index <- 1:nrow(train_df)
  porc_test <- 0.2
  testindex <- sample(index, trunc(length(index)*porc_test))
  testset <- train_df[testindex, ]
  trainset <- train_df[-testindex, ]
  
  if(length(unique(train_df$Outcome)) < 3)
  {
    trainset$Outcome <- factor(trainset$Outcome)
    testset$Outcome <- factor(testset$Outcome)
  }
  # Análisis Lineal Discriminante, con validación Cruzada (Leave-one-out):
  fit.CV_LDA <- lda(formula = Outcome ~ ., data = trainset, CV = TRUE) # CV == cross-validation
  
  # # fit$class contiene la prediccion de cada individuo (leave-one-out)
  # fit.CV_LDA$class # clase predicha
  # fit.CV_LDA$posterior # probabilidad a posteriori de pertenecer a cada grupo
  # # scatterplot de los grupos
  # pairs(trainset, main = "LDA - Outcome", pch = 19, col = c("steelblue4","indianred4")[fit.CV_LDA$class])
  
  print("Tabla de clasificación (trainset):")
  print(table(trainset$Outcome,fit.CV_LDA$class, deparse.level = 2))
  
  # Error cometido (trainset):
  err.CV_LDA <- (1 - mean(fit.CV_LDA$class == trainset$Outcome))
  print(paste("Error cometido (trainset) =", 100 * round(err.CV_LDA,4), "%"))
  
  # # Plot of the posterior probabilities: [Azul = Sí, Rojo = No]
  # n <- nrow(trainset)
  # plot(1:n, fit.CV_LDA$posterior[,"0"], main = "CV LDA - Probabilidad de Outcome 1", pch = 19, col = "steelblue4",
  #      xlab = "Nº de Observación", ylab = "Probabilidad a Posteriori")
  # points(1:n, fit.CV_LDA$posterior[,"1"], pch = 19, col = "indianred4")
  
  # Finalmente, LDA sin CV:
  #########################
  # Entrenamos (trainset):
  fit.LDA <- lda(formula = Outcome ~ ., data = trainset, CV = FALSE)
  # Predecimos (testset):
  predict.LDA <- predict(fit.LDA, newdata = testset[,-1])
  # Verificamos modelo (testset) (calculamos AUC, etc.):
  print("Tabla de clasificación (testset):")
  print(table(testset$Outcome, predict.LDA$class, deparse.level = 2))
  # Error cometido (testset):
  err.LDA <- 1 - mean(testset$Outcome==predict.LDA$class)
  print(paste("Error cometido (testset) =", 100 * round(err.LDA,4), "%"))
  # Calculamos AUC:
  pred.LDA <<- prediction(as.numeric(predict.LDA$posterior[,"1"]), testset$Outcome)
  auc.LDA <- performance(pred.LDA, measure = "auc", x.measure = "fpr",  fpr.stop=1) 
  auc.LDA.y <<- auc.LDA@y.values[[1]]
  print(paste("LDA - AUC =", auc.LDA.y))
  
  #############################
  # Curva ROC y AUC:
  pred <- pred.LDA
  tit.main <- "ROC - LDA"
  curvaROC(pred, tit.main)
  #############################
  
  # Predecimos en test_df:
  # ----------------------
  predict.LDA <- predict(fit.LDA, newdata = test_df[,-c(1,2)])
  probs <- as.numeric(predict.LDA$posterior[,"1"])
  return(probs)
}

# ########################################################################
# 1.a.vii.- Random Forest:
# ########################################################################
entrenar_randfor <- function(train_df, test_df, ntrees = 100, curvaROC)
{
  # Dividimos train_df en trainset y testset: [La variable a predecir debe ser la primera columna y se llama Outcome]
  # Quitamos columna Outcome y la ponemos en primera posición:
  train_df <- cbind(Outcome = train_df$Outcome, train_df[ ,colnames(train_df) != "Outcome"])
  set.seed(1)
  index <- 1:nrow(train_df)
  porc_test <- 0.25
  testindex <- sample(index, trunc(length(index)*porc_test))
  testset <- train_df[testindex, ]
  trainset <- train_df[-testindex, ]
  
  if(length(unique(train_df$Outcome)) < 3)
  {
    trainset$Outcome <- factor(trainset$Outcome)
    testset$Outcome <- factor(testset$Outcome)
  }
  # Entrenamos:
  memory.limit(size = 50000) # 50 GB !!!
  fit.RANDFOR <- randomForest(formula = trainset$Outcome ~ ., data = trainset[,-1],
                              importance = TRUE, proximity = TRUE,
                              ntree = ntrees, keep.forest = TRUE) # keep.forest = FALSE solo vale si se pasa el testset también
  # # Gráfico de las variables más importantes:
  # varImpPlot(fit.RANDFOR)
  
  print("Tabla de clasificación (trainset):")
  print(fit.RANDFOR$confusion)
  print(table(trainset$Outcome, fit.RANDFOR$predicted, deparse.level = 2))
  # Error cometido (trainset):
  err.TR_RANDFOR <- (1 - mean(fit.RANDFOR$predicted == trainset$Outcome))
  print(paste("Error cometido (trainset) =", 100 * round(err.TR_RANDFOR,4), "%"))
  
  # Finalmente, predecimos:
  #########################
  # Predecimos (testset):
  predict.RANDFOR       <- predict(fit.RANDFOR, testset[,-1], type = 'prob') # devuelve las probabilidades de pertenencia
  # Verificamos modelo (testset) (calculamos AUC, etc.):
  predict.RANDFOR.class <- predict(fit.RANDFOR, testset[,-1], type = 'response')
  print("Tabla de clasificación (testset):")
  print(table(testset$Outcome, predict.RANDFOR.class, deparse.level = 2))
  # Error cometido (testset):
  err.RANDFOR <- (1 - mean(predict.RANDFOR.class == testset$Outcome))
  print(paste("Error cometido (testset) =", 100 * round(err.RANDFOR,4), "%"))
  # Calculamos AUC:
  pred.RANDFOR <<- prediction(predict.RANDFOR[,"1"], testset$Outcome)
  auc.RANDFOR <- performance(pred.RANDFOR, measure = "auc", x.measure = "fpr",  fpr.stop=1) 
  auc.RANDFOR.y <<- auc.RANDFOR@y.values[[1]]
  print(paste("RANDFOR - AUC =", auc.RANDFOR.y))
  
  #############################
  # Curva ROC y AUC:
  pred <- pred.RANDFOR
  tit.main <- "ROC - RandomForest"
  curvaROC(pred, tit.main)
  #############################

  # Predecimos en test_df:
  # ----------------------
  predict.RANDFOR       <- predict(fit.RANDFOR, test_df[,-c(1,2)], type = 'prob') # devuelve las probabilidades de pertenencia
  probs <- as.numeric(predict.RANDFOR[,"1"])
  return(probs)
}

# ##################################################
# ## Inicio:
# ##################################################
########################################################################################################
Proyecto <- "Knocktober 2016"
########################################################################################################
print(paste('Proyecto =', Proyecto))

Proyecto.s <- str_replace_all(Proyecto, "\\(|\\)| |:", "_") # Quitamos espacios, paréntesis, etc.

# check_Go() # Verificamos fichero GO para salir con Ok o con error 9 (para batch)

# # Cargamos el sample_submission:
# sample_subm_df <- read.csv("sample_submission_cvwryvM.csv")

# Cargamos el test:
test_df <- read.csv("Test_D7W1juQ.csv", stringsAsFactors = F)

# # Creamos submission data.frame (con Outcome = 0):
# subm_df <- data.frame(cbind(test_df[,c("Patient_ID", "Health_Camp_ID")], 0))
# colnames(subm_df)[3] <- colnames(sample_subm_df)[3]
# rm(sample_subm_df) # Liberamos memoria
####################################################
## # subm_df_simple: Aleatorio
####################################################
crear_subm_df_simple <- function()
{
  # Ponemos un Outcome aleatorio (N(1/2,1/6)):
  subm_df_simple <- subm_df
  subm_df_simple[,3] <- rnorm(nrow(subm_df_simple), 0.5, 1/6)
  subm_df_simple[,3][subm_df_simple[,3] < 0] <- 0
  subm_df_simple[,3][subm_df_simple[,3] > 1] <- 1
  summary(subm_df_simple)
  hist(subm_df_simple[,3])
  
  write.csv(x = subm_df_simple, file = "jjtz_basic_subm.csv", row.names = FALSE)
  # rm(subm_df_simple) # Liberamos memoria
}
####################################################
## # Cargamos resto de datos:
####################################################
pacientes_df <- read.csv("Train_2/Patient_Profile.csv", stringsAsFactors = F)
camps_df     <- read.csv("Train_2/Health_Camp_Detail.csv", stringsAsFactors = F)
atend_camp_1 <- read.csv("Train_2/First_Health_Camp_Attended.csv")
atend_camp_2 <- read.csv("Train_2/Second_Health_Camp_Attended.csv")
atend_camp_3 <- read.csv("Train_2/Third_Health_Camp_Attended.csv")
train_df     <- read.csv("Train_2/Train.csv", stringsAsFactors = F)

if(all(is.na(atend_camp_1[,ncol(atend_camp_1)])))
  atend_camp_1 <- atend_camp_1[,-ncol(atend_camp_1)]

# any(is.na(atend_camp_1))
# any(is.na(atend_camp_2))
# any(is.na(atend_camp_3))

# Simplificamos los datos de los "camps", para usarlos al entrenar y al predecir:
# table(camps_df$Category1, camps_df$Category2)
# table(camps_df$Category2, camps_df$Category3)
camps_df$camp_cat <- ifelse(camps_df$Category1 == "First", "1", ifelse(camps_df$Category1 == "Second", "2", "3"))
camps_df$camp_cat <- paste0(camps_df$camp_cat, camps_df$Category2, camps_df$Category3)
camps_df$Category1 <- NULL # la quitamos
camps_df$Category2 <- NULL # la quitamos
camps_df$Category3 <- NULL # la quitamos
camps_df$camp_cat <- as.factor(camps_df$camp_cat)
# Fechas:
camps_df$Camp_Start_Date <- tratar_fechas(camps_df$Camp_Start_Date)
camps_df$Camp_End_Date <- tratar_fechas(camps_df$Camp_End_Date)

# Incluimos los datos de camps_df en train y en test:
train_df <- merge.data.frame(train_df, camps_df, by = c("Health_Camp_ID"))
test_df <- merge.data.frame(test_df, camps_df, by = c("Health_Camp_ID"))

# pacientes_df:
# Imputamos en Income el promedio de los demás:
pacientes_df$Income[pacientes_df$Income == "None"] <- mean(as.numeric(pacientes_df$Income[pacientes_df$Income != "None"]), na.rm = TRUE)
pacientes_df$Income <- as.numeric(pacientes_df$Income)
# Imputamos en Education_Score el promedio de los demás:
pacientes_df$Education_Score[pacientes_df$Education_Score == "None"] <- mean(as.numeric(pacientes_df$Education_Score[pacientes_df$Education_Score != "None"]), na.rm = TRUE)
pacientes_df$Education_Score <- as.numeric(pacientes_df$Education_Score)
# Imputamos en Age el promedio de los demás:
pacientes_df$Age[pacientes_df$Age == "None"] <- mean(as.numeric(pacientes_df$Age[pacientes_df$Age != "None"]), na.rm = TRUE)
pacientes_df$Age <- as.numeric(pacientes_df$Age)
# Fechas:
pacientes_df$First_Interaction <- tratar_fechas(pacientes_df$First_Interaction)
# Employer_Category: Categoría
pacientes_df$Employer_Category[pacientes_df$Employer_Category == ""] <- "Others"
pacientes_df$Employer_Category <- as.factor(pacientes_df$Employer_Category)
# City_Type: Categoría
pacientes_df$City_Type[pacientes_df$City_Type == ""] <- "Z"
pacientes_df$City_Type <- as.factor(pacientes_df$City_Type)
# Online_Follower,LinkedIn_Shared,Twitter_Shared,Facebook_Shared: Categoría binaria. Ya son integer...
pacientes_df$Online_Follower <- as.integer(pacientes_df$Online_Follower)
pacientes_df$LinkedIn_Shared <- as.integer(pacientes_df$LinkedIn_Shared)
pacientes_df$Twitter_Shared <- as.integer(pacientes_df$Twitter_Shared)
pacientes_df$Facebook_Shared <- as.integer(pacientes_df$Facebook_Shared)
# summary(pacientes_df)

# Incluimos los datos de pacientes_df en train y en test:
train_df <- merge.data.frame(train_df, pacientes_df, by = c("Patient_ID"))
test_df <- merge.data.frame(test_df, pacientes_df, by = c("Patient_ID"))

# Fechas:
train_df$Registration_Date <- tratar_fechas(train_df$Registration_Date)
test_df$Registration_Date <- tratar_fechas(test_df$Registration_Date)
train_df$Registration_Date[is.na(train_df$Registration_Date)] <- train_df[is.na(train_df$Registration_Date),]$Camp_Start_Date
test_df$Registration_Date[is.na(test_df$Registration_Date)] <- test_df[is.na(train_df$Registration_Date),]$Camp_Start_Date

# Obtenemos Outcome (probabilidad) usando train_df_Full:
train_df$Outcome <- 0
train_df$Outcome <- Calcular_Outcome(train_df, atend_camp_1, atend_camp_2, atend_camp_3, camps_df)
summary(train_df$Outcome)
# Ponemos Outcome como primera columna:
train_df <- cbind(Outcome = train_df$Outcome, train_df[ ,colnames(train_df) != "Outcome"])
# Ordenamos train_df por Patient_ID,Health_Camp_ID (por si acaso):
train_df <- train_df[order(train_df$Patient_ID, train_df$Health_Camp_ID),]
# Ponemos los IDs como nombres de filas:
rownames(train_df) <- paste0(train_df$Patient_ID, '-', train_df$Health_Camp_ID)
# Y quitamos los IDs para entrenar:
train_df.bak <- train_df # Backup
train_df$Patient_ID <- NULL
train_df$Health_Camp_ID <- NULL

####################################################
## # subm_df_simple2: Training casi a ciegas (22 vars)
####################################################
crear_subm_df_simple2 <- function(b_lda, b_randfor, n_randfor_ntree=10, b_randfor2, b_randfor3, nMaxBatchSize=10000, entrenar_randfor, curvaROC)
{
  if(b_lda){
    # ## LDA:
    # Entrenamos y Predecimos:
    probs <- entrenar_lda(train_df, test_df)
  
    # Preparamos submission df:
    subm_df <- data.frame(cbind(test_df[,c("Patient_ID", "Health_Camp_ID")], "Outcome" = 0.0))
    subm_df$Outcome <- as.numeric(probs)
    summary(subm_df)
    hist(subm_df$Outcome)
    plot(subm_df$Outcome)
    # Guardamos submission df:
    write.csv(x = subm_df, file = "jjtz_ml_lda_subm.csv", row.names = FALSE)
    print('Ok. Fichero creado.')
  }
  
  if(b_randfor){
    # ## Random Forest por camp_cat: (PERO SOLO EN LOS QUE ESTÁN EN TESTSET)
    
    # NOTA: Por problemas de memoria, lo hacemos por camp_cat (y, de paso, decubro ahora (!) que algunos campos no aparecen en test_df):
    # Entrenamos:
    cats_en_test_df <- names(table(test_df$camp_cat)[table(test_df$camp_cat) > 0])
    table(train_df$camp_cat, train_df$Outcome)
    table(train_df[train_df$camp_cat %in% cats_en_test_df,]$camp_cat)
    table(test_df$camp_cat) # !!!
    # Añadimos Outcome en test_df_tmp:
    test_df_tmp <- test_df
    test_df_tmp$Outcome <- 0.0
    for(mi_cat in cats_en_test_df)
    {
      mi_train_df <- train_df[train_df$camp_cat == mi_cat, ]
      mi_test_df <- test_df[test_df$camp_cat == mi_cat, ]
      # NOTA: Por problemas de tiempo y de memoria, si hay más de 10.000 hacemos varios samples de 10000:
      if(nrow(mi_train_df) > nMaxBatchSize)
      {
        nmax <- 1 + trunc(nrow(mi_train_df) / nMaxBatchSize)
        # data.frame con probs (de mi_test_df) en columnas:
        probs.v <- data.frame(t(vector(mode = "numeric", length = nmax)))
        # probs.v[,1:(nrow(mi_test_df))] <- 0 # Inicializamos el df entero!
        index <- 1:nrow(mi_train_df)
        nsize <- trunc(nrow(mi_train_df) / nmax)
        probs.v <- foreach(i = 1:nmax, .inorder=FALSE, .combine=cbind, .packages=c('randomForest', 'ROCR')) %dopar%
        {
          set.seed(i^2) # Semilla diferente para cada proceso en en paralelo...
          testindex <- sample(index, nsize)
          # probs.v[,i] <- t( entrenar_randfor(mi_train_df[testindex, ], mi_test_df, n_randfor_ntree, curvaROC) )
          entrenar_randfor(mi_train_df[testindex, ], mi_test_df, n_randfor_ntree, curvaROC)
        }
        probs <- sapply(data.frame(t(probs.v)), mean)
      } else
      {
        # Predecimos:
        probs <- entrenar_randfor(mi_train_df, mi_test_df, n_randfor_ntree, curvaROC)
      }
      # Guardamos las probs (Outcome):
      test_df_tmp[test_df_tmp$camp_cat == mi_cat, ]$Outcome <- probs
    }
    # Las probs finales son las que están en test_df_tmp (copia de test_df:
    probs <- test_df_tmp$Outcome
    
    # Preparamos submission df:
    subm_df <- data.frame(cbind(test_df[,c("Patient_ID", "Health_Camp_ID")], "Outcome" = 0.0))
    subm_df$Outcome <- as.numeric(probs)
    summary(subm_df)
    hist(subm_df$Outcome)
    plot(subm_df$Outcome)
    # Guardamos submission df:
    write.csv(x = subm_df, file = str_replace(str_replace("jjtz_ml_randfor_tTTTT_bBBBB_subm.csv", "TTTT", n_randfor_ntree), "BBBB", nMaxBatchSize), row.names = FALSE)
    print('Ok. Fichero creado.')
  }
  
  if(b_randfor2){
    # ## Random Forest por left(camp_cat, 1):
    
    # Entrenamos:
    table(substr(train_df$camp_cat,1,1), train_df$Outcome)
    table(substr(test_df$camp_cat,1,1)) # !!!
    cats2_en_test_df <- names(table(substr(test_df$camp_cat,1,1))[table(substr(test_df$camp_cat,1,1)) > 0])
    # Añadimos Outcome en test_df_tmp:
    test_df_tmp <- test_df
    test_df_tmp$Outcome <- 0.0
    # foreach(mi_cat = cats2_en_test_df) %dopar% 
    for(mi_cat in cats2_en_test_df)
    {
      mi_train_df <- train_df[substr(train_df$camp_cat,1,1) == mi_cat, ]
      mi_test_df <- test_df[substr(test_df$camp_cat,1,1) == mi_cat, ]
      # NOTA: Por problemas de tiempo y de memoria, si hay más de 10.000 hacemos varios samples de 10000:
      if(nrow(mi_train_df) > nMaxBatchSize)
      {
        nmax <- 1 + trunc(nrow(mi_train_df) / nMaxBatchSize)
        # data.frame con probs (de mi_test_df) en columnas:
        probs.v <- data.frame(t(vector(mode = "numeric", length = nmax)))
        # probs.v[,1:(nrow(mi_test_df))] <- 0 # Inicializamos el df entero!
        index <- 1:nrow(mi_train_df)
        nsize <- trunc(nrow(mi_train_df) / nmax)
        probs.v <- foreach(i = 1:nmax, .inorder=FALSE, .combine=cbind, .packages=c('randomForest', 'ROCR')) %dopar%
        {
          set.seed(i^2) # Semilla diferente para cada proceso en en paralelo...
          testindex <- sample(index, nsize)
          # probs.v[,i] <- t( entrenar_randfor(mi_train_df[testindex, ], mi_test_df, n_randfor_ntree, curvaROC) )
          entrenar_randfor(mi_train_df[testindex, ], mi_test_df, n_randfor_ntree, curvaROC)
        }
        probs <- sapply(data.frame(t(probs.v)), mean)
      } else
      {
        # Predecimos:
        probs <- entrenar_randfor(mi_train_df, mi_test_df, n_randfor_ntree, curvaROC)
      }
      # Guardamos las probs (Outcome):
      test_df_tmp[substr(test_df_tmp$camp_cat,1,1) == mi_cat, ]$Outcome <- probs
    }
    # Las probs finales son las que están en test_df_tmp (copia de test_df:
    probs <- test_df_tmp$Outcome
    
    # Preparamos submission df:
    subm_df <- data.frame(cbind(test_df[,c("Patient_ID", "Health_Camp_ID")], "Outcome" = 0.0))
    subm_df$Outcome <- as.numeric(probs)
    summary(subm_df)
    hist(subm_df$Outcome)
    plot(subm_df$Outcome)
    # Guardamos submission df:
    write.csv(x = subm_df, file = str_replace(str_replace("jjtz_ml_randfor2_tTTTT_bBBBB_subm.csv", "TTTT", n_randfor_ntree), "BBBB", nMaxBatchSize), row.names = FALSE)
    print('Ok. Fichero creado.')
  }
  
  if(b_randfor3){
    # ## Random Forest por left(camp_cat, 1): (PERO SOLO EN LOS QUE ESTÁN EN TESTSET) y con 4 iteraciones como mínimo
    # ## NOTA: Ponemos (n_randfor_ntree + i) en cada iteracción para que haya variedad...
    
    # Entrenamos:
    cats_en_test_df <- names(table(test_df$camp_cat)[table(test_df$camp_cat) > 0])
    table(substr(train_df$camp_cat,1,1), train_df$Outcome)
    table(substr(test_df$camp_cat,1,1)) # !!!
    cats3_en_test_df <- names(table(substr(test_df$camp_cat,1,1))[table(substr(test_df$camp_cat,1,1)) > 0])
    # Añadimos Outcome en test_df_tmp:
    test_df_tmp <- test_df
    test_df_tmp$Outcome <- 0.0
    # foreach(mi_cat = cats3_en_test_df) %dopar% 
    for(mi_cat in cats3_en_test_df)
    {
      mi_train_df <- train_df[substr(train_df$camp_cat,1,1) == mi_cat & train_df$camp_cat %in% cats_en_test_df,]
      mi_test_df <- test_df[substr(test_df$camp_cat,1,1) == mi_cat, ]
      # NOTA: Por problemas de tiempo y de memoria, si hay más de 10.000 hacemos varios samples de 10000:
      if(nrow(mi_train_df) > nMaxBatchSize)
      {
        nmax <- 1 + trunc(nrow(mi_train_df) / nMaxBatchSize)
      } else
      {
        nmax <- 1
      }
      # probs.v[,1:(nrow(mi_test_df))] <- 0 # Inicializamos el df entero!
      index <- 1:nrow(mi_train_df)
      nsize <- trunc(nrow(mi_train_df) / nmax)
      nmax <- nmax + 3 # Como mínimo haremos 4 iteraciones (aunque sea con el mismo train_df)
      # data.frame con probs (de mi_test_df) en columnas:
      probs.v <- data.frame(t(vector(mode = "numeric", length = nmax)))
      # Predecimos:
      probs.v <- foreach(i = 1:nmax, .inorder=FALSE, .combine=cbind, .packages=c('randomForest', 'ROCR')) %dopar% # .inorder=FALSE porque haremos el promedio al final...
      {
        set.seed(i^2) # Semilla diferente para cada proceso en en paralelo...
        testindex <- sample(index, nsize, replace = FALSE) # En el caso nmax <- 1 será el mismo mi_train_df en otro orden
        # probs.v[,i] <- t( entrenar_randfor(mi_train_df[testindex, ], mi_test_df, n_randfor_ntree + i, curvaROC) )
        # El print() en paralelo se pierde (en RStudio al menos...):
        print(paste0('entrenar_randfor(', i, '/', nmax, ') - ', nsize, ' regs - ', n_randfor_ntree + i, ' ntrees...'))
        entrenar_randfor(mi_train_df[testindex, ], mi_test_df, ntrees = n_randfor_ntree + i, curvaROC = curvaROC)
      }
      print(paste0('Ok. entrenar_randfor(', nmax, ' iters. x ', nsize, ' regs) - ', n_randfor_ntree, ' ntrees...'))
      probs <- sapply(data.frame(t(probs.v)), mean)
      # Guardamos las probs (Outcome):
      test_df_tmp[substr(test_df_tmp$camp_cat,1,1) == mi_cat, ]$Outcome <- probs
    }
    # Las probs finales son las que están en test_df_tmp (copia de test_df:
    probs <- test_df_tmp$Outcome
    
    # Preparamos submission df:
    subm_df <- data.frame(cbind(test_df[,c("Patient_ID", "Health_Camp_ID")], "Outcome" = 0.0))
    subm_df$Outcome <- as.numeric(probs)
    summary(subm_df)
    hist(subm_df$Outcome)
    # plot(subm_df$Outcome)
    # Guardamos submission df:
    write.csv(x = subm_df, file = str_replace(str_replace("jjtz_ml_randfor3_x4_tTTTT_bBBBB_subm.csv", "TTTT", n_randfor_ntree), "BBBB", nMaxBatchSize), row.names = FALSE)
    print('Ok. Fichero creado.')
  }
  
  # rm(subm_df_simple) # Liberamos memoria
}

# crear_subm_df_simple2(b_lda = FALSE, b_randfor = FALSE, b_randfor2 = FALSE, b_randfor3 = TRUE,
#                       n_randfor_ntree = 50,
#                       nMaxBatchSize = 10000,
#                       entrenar_randfor=entrenar_randfor, curvaROC=curvaROC)
# crear_subm_df_simple2(b_lda = FALSE, b_randfor = FALSE, b_randfor2 = FALSE, b_randfor3 = TRUE,
#                       n_randfor_ntree = 100,
#                       nMaxBatchSize = 10000,
#                       entrenar_randfor=entrenar_randfor, curvaROC=curvaROC)
# crear_subm_df_simple2(b_lda = FALSE, b_randfor = FALSE, b_randfor2 = FALSE, b_randfor3 = TRUE,
#                       n_randfor_ntree = 500,
#                       nMaxBatchSize = 10000,
#                       entrenar_randfor=entrenar_randfor, curvaROC=curvaROC)
# crear_subm_df_simple2(b_lda = FALSE, b_randfor = FALSE, b_randfor2 = FALSE, b_randfor3 = TRUE,
#                       n_randfor_ntree = 1000,
#                       nMaxBatchSize = 10000,
#                       entrenar_randfor=entrenar_randfor, curvaROC=curvaROC)
# (function(){ # Ejecutamos todo en una función anónima para no recargar el entorno con variables innecesarias
#   d10_10000 <- read.csv("jjtz_ml_randfor3_x4_t10_b10000_subm.csv")
#   d50_10000 <- read.csv("jjtz_ml_randfor3_x4_t50_b10000_subm.csv")
#   d100_10000 <- read.csv("jjtz_ml_randfor3_x4_t100_b10000_subm.csv")
#   d500_10000 <- read.csv("jjtz_ml_randfor3_x4_t500_b10000_subm.csv")
#   d1000_10000 <- read.csv("jjtz_ml_randfor3_x4_t1000_b10000_subm.csv")
#   d_10000 <- d1000_10000
#   d_10000[,3] <- (d10_10000[,3] + 5 * d50_10000[,3] + 10 * d100_10000[,3] + 50 * d500_10000[,3] + 100 * d1000_10000[,3]) / (1+5+10+50+100)
#   write.csv(x = d_10000, file = "jjtz_ml_randfor3_x4_tMean_b10000_subm.csv", row.names = FALSE)  
# })()
# 
# crear_subm_df_simple2(b_lda = FALSE, b_randfor = FALSE, b_randfor2 = FALSE, b_randfor3 = TRUE,
#                       n_randfor_ntree = 10,
#                       nMaxBatchSize = 15000,
#                       entrenar_randfor=entrenar_randfor, curvaROC=curvaROC)
# crear_subm_df_simple2(b_lda = FALSE, b_randfor = FALSE, b_randfor2 = FALSE, b_randfor3 = TRUE,
#                       n_randfor_ntree = 50,
#                       nMaxBatchSize = 15000,
#                       entrenar_randfor=entrenar_randfor, curvaROC=curvaROC)
# crear_subm_df_simple2(b_lda = FALSE, b_randfor = FALSE, b_randfor2 = FALSE, b_randfor3 = TRUE,
#                       n_randfor_ntree = 100,
#                       nMaxBatchSize = 15000,
#                       entrenar_randfor=entrenar_randfor, curvaROC=curvaROC)
# crear_subm_df_simple2(b_lda = FALSE, b_randfor = FALSE, b_randfor2 = FALSE, b_randfor3 = TRUE,
#                       n_randfor_ntree = 500,
#                       nMaxBatchSize = 15000,
#                       entrenar_randfor=entrenar_randfor, curvaROC=curvaROC)
# crear_subm_df_simple2(b_lda = FALSE, b_randfor = FALSE, b_randfor2 = FALSE, b_randfor3 = TRUE,
#                       n_randfor_ntree = 1000,
#                       nMaxBatchSize = 15000,
#                       entrenar_randfor=entrenar_randfor, curvaROC=curvaROC)
# (function(){ # Ejecutamos todo en una función anónima para no recargar el entorno con variables innecesarias
#   d10_15000 <- read.csv("jjtz_ml_randfor3_x4_t10_b15000_subm.csv")
#   d50_15000 <- read.csv("jjtz_ml_randfor3_x4_t50_b15000_subm.csv")
#   d100_15000 <- read.csv("jjtz_ml_randfor3_x4_t100_b15000_subm.csv")
#   d500_15000 <- read.csv("jjtz_ml_randfor3_x4_t500_b15000_subm.csv")
#   d1000_15000 <- read.csv("jjtz_ml_randfor3_x4_t1000_b15000_subm.csv")
#   d_15000 <- d1000_15000
#   d_15000[,3] <- (d10_15000[,3] + 5 * d50_15000[,3] + 10 * d100_15000[,3] + 50 * d500_15000[,3] + 100 * d1000_15000[,3]) / (1+5+10+50+100)
#   write.csv(x = d_15000, file = "jjtz_ml_randfor3_x4_tMean_b15000_subm.csv", row.names = FALSE)
# })()

crear_subm_df_simple2(b_lda = FALSE, b_randfor = TRUE, b_randfor2 = TRUE, b_randfor3 = TRUE,
                      n_randfor_ntree = 1500,
                      nMaxBatchSize = 15000,
                      entrenar_randfor=entrenar_randfor, curvaROC=curvaROC)


train_df <- train_df.bak # Restore Backup
registerDoSEQ() # library(doParallel) [turn parallel processing off and run sequentially again]

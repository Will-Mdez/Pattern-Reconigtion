#namefile <- "C://Users//alumnos//Documents//GitHub//Pattern-Reconigtion//datasets-20230307T150614Z-001//datasets//agaricus-lepiota.data"
#namefile <-"C://Users//alumnos.SALAC6-05//Documents//Pattern-Reconigtion-main//datasets-20230307T150614Z-001//datasets//agaricus-lepiota.data"
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//datasets-20230307T150614Z-001//datasets//agaricus-lepiota.data"
dataLepiota <- read.table(namefile, header = TRUE, sep = ",")
colSelec <- c(1,18,20,22,23)
dataFungi <- dataLepiota[,colSelec]
summary(dataFungi)


#Nombres a las columnas de datos
 
namesFungiD<- c("clase","veil-color","ring-type","population","habitat")
namesFeatures <- namesFungiD[2:5]
names(dataFungi) <- namesFungiD
#Convertir a categoricos
dataFungiF <- lapply(namesFungiD,function(x) factor(dataFungi[,x]))
dataFungiF <- as.data.frame(dataFungiF)
names(dataFungiF) <- namesFungiD
summary(dataFungiF)

Prob_clase<-table(dataFungiF$clase)
Prob_clase <- Prob_clase/dim(dataFungiF)[1]



Prob_clase_E_VeilColor<-table(dataFungiF$clase,dataFungiF$`veil-color`)
sumRows_Prob_clase_E_VeilColor<-rowSums(Prob_clase_E_VeilColor)
sumCols_Prob_clase_E_VeilColor<-colSums(Prob_clase_E_VeilColor)
tablaDT_NC_chi<-matrix(c((sumRows_tablaDT_NC[1]*sumCols_tablaDT_NC)/N,(sumRows_tablaDT_NC[2]*sumCols_tablaDT_NC)/N),nrow=2)


Prob_clase_E_RingType<-table(dataFungiF$clase,dataFungiF$`ring-type`)
sumRows_Prob_clase_E_RingType<-rowSums(Prob_clase_E_RingType)
sumCols_Prob_clase_E_RingTyper<-colSums(Prob_clase_E_RingType)


Prob_clase_E_Popu<-table(dataFungiF$clase,dataFungiF$population)
sumRows_Prob_clase_E_Popu<-rowSums(Prob_clase_E_Popu)
sumCols_Prob_clase_E_Popu<-colSums(Prob_clase_E_Popu)


Prob_clase_E_Habi<-table(dataFungiF$clase,dataFungiF$habitat)
sumRows_Prob_clase_E_Habi<-rowSums(Prob_clase_E_Habi)
sumCols_Prob_clase_E_Habi<-colSums(Prob_clase_E_Habi)

datafungiTrain <- dataFungiF[1:6000,]
dataFungiTest <- dataFungiF[6001:8124,]
Prob_clase<-table(datafungiTrain$clase)
Prob_clase <- Prob_clase/dim(datafungiTrain)[1]

#Verosimilitud-indep
verIndFC <- function(dataClase,dataFeat,namesFC){
  prob <- table(dataClase,dataFeat,dnn=namesFC)
  tot <- apply(prob,1,sum)
  probFC <- prob/tot
  return(probFC)
}


verosimiFeatClase <- lapply(namesFeatures, function(x) verIndFC(datafungiTrain[,1],datafungiTrain[,x],c("clase",x)))

clase4 <- verosimiFeatClase[[4]]
clase4[1,1]
names(verosimiFeatClase) <- namesFeatures
#PRUEBA CLASE

dataFungiTest[1,]
verosimiFeatFeat_VC <- verosimiFeatClase[["veil-color"]]
verosimiFeatFeat_RT <- verosimiFeatClase[["ring-type"]]
verosimiFeatFeat_P <- verosimiFeatClase[["population"]]
verosimiFeatFeat_H <- verosimiFeatClase[["habitat"]]

obj1 <- dataFungiTest[1,]
obj1
res1 <- verosimiFeatFeat_VC["e",obj1[["veil-color"]]]
valClases <- levels(dataFungiF$clase)
res1 <- sapply(namesFeatures, function(x) verosimiFeatClase[[x]][valClases[1],obj1[[x]]])
verosimTotprod <- prod(res1)

#prob posteriori
verosimTotprod*Prob_clase[1]

#OTRA PRUEBA
#p(clase=e)=p(clase=e)p(v=w,r=e,p=v,h=p|clase=e)
Resultado_test <- Prob_clase[1]*verosimiFeatFeat_VC["e","w"]*verosimiFeatFeat_RT["e","e"]*verosimiFeatFeat_P["e","v"]*verosimiFeatFeat_H["e","p"]
indObj <- c(1:dim(dataFungiTest)[1])


Result_Test<-sapply(indObj,function(x) sapply(namesFeatures,function(y) verosimiFeatClase[[y]][valClases[1],dataFungiTest[x,][[y]]]))
dim(Result_Test)
Result_Test <- t(Result_Test)
Result_Test_prod <- sapply(c(1:dim(dataFungiTest)[1]), function(X) prod(Result_Test[X,])*Prob_clase[1])

Result_Test2<-sapply(indObj,function(x) sapply(namesFeatures,function(y) verosimiFeatClase[[y]][valClases[2],dataFungiTest[x,][[y]]]))
dim(Result_Test2)
Result_Test2 <- t(Result_Test2)
Result_Test2_prod <- sapply(c(1:dim(dataFungiTest)[1]), function(X) prod(Result_Test2[X,])*Prob_clase[2])


valClases <- levels(dataFungiF$clase)
res1 <- sapply(namesFeatures, function(x) verosimiFeatClase[[x]][valClases[1],obj1[[x]]])
res1
vero_obj1 <- prod(res1)
vero_obj1
nrows <- nrow(dataFungiTest)
print(nrows)
Result_Test <- sapply(seq(1:nrows), function(y) sapply(namesFeatures, function(x) verosimiFeatClase[[x]][valClases[1],dataFungiTest[y,][[x]]]))
Result_Test <- t(Result_Test)
Result_Test_prod <- sapply(seq(1:nrows), function(x) prod(Result_Test[x,])*Prob_clase[1])
Result_Test_prod

Result_Test2 <- sapply(seq(1:nrows), function(y) sapply(namesFeatures, function(x) verosimiFeatClase[[x]][valClases[2],dataFungiTest[y,][[x]]]))
Result_Test2 <- t(Result_Test2)
Result_Test2_prod <- sapply(seq(1:nrows), function(x) prod(Result_Test2[x,])*Prob_clase[2])
Result_Test2_prod



claseEst <- data.frame(e=Result_Test_prod,p=Result_Test2_prod)
summary(claseEst)
claseEst$clase <- ifelse(claseEst$p > claseEst$e,'p','e')
claseEst
TablaFinal <- data.frame(PpostCe=Result_Test_prod,PpostCp=Result_Test2_prod,claseEst$clase,dataFungiTest$clase)
TablaFinal$claseEst.clase <- factor(TablaFinal$claseEst.clase)
summary(TablaFinal)

TP <- table(TablaFinal$claseEst.clase,TablaFinal$dataFungiTest.clase)
sumRows_TP<-rowSums(TP)
sumCols_TP<-colSums(TP)

Exactitud <- (TP[1,1]*TP[2,2])/sum(sumRows_TP)

Error <-  (TP[1,2]*TP[2,1])/sum(sumRows_TP)

Recall <-  (TP[1,1])/sumRows_TP[1]

Especificidad <-  (TP[2,2])/sumRows_TP[2]

Presicion <-  (TP[1,1])/TP[1,1]+TP[2,1]

F_Score <- 2*Presicion*Recall/Presicion*Recall



#K fold cross 
dataFungiFtraining <- datafungiTrain
dataFungiF_training <- datafungiTrain
#Desordenar datos de trainig-desrodenando indices
Ndata <- dim(dataFungiFtraining)[1]
indData <- sample(1:Ndata,Ndata,replace=F)
#Dividir en k partes, k=10

K <- 10
ndataFold <- Ndata/K
indFoldI <- seq(from=1,to=Ndata,by=ndataFold)
indFold_list <- lapply(indFoldI, function(x) seq(x,x+ndataFold-1))
fold <- 1
indTest <- indFold_list[[fold]]
restoFolds <- seq(2,K)
#Aqui no se desordenan los datos para ser seleccionado aleatoreamente
indTraining <- unlist(indFold_list[restoFolds])
dataTraining <- dataFungiF_training[indTraining,]
dataTest <- dataFungiF_training[indTest,]


#Aqui si

indTest <- indData[indFold_list[[fold]]]
indTraining <- indData[unlist(indFold_list[restoFolds])]
dataTraining <- dataFungiF_training[indTraining,]
dataTest <- dataFungiF_training[indTest,]
dim(dataTraining)
dim(dataTest)


#APLICACAMOS TODO DE NUEVO
Exactitud <- c()

Error <-  c()

Recall <-  c()

Especificidad <-  c()

Presicion <-  c()

F_Score <- c()



for (fold in 1:K) {
  
  indTest <- indData[indFold_list[[fold]]]
  restoFolds <- setdiff(1:K, fold)
  indTraining <- indData[unlist(indFold_list[restoFolds])]
  dataTraining <- dataFungiF_training[indTraining,]
  dataTest <- dataFungiF_training[indTest,]

  probClase <- table(dataTraining$clase)
  probClase <- probClase/dim(dataTraining)[1]
  probClase

  verosimFeatClase <- lapply(namesFeatures, function(x) verIndFC(dataTraining[,1],dataTraining[,x],c("clase",x)))

  names(verosimFeatClase) <- namesFeatures
  prob1 <- probClase[1]
  
  
  valClases <- levels(dataFungiF$clase)
  
  nrows <- nrow(dataTest)
  probabilidades_test_e <- sapply(seq(1:nrows), function(y) sapply(namesFeatures, function(x) verosimFeatClase[[x]][valClases[1],dataTest[y,][[x]]]))
  probabilidades_test_e <- t(probabilidades_test_e)
  Result_Test_prod <- sapply(seq(1:nrows), function(x) prod(probabilidades_test_e[x,])*probClase[1])

  
  Result_Test2_prod <- sapply(seq(1:nrows), function(y) sapply(namesFeatures, function(x) verosimFeatClase[[x]][valClases[2],dataTest[y,][[x]]]))
  Result_Test2_prod <- t(Result_Test2_prod)
  proba_p_test <- sapply(seq(1:nrows), function(x) prod(Result_Test2_prod[x,])*probClase[2])
 
  
  TablaFinal <- data.frame(e=Result_Test_prod,p=proba_p_test)
  TablaFinal$clase_est <- ifelse(TablaFinal$p > TablaFinal$e, 'p', 'e')
  TablaFinal$clase_real <- dataTest$clase
  matriz_confusion <- table(TablaFinal$clase_est,dataTest$clase)
 

  Exactitud <- c(Exactitud,((matriz_confusion[1]+matriz_confusion[4])/nrows))
  
  Error <-  c(Error,((matriz_confusion[2]+matriz_confusion[3])/nrows))
  
  Recall <-  c(Recall,(matriz_confusion[1]/(matriz_confusion[1]+matriz_confusion[3])))
  
  Especificidad <-  c(Especificidad,(matriz_confusion[4]/(matriz_confusion[2]+matriz_confusion[4])))
  
  Presicion <-  c(Presicion,(matriz_confusion[1]/(matriz_confusion[1]+matriz_confusion[2])))
  
  F_Score <- c(F_Score,((2*Presicion[length(Presicion)]*Recall[length(Recall)])/(Presicion[length(Presicion)]+Recall[length(Recall)])))
}

mean(Exactitud)
mean(Error)
mean(Recall)
mean(Especificidad)
mean(Presicion)
mean(F_Score)


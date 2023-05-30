namefile <- "C://Users//alumnos//Documents//GitHub//Pattern-Reconigtion//datasets-20230307T150614Z-001//datasets//agaricus-lepiota.data"
#namefile <-"C://Users//alumnos.SALAC6-05//Documents//Pattern-Reconigtion-main//datasets-20230307T150614Z-001//datasets//agaricus-lepiota.data"
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

obj1 <- dataFungiTest[1,][["population"]]
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


claseEst <- data.frame(e=Result_Test_prod,p=Result_Test2_prod)
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


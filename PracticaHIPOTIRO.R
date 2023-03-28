namefile <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//HIPOTIRO_CORTO.csv"
dataHipoTiro <- read.table(namefile, header = TRUE, sep = ",")

dataHipTir_Cuanti<-dataHipoTiro[,9:12]
summary(dataHipTir_Cuanti)

varianzas<-sapply(c(1,2,3,4), function(x) sd(dataHipTir_Cuanti[,x]))
print(varianzas)
c1<-c(varianzas[1],0,0,0)
c2<-c(0,varianzas[2],0,0)
c3<-c(0,0,varianzas[3],0)
c4<-c(0,0,0,varianzas[4])

matrizMahalanobis<-rbind(c1,c2,c3,c4)
matrizInversa<-solve(matrizMahalanobis)
matrizInversa

distanciaMahalanobis<-function(a,b){
  resta<-(a-b)
  restaT<-t(resta)
  dist<-sqrt(t(restaT) %*% matrizInversa %*% (restaT))
  return(dist)
}


N<-dim(dataHipTir_Cuanti)[1]
indObj<-seq(1:N)

distanciaObjetos<-sapply(indObj,function(x) sapply(indObj,function(y) distanciaMahalanobis(dataHipTir_Cuanti[x,],dataHipTir_Cuanti[y,])))

centro1<-dataHipTir_Cuanti[9,]
centro2<-dataHipTir_Cuanti[28,]
centro3<-dataHipTir_Cuanti[37,]


distanciaObjetosC1<-sapply(indObj,function(x)  distanciaMahalanobis(centro1,dataHipTir_Cuanti[x,]))
distanciaObjetosC2<-sapply(indObj,function(x) distanciaMahalanobis(centro2,dataHipTir_Cuanti[x,]))
distanciaObjetosC3<-sapply(indObj,function(x) distanciaMahalanobis(centro3,dataHipTir_Cuanti[x,]))

distanciaObjetosC1

matrizDistancias<-cbind(distanciaObjetosC1,distanciaObjetosC2,distanciaObjetosC3)

grupo <- apply(matrizDistancias,1,which.min)
grupo


indx1<-which(grupo %in% c(1))
indx2<-which(grupo %in% c(2))
indx3<-which(grupo %in% c(3))

Mediasc1<-apply(dataHipTir_Cuanti[indx1,],2,mean)
Mediasc2<-apply(dataHipTir_Cuanti[indx2,],2,mean)
Mediasc3<-apply(dataHipTir_Cuanti[indx3,],2,mean)
Mediasc1
Mediasc2
Mediasc3


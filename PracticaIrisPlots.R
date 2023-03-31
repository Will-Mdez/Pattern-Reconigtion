library(ggplot2)
library(dplyr)
library(tidyr)
library(car)

namefile <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//iris.data"
dataIris <- read.table(namefile, header = TRUE, sep = ",")


#NOmbres
namesIris<-c("sepal_l","sepal_w","petal_l","petal_w","clase")
names(dataIris)<-namesIris

#Convertir datos categoricos
dataIris$clase<-factor(dataIris$clase)
#DEcripcion
summary(dataIris)



#BAse
#pch:tipo de marcador
plot(dataIris$sepal_l,dataIris$petal_l,pch=19,col="black")
plot(dataIris$sepal_l,dataIris$petal_l,pch=19,col="red",xlab = "longitud sepalo",ylab = "longitud petalo")
plot(dataIris$sepal_l,dataIris$petal_l,pch=as.numeric(dataIris$clase),col=as.numeric(dataIris$clase),xlab = "longitud sepalo",ylab = "longitud petalo")

legend("bottomright",legend = c("setosa","versicolor","virginica"),
       pch=c(1,2,3),lty=c(1,2,3),col=c(1,2,3),lwd=2)
x11()


#Otra forma
scatterplot(dataIris$sepal_l,dataIris$petal_l,
            col=1,pch=15,boxplots = "",
            regLine=list(col="green",lwd=3),
            smooth=list(col.smooth="red",col.spread="blue"))

x11()        
scatterplot(dataIris$sepal_l,dataIris$petal_l,
            col=1,pch=15,boxplots = "",
            regLine=list(col="green",lwd=3),
            smooth=FALSE,
            xlab = "long sepalo",ylab = "long petalo")

x11()
scatterplot(dataIris$sepal_l,dataIris$petal_l,groups=dataIris$clase,
            by.groups=TRUE,
            xlab = "long sepalo",ylab = "long petalo")
x11()
ggplot(dataIris,aes(x=sepal_l,y=petal_l,colour=clase,shape=clase))+
  geom_point(size=3)+# points and color by group
  geom_smooth(method=lm, #add linear regresion
              se=FALSE)+
  xlab("long sepalo")+
  ylab("long petalo")+
  theme_bw()
  
x11()
ggplot(dataIris,aes(x=sepal_l,y=petal_l,colour=clase,shape=clase))+
  geom_point(size=3)+# points and color by group
  geom_smooth(method=lm, #add linear regresion
              se=TRUE)+ #and confidence
  xlab("long sepalo")+
  ylab("long petalo")+
  theme_bw()
  
x11()
pairs(~sepal_l+petal_l+sepal_w,data=dataIris,col=dataIris$clase,
      pch=as.numeric(dataIris$clase),main="Scatterplot Matrix")

x11()
scatterplotMatrix(~sepal_l+petal_l+sepal_w,data=dataIris,
      diagonal = FALSE, #remove kernel density estimates
      groups= dataIris$clase,
      by.groups=TRUE,
      regLine=list(col="green",lwd=3),#linear regresion line color, and linear regresssion line width
      smooth=list(col.smooth="red", #non parametric mean color
                  col.spread="blue") #non parametric variance color
      )
  
x11()
scatterplotMatrix(~sepal_l+petal_l+sepal_w,data=dataIris,
      diagonal = FALSE, #remove kernel density estimates
      groups= dataIris$clase,
      by.groups=TRUE,
      smooth=FALSE,legend=FALSE)


library(reshape2)
correlacion_df<-round(cor(dataIris[,1:4]),2)
print(correlacion_df)
#fusiona/ora sdatos de correlacion
melted_correlacion<-melt(correlacion_df)
print(melted_correlacion)
x11()
ggplot(data=melted_correlacion,aes(x=Var1,y=Var2,fill=value))+
  geom_tile()+
  geom_text(aes(Var2,Var1,label=value),size=5)+
  scale_fill_gradient2(low="blue",high = "brown",
                       limit=c(-1,1),name="Correlation")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())

m1 <- c(0.368,0.281,0.058,0.736,0.337,0.311,0.178,0.186,0.29,0.101)
m2 <- c(0.124,0.564,0.817,0.571,0.431,0.933,0.327,0.847,0.517,0.372)
c1 <- c(80,123,83,115,152,97,119,84,81,95)
c2 <- c(0.7,0.93,0.89,0.95,0.99,0.95,1.55,0.92,0.83,1.05)
b <- 1.5
(m1**b)*c1
m11 <- sum((m1**b)*c1)/sum(m1**1.5)
m11
m22 <- sum((m2**b)*c2)/sum(m2**1.5)
m22
VectorPEscalar1 <- c(m1**b,m2**b)
dataEx <- cbind(c1,c2,m1,m2) 


euclidiana <- function(a,b) (sqrt ( sum ((a - b) ^ 2)))

distancia1<-sapply(seq(1:2),function(x) euclidiana(m11,dataEx[x,]))
distancia1
distancia2<-sapply(seq(1:2),function(x) euclidiana(m22,dataEx[x,]))
distancia2

colum1<-((1/distancia1**2))**(1/(1.5-1))
colum1

colum2<-((1/distancia2**2))**(1/(1.5-1))
colum2

sumadistancias<-(((1/distancia1**2))**(1/(1.5-1)))+(((1/distancia2**2))**(1/(1.5-1)))
sumadistancias



pertenencia1 <- (1/distancia1**2)**(1/(1.5-1))/sumadistancias
pertenencia1
pertenencia2 <- (1/distancia2**2)**(1/(1.5-1))/sumadistancias
pertenencia2



TSH <- c(4.1,0.98,0.6,2.4,1.1,0.03,2.8,4.1,0.98,0.16)
TT4 <- c(102,109,123,83,115,171,97,102,109,175)
g1 <- c(1,2,10,9,8,6)
g2 <- c(3,4,5,7)
mediatsh1 <- mean(TSH[g1])
mediatsh1 <- mean(TSH[g2])
mediatsh1
mediatsh <- mean(TSH)
mediatt4 <- mean(TT4)
sdtsh <- sd(TSH)
sdtt4 <- sd(TT4)
mediatsh
mediatt4
sdtsh
sdtt4
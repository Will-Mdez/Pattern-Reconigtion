graduacionT<-c(0,0,1,0,0,1,0,0,1)
astigmatismoT<-c(0,0,0,0,0,1,0,0,1)
lagrimasT<-c(1,0,1,1,0,1,1,0,1)

transformados<-data.frame(graduacionT,astigmatismoT,lagrimasT)
a13<-1
b13<-0
c13<-1
d13<-1

a36<-2
b36<-0
c36<-1
d36<-0

emSimple13<-(a13+d13)/(a13+b13+c13+d13)
print(emSimple)
Pearson13<-((a13*d13)-(b13*c13))/sqrt
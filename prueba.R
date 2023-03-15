
R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"
Copyright (C) 2022 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from ~/.RData]

> peso <- c(0.23,0.21,0.23,0.29,0.29,0.31)
> profundidad<-c(61.5,59.8,56.9,56.9,62.4,63.3)
> precio<-c(356,326,327,327,334,335)
> sd(peso)
[1] 0.04147288
> sd(peso)**2
[1] 0.00172
> p1<-c(sd(peso)**2,0,0)
> p2<-c(0,sd(profundidad)**2,0)
> p3<-c(0,0,sd(precio)**2)
> matriz<-rbind(p1,p2,p3)
> matriz
[,1]     [,2]     [,3]
p1 0.00172 0.000000   0.0000
p2 0.00000 7.610667   0.0000
p3 0.00000 0.000000 129.3667
> sd(precio)**2
[1] 129.3667
> precio<-c(326,326,327,327,334,335)
> sd(precio)**2
[1] 17.36667
> p3<-c(0,0,sd(precio)**2)
> matriz<-rbind(p1,p2,p3)
> matriz
[,1]     [,2]     [,3]
p1 0.00172 0.000000  0.00000
p2 0.00000 7.610667  0.00000
p3 0.00000 0.000000 17.36667
> obj16<-c(peso[0]-peso[5],profundidad[0]-profundidad[5],precio[0]-precio[5])
> obj16
numeric(0)
> obj16
numeric(0)
> precio[1]
[1] 326
> precio[0]
numeric(0)
> obj16<-c(peso[1]-peso[6],profundidad[1]-profundidad[6],precio[1]-precio[6])
> obj16
[1] -0.08 -1.80 -9.00
> obj16t<-data.frame(t(obj16))
> obj16t
X1   X2 X3
1 -0.08 -1.8 -9
> obj16t2<-data.frame(t(obj16t))
> obj16t2
t.obj16t.
X1     -0.08
X2     -1.80
X3     -9.00
> distancia <- sqrt(obj16,solve(matriz),obj16t2)
Error in sqrt(obj16, solve(matriz), obj16t2) : 
  3 arguments passed to 'sqrt' which requires 1
> distancia <- sqrt(obj16%*%solve(matriz)%*%obj16t2)
Error in obj16 %*% solve(matriz) %*% obj16t2 : 
  requires numeric/complex matrix/vector arguments
> distancia <- sqrt(obj16 %*% solve(matriz) %*% obj16t2)
Error in obj16 %*% solve(matriz) %*% obj16t2 : 
  requires numeric/complex matrix/vector arguments
> prueba <- obj16 %*% solve(matriz)
> prueba
p1         p2         p3
[1,] -46.51163 -0.2365102 -0.5182342
> prueba2 <- prueba %*% obj16t2
Error in prueba %*% obj16t2 : 
  requires numeric/complex matrix/vector arguments
> prueba2 <- prueba %*% obj16t
Error in prueba %*% obj16t : 
  requires numeric/complex matrix/vector arguments
> prueba2 <- prueba %*% t(obj16)
Error in prueba %*% t(obj16) : non-conformable arguments
> 
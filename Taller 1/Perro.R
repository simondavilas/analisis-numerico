#Codigo Realizado por: Gabriel Gomez Corredor
#                      Simon Davila
#                      Juan Pablo Mendez

# PUNTO 6 DOCUMENTO 1

library(polynom)
library(PolynomF)
x=c(1,2,6,9,14,17.6,20,23,24.5,26.85,28.7,29,25.51,11.15,9.32,8.37,9.03,7.76,1) 
y=c(3,3.7,4.5,7.12,6.7,4.45,7,6.5,5.6,5.87,5.05,3.71,0.47,1.65,1.22,1.7,2.92,2.36,3)
plot(x,y, pch=19, cex=0.9, col = "blue", asp=1,xlab="X", ylab="Y", main="Perro ")
n=19
pint<-function(x,y){
  t = 1:length(x)
  sx = spline(t,x)
  sy = spline(t,y)
  lines(sx[[2]],sy[[2]],type='l', col="red")
}
pint(x,y)
#puntos origales / dados
yx=c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)                                    

xx=c(1,2,5,6,7.5,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30)      

#valores no seleccionados
inter = splinefun(x,y,method = "natural")
i=0
cat(x[c(i)],"      ",xx[c(i)])
for(i in 2:n-1){
  if(x[c(i)]!=xx[c(i)]){
    valorinter = inter(xx[i])
    cat(xx[i],",",yx[i],",",valorinter,"\n")
  }
}
yx=c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)                                    
xx=c(1,2,5,6,7.5,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30)      
#error relativo valores interpolados
cat("x   ","y    ","s(x)   ","   error relativo")  
inter = splinefun(x,y,method = "natural")
acumerrorrela=0
for(i in 2:n-1){
  valorinter = inter(xx[i])
  errabs=abs(yx[i]-valorinter)
  error = errabs/yx[i] * 100
  acumerrorrela=acumerrorrela+error
  cat(xx[i],",",yx[i],",",valorinter,",",error,"\n")
}
cat("error total:   ", acumerrorrela)
#error relativo valores originales
inter = splinefun(xx,yx,method = "natural")
for(i in 2:n-1){
  valorinter = inter(x[i])
  errabs=abs(y[i]-valorinter)
  error = errabs/y[i] * 100
  acumerrorrela=acumerrorrela+error
  cat(x[i],",",y[i],",",valorinter,",",error,"\n")
}
cat("error total:   ", acumerrorrela)
#cota valores interpolados
cota=0
acumulador=0

cat("x   ","y    ","f^4")
for(i in 2:n-1){
  cota=(x[c(i+1)]-x[c(i)])**4
  acumulador=acumulador+cota
  cat(x[c(i)],",",y[c(i)],",",cota,"\n")
}
cat(acumulador)
#cota valores originales
cota=0
acumulador=0
for(i in 3:n-2){
  cota=(xx[c(i+1)]-xx[c(i)])**4
  acumulador=acumulador+cota
  cat(xx[c(i)],",",yx[c(i)],",",cota,"\n")
}
cat(acumulador)
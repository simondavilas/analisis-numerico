#TALLER INTERPOLACION
#Realizado por: Gabriel Gomez, Juan Pablo Mendez, Simon Davila

rm(list=ls())


#PUNTO 1 -----------------------------------------------------------------------------------------------

#el polinomio interpolante que incluye a todos los puntos es unico (puntos distintos (xi,yi))

#Puntos: (-1,0),(1,3),(2,5),(4,2) 

mat <- matrix(c(-1,1,-1,1,1,1,1,1,8,4,2,1,27,9,3,1), nrow=4,ncol = 4, byrow=TRUE)
b <- c(0,3,5,2)

print(mat)
print(b)

det(mat) #corroborar que tenga solucion (det != 0)

coef_pol <- (solve(mat,b)) #se encuentran los coeficientes del polinomio

cat("Resultado Polinomio: ", coef_pol[1],"X^3 +",coef_pol[2],"x^2 +",coef_pol[3],"X +",coef_pol[4])


#PUNTO 2------------------------------------------------------------------------------------------------

#Dados los puntos (0,10), (1,15), (2,5) y f'(0)=1 ya que la tangente debe ser igual a 1 en X0(0)
mat <- matrix(c(0,0,0,1,1,1,1,1,8,4,2,1,0,0,1,0), nrow=4,ncol = 4, byrow=TRUE)
b <- c(10,15,5,1)

det(mat) #corroborar que tenga solucion (det != 0)

coef_pol <- (solve(mat,b)) #se encuentran los coeficientes del polinomio

print("Matriz resultante ")
print(mat)
print("vector de imagenes")
print(b)

print(coef_pol)

cat("Resultado Polinomio: ", coef_pol[1],"X^3 +",coef_pol[2],"x^2 +",coef_pol[3],"X +",coef_pol[4])


#PUNTO 3 ----------------------------------------------------------------------------------------------

#Dada la siguiente informacion: f(1)=2, f(2)=6, f'(1)=3, f'(2)=7, f''(2)=8

x <-c(1,1,2,2,2)
fx <-c(2,3,6,7,4)
coef_pol <-c(0)

coef_pol[1]=fx[1]
coef_pol[2]=fx[2]
coef_pol[3]=(4-fx[2])/(x[3]-x[1])
coef_pol[4]=((fx[4]-(fx[3]-fx[1]))-(coef_pol[3]))/(x[4]-x[1])
coef_pol[5]= ((fx[5]- (fx[4]-(fx[3]-fx[1])))-(coef_pol[4]))/(x[5]-x[1])
 
print(coef_pol)

cat("P(x):",coef_pol[1],"+",coef_pol[2],"( x-",x[1],")+",coef_pol[3],"( x-",x[2],")",x[3],"+",coef_pol[4],"( x-",x[2],")",x[3],"( x-",x[3],")",coef_pol[5],"( x-",x[2],")",x[4],"( x-",x[3],")",x[5])

cat(" Simplificado -> P(x): -x^4 +8x^3 -20x^2 +23x-8")


#PUNTO 4 ------------------------------------------------------------------------------------------------

#preguntar error intervalo [1,2]

#Funcion f(x)= lnx , construir la interpolacion de diferencias divididas, x0=1, x1=2

f <- function(x){
  return(log(x))
}


fx <- c(0)
x <- c(0)
cont <- 1


for (i in 1:5){
  
  imagen <- f(i)
  fx[cont]=imagen
  x[cont]=i
  cont=cont+1
}

tabla = data.frame(x,fx)
print(tabla)

#crear vector para almacenar resultados de diferencias divididas

dif_divi <- c(0)
cont = 1

dif_divi[cont] = (fx[cont+1]-fx[cont])/(x[cont+1]-x[cont])
cont=cont+1

while( cont < 5){
  
  dif_divi[cont] = (fx[cont+1]-fx[cont])/(x[cont+1]-x[cont]) 

  cont=cont+1

}

print(dif_divi)

#debido a un x=1.5 se halla el error en [1,2]

lagra <- abs((1.5-x[1])*(1.5-x[2])/factorial(2))

f = expression(log(x))

Lagrange_Error <-function(f,Grado,sec){
  x=0;
  while (x < Grado){
    f= D(f,'x');
    x=x+1;
  }
  return (sec*abs(eval(f)));
}


cat("Error: ",Lagrange_Error(f,2,lagra))  #se evalua la segunda derivada en 2 por [1,2]


#PUNTO 5 -------------------------------------------------------------------------------------


# INTERPOLACION DE SPLINES 

#PERRO

library(stats)
x = c(00.50, 01.01, 05.85, 07.46, 11.28, 15.20, 18.46, 21.25, 24.15, 25.80, 28.00, 30.80, 30.81, 29.40, 27.40, 26.21, 24.97, 20.32, 19.54, 18.80, 14.04, 12.54, 11.68, 09.55, 08.30, 09.10, 08.85, 07.80, 00.50)
y = c(02.40, 02.95, 03.86, 05.41, 07.45, 06.30, 04.49, 07.15, 07.05, 05.80, 05.85, 04.50, 02.40, 01.20, 00.80, 00.44, 00.54, 01.01, 00.80, 01.08, 00.98, 01.08, 01.33, 01.00, 01.64, 02.65, 02.70, 02.24, 02.40)
plot(x,y,main = "Interpolación perro", asp = 1)
vx1 = c(x[1:4])
vy1 = c(y[1:4])
splines = splinefun(vx1,vy1, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx1[1], to = vx1[length(vx1)])
vx2 = c(x[4:7])
vy2 = c(y[4:7])
splines = splinefun(vx2,vy2, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx2[1], to = vx2[length(vx2)])
vx3 = c(x[7:12])
vy3 = c(y[7:12])
splines = splinefun(vx3,vy3, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx3[1], to = vx3[length(vx3)])
vx4 = c(x[12:13])
vy4 = c(y[12:13])
splines = splinefun(vx4,vy4, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx4[1], to = vx4[length(vx4)])
vx5 = c(x[13:18])
vy5 = c(y[13:18])
splines = splinefun(vx5,vy5, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx5[1], to = vx5[length(vx5)])
vx6 = c(x[18:25])
vy6 = c(y[18:25])
splines = splinefun(vx6,vy6, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx6[1], to = vx6[length(vx6)])
vx7 = c(x[25:26])
vy7 = c(y[25:26])
splines = splinefun(vx7,vy7, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx7[1], to = vx7[length(vx7)])
vx8 = c(x[26:28])
vy8 = c(y[26:28])
splines = splinefun(vx8,vy8, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx8[1], to = vx8[length(vx8)])
vx9 = c(x[28:29])
vy9 = c(y[28:29])
splines = splinefun(vx9,vy9, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx9[1], to = vx9[length(vx9)])



# MANO


x=c(14.6, 14.7, 14.8, 15.2, 15.6, 15.7, 17.0, 17.6, 17.5, 17.3, 16.8, 15.4, 14.8, 14.4, 14.5, 15.0, 15.1, 15.0, 14.9, 14.3, 14.0, 13.9, 13.8, 13.5, 13.1, 13.0, 13.3, 13.2, 12.9, 12.4, 11.9, 11.7, 11.6, 11.3, 10.9, 10.7, 10.6, 10.1, 9.7, 9.4, 9.3, 9.6, 9.9, 10.1, 10.2, 10.3, 9.10, 8.6, 7.5, 7.0, 6.7, 6.6, 7.70, 8.00, 8.10, 8.40, 9.00, 9.30, 10, 10.2, 10.3, 10.0, 9.50)                                                                                                      
y=c(14.7, 14.0, 12.3, 11.0, 10.5, 10.2, 8.20, 7.10, 6.70, 6.60, 6.80, 8.30, 8.80, 9.30, 8.80, 6.30, 5.50, 5.00, 4.70, 4.50, 4.90, 5.40, 5.80, 6.90, 8.20, 7.60, 5.80, 4.50, 3.90, 4.20, 5.70, 7.00, 7.90, 8.20, 7.30, 6.70, 5.50,  4.60, 4.7, 5.0, 5.5, 7.2, 7.8, 8.60, 9.40, 10.0, 10.7, 9.9, 9.0, 9.1, 9.3, 9.7, 11.7, 12.3, 12.5, 13.0, 13.9, 14.9, 16, 16.4, 16.8, 10.7, 11.0)
plot(x,y,main = "Interpolación mano", asp = 1)
vx1 = c(x[1:3])
vy1 = c(y[1:3])
splines = splinefun(vx1,vy1, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx1[1], to = vx1[length(vx1)])
vx2 = c(x[3:5])
vy2 = c(y[3:5])
splines = splinefun(vx2,vy2, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx2[1], to = vx2[length(vx2)])
vx3 = c(x[5:8])
vy3 = c(y[5:8])
splines = splinefun(vx3,vy3, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx3[1], to = vx3[length(vx3)])
vx4 = c(x[8:10])
vy4 = c(y[8:10])
splines = splinefun(vx4,vy4, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx4[1], to = vx4[length(vx4)])
vx5 = c(x[10:13])
vy5 = c(y[10:13])
splines = splinefun(vx5,vy5, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx5[1], to = vx5[length(vx5)])
vx6 = c(x[13:14])
vy6 = c(y[13:14])
splines = splinefun(vx6,vy6, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx6[1], to = vx6[length(vx6)])
vx7 = c(x[14:17])
vy7 = c(y[14:17])
splines = splinefun(vx7,vy7, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx7[1], to = vx7[length(vx7)])
vx8 = c(x[17:23])
vy8 = c(y[17:23])
splines = splinefun(vx8,vy8, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx8[1], to = vx8[length(vx8)])
vx9 = c(x[23:26])
vy9 = c(y[23:26])
splines = splinefun(vx9,vy9, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx9[1], to = vx9[length(vx9)])
vx10 = c(x[26:27])
vy10 = c(y[26:27])
splines = splinefun(vx10,vy10, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx10[1], to = vx10[length(vx10)])
vx11 = c(x[27:28])
vy11 = c(y[27:28])
splines = splinefun(vx11,vy11, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx11[1], to = vx11[length(vx11)])
vx12 = c(x[28:33])
vy12 = c(y[28:33])
splines = splinefun(vx12,vy12, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx12[1], to = vx12[length(vx12)])
vx13 = c(x[33:37])
vy13 = c(y[33:37])
splines = splinefun(vx13,vy13, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx13[1], to = vx13[length(vx13)])
vx14 = c(x[37:41])
vy14 = c(y[37:41])
splines = splinefun(vx14,vy14, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx14[1], to = vx14[length(vx14)])
vx15 = c(x[41:46])
vy15 = c(y[41:46])
splines = splinefun(vx15,vy15, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx15[1], to = vx15[length(vx15)])
vx16 = c(x[46:52])
vy16 = c(y[46:52])
splines = splinefun(vx16,vy16, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx16[1], to = vx16[length(vx16)])
vx17 = c(x[52:57])
vy17 = c(y[52:57])
splines = splinefun(vx17,vy17, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx17[1], to = vx17[length(vx17)])
vx18 = c(x[57:61])
vy18 = c(y[57:61])
splines = splinefun(vx18,vy18, method = "fmm")
curve(splines(x), add = TRUE, col = 1, from = vx18[1], to = vx18[length(vx18)])



#PUNTO 7--------------------------------------------------------------------------------------

# Con F(x)=e^x Determinar el tamaño del paso por el metodo de Lagrange con una tolerancia de 10e-5


f <- function( x ) { exp(x) }
lagrange = function(x,y,a){ 
  n = length(x) 
  if(a < min(x) || max(x) < a) stop("No está interpolando")
  X = matrix(rep(x, times=n), n, n, byrow=T)
  mN = a - X; diag(mN) = 1
  mD = X - t(X); diag(mD) = 1
  Lnk = apply(mN, 1, prod)/apply(mD, 2, prod)
  sum(y*Lnk)
}
x<-c(0,0.25,0.5,0.75,1)
y<-f(x[1:5])
lagrange(x[1:5],y[1:5],0.5)

require(pracma)
f = function(x) exp(x)
g= function (x) 1/x
polinomio_1= taylor(f, 0, 4) 
polinomio_2= taylor(g, 5, 4) 
polinomio_3=taylor(g,-5,4)
curve(f, col= "blue", from = -10, to= 10)
curve(polinomio_1[1]*x^(4)+ppolinomio_1[2]*x^(3)+polinomio_1[3]*x^(2)+ppolinomio_1[4]*x+polinomio_1[5],add=TRUE, col="green", from = -100, to= 100)
curve(g, col= "purple", from = -10, to= 10)
curve(polinomio_2[1]*x^(4)+polinomio_2[2]*x^(3)+polinomio_2[3]*x^(2)+polinomio_2[4]*x+polinomio_2[5],add=TRUE, col="yellow", from = -100, to= 100 )
curve(polinomio_3[1]*x^(4)+polinomio_3[2]*x^(3)+polinomio_3[3]*x^(2)+polinomio_3[4]*x+polinomio_3[5],add=TRUE, col="salmon", from = -100, to= 100 )
cat(polinomio_1[1],"x^(4)+",polinomio_1[2],"*x^(3)+",polinomio_1[3],"*x^(2)+",polinomio_1[4],"x+",polinomio_1[5],"/n")
cat(polinomio_2[1],"x^(4)+",polinomio_2[2],"*x^(3)+",polinomio_2[3],"*x^(2)+",polinomio_2[4],"x+",polinomio_2[5],"/n")
cat(polinomio_3[1],"x^(4)+",polinomio_3[2],"*x^(3)+",polinomio_3[3],"*x^(2)+",polinomio_3[4],"x+",polinomio_3[5],"/n")

#PUNTO 8 ---------------------------------------------------------------------------------------

  x<-c(100,200,300,400,500,600)
  y<-c(-160,-35,-4.2,9.0,16.9,21.3)
  
  mat <- matrix(c(0,0,0,0,0,0), nrow=1,ncol = 6, byrow=TRUE)
  
  options(digits = 16)
  vec<-c(0)
  
  for(i in 1:length(x)){
    
    for(j in 0:length(x)){
      
      vec[j+1]=x[i]^j
      
  
    }
    mat<-rbind(mat,c(vec))
  }
  
  mat <- mat[1:length(x)+1,]
  
  
  
  
  coef_pol <- (solve(mat,y))     #el solve solo sirve para matrices cuadradas   
  
  cat("Resultado Polinomio: ", coef_pol[6],"X^5 +",coef_pol[5],"x^4 +",coef_pol[4],"X^3 +",coef_pol[3],"X^2 +",coef_pol[2],"X +",coef_pol[1])
  
  cat("F(450) = ",coef_pol[6]*(450)^{5}+coef_pol[5]*(450)^{4} +coef_pol[4]*(450)^{3} +coef_pol[3]*(450)^{2} + coef_pol[2]*(450)+coef_pol[1])
  cat ("el segundo viral es:", coef_pol[2]*(450), "o es: ",coef_pol[2]) 
  cat ("el segundo viral es:", coef_pol[3]*(450)^{2}, "o es: ",coef_pol[3])
  
  
  
parabola=function(x,y) coef_pol[6]*(x)^{5}+coef_pol[5]*(x)^{4} +coef_pol[4]*(x)^{3} +coef_pol[3]*(x)^{2} + coef_pol[2]*(x)+coef_pol[1]
z=outer(x, y, parabola)
persp(x,y,z)
persp(x,y,z,phi = 325,col = "salmon")

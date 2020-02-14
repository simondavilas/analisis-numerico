#Codigo Realizado por: Gabriel Gomez Corredor
#                      Simon Davila
#                      Juan Pablo Mendez

# PUNTO 6 DOCUMENTO 2

#Informacion: Para realizar el metodo de Newton necesitamos una funcionn y luego de
#             esto hallar la derivada de su funcion y vamos comparando si la aproximacion
#             es igual al valor de del punto se sale de la condicion si no se efectuan
#             la operacion y se sigue haciendo iterativamente

rm(list=ls())

expresion <- expression (exp(x) - x-1) # escribimos el polinomio
derivada <- D(expresion, "x") # Derivada del polinomio

x <- 0  # Cualquier valor diferente de aprox


aprox <- 0.000005 # valor punto inicial

while ( x != aprox) {
  
  x <- aprox # Se le asigna el valor aproximado a x.
  
  reemplazoexpresion <- eval(expresion) #Reemplaza el valor de x en "expresion"
  
  reemplazoderiv <- eval(derivada) #Reemplaza el valor de x en "derivada"
  
  #newton
  
  aprox <- x - (reemplazoexpresion/reemplazoderiv) #Ecuación método de Newton
  
  print(x)
}
#Grafica de la funcionn
expresiong <- function(x)(exp(x) - x-1)
curve(expresiong,-1,5,101, ylim = c(0,12))
abline(0,0,col="blue")
Horner <- function(Coef_Polinomio, x0) {
  # Creación de la función, 
  #recibe los coeficientes del polinomio 
  #y el punto a evaluar.
  TerminoInd <- Coef_Polinomio[1]
  Coef_Polinomio <- Coef_Polinomio[-1]
  #Guarda el término de independiente en una 
  #variable aparte de los coeficientes que 
  #acompañan algún valor de "x".
  P_x0  <- Coef_Polinomio[length(Coef_Polinomio)]
  P1_x0 <- Coef_Polinomio[length(Coef_Polinomio)]
  j <- length(Coef_Polinomio)-1
  #Guarda los coeficientes no independientes 
  #del polinomio en dos variables, 
  #y se inicializa la variable iterativa.
  while (j>0) {
    P_x0  <- x0*P_x0 + Coef_Polinomio[j]
    P1_x0 <- x0*P1_x0 + P_x0
    j <- j-1
  }
  #Se aplica el Método de Horner para 
  #calcular el valor del polinomio en x0 
  #y el valor de su derivada en x0.
  P_x0 <- (x0*P_x0)+TerminoInd
  #Se suma el término independiente al polinomio, 
  #pero no a la derivada, ya que en la 
  #derivada este término desaparece.
  cat(paste("P(", x0, ") =", P_x0,
            "\nP'(", x0, ") =", P1_x0))
}
#Se imprimen los resultados. 
#Siendo P_x0 el valor del polinomio en x0 y 
#P1_x0 el valor de la derivada en x0.




HornerC <- function(Coef_Polinomio, x0) {
  Coef_Polinomio  <- as.complex(Coef_Polinomio)
  #Los coeficientes pasan a ser tratados como números complejos.
  TerminoInd      <- Coef_Polinomio[1]
  Coef_Polinomio  <- Coef_Polinomio[-1]
  P_x0  <- Coef_Polinomio[length(Coef_Polinomio)]
  P1_x0 <- Coef_Polinomio[length(Coef_Polinomio)]
  j <- length(Coef_Polinomio)-1
  while (j>0) {
    P_x0  <- x0*P_x0 + Coef_Polinomio[j]
    P1_x0 <- x0*P1_x0 + P_x0
    j <- j-1
  }
  P_x0 <- (x0*P_x0)+TerminoInd
  cat(paste("P(", x0, ") =", P_x0,
            "\nP'(", x0, ") =", P1_x0))
}




install.packages("signal")
library(signal)
Operaciones <- 0
#Se instala un paquete para utilizar 
#la función "polyval" más adelante y se 
#inicia el número de operaciones PRINCIPALES en 0.

Seno_Taylor <- function(n){
  Taylor_Pol <- c()
  for(i in 0:n){
    Taylor_Pol <- c(Taylor_Pol,0)
    Taylor_Pol <- c(Taylor_Pol,((-1)^i)/factorial(2*i+1))
    Operaciones <- Operaciones + 1
  }
  return(Taylor_Pol)
}
#Esta función recibe un valor de "n" 
#y devuelve los coeficientes del polinomio de Taylor 
#de grado "n" de la función seno.

Norm <- function(vect1,vect2){
  error_relativo <- 0
  err <- 0
  for(i in 1:length(vect1)){
    error_relativo <- error_relativo + abs(vect1[i]-vect2[i])
    Operaciones <- Operaciones + 1
    err <- err + abs(vect1[i])
    Operaciones <- Operaciones + 1
  }
  return((error_relativo/err)-1)
}
#Se define una función que recibe imágenes de dos 
#funciones en los mismos puntos, y devuelve una forma 
#de medir el error que hay entre ellas. 
#En este caso se calcula este error como un ponderado 
#de los valores absolutos de las restas entre valores.

Taylor <- function(error_minimo){
  test_val <- c()
  for(i in seq(from=-pi/64,to=pi/64,by=0.001)){
    test_val <- c(i,test_val)
  }
  #Se generan varios valores en el intervalo de interés 
  #y se guardan en la variable "test_val".
  iteraciones <- 0
  n <- 0
  e_rel <- 1
  #Se inicializan varias variables. 
  #El número de iteraciones en 0, el grado del 
  #polinomio de Taylor en 0, y el error relativo en 1.
  cat("Iteracion =",iteraciones,"\n")
  #Se muestra la iteración en la que va el código.
  Aprox_Pol <- Seno_Taylor(n)
  Pol_Ant <- polyval(Aprox_Pol[length(Aprox_Pol):1],test_val)
  #Se inicializa el polinomio de aproximación en grado 0 
  #y se evaluan los datos de prueba en él.
  Operaciones <- Operaciones + 1
  while(e_rel>error_minimo){
    n <- n+1
    Operaciones <- Operaciones + 1
    Aprox_Pol <- Seno_Taylor(n)
    Pol_Act <- polyval(Aprox_Pol[length(Aprox_Pol):1],test_val)
    #La función polyval evalúa el vector de puntos en el 
    #polinomio, dados sus coeficientes en orden descendente.
    Operaciones <- Operaciones + 1
    e_rel <- Norm(Pol_Ant,Pol_Act)
    iteraciones <- iteraciones+1
    cat("Iteracion =",iteraciones,"\n")
    cat("Error Relativo =",e_rel,"\n")
    Pol_Ant <- Pol_Act
  }
  #Se lleva a cabo la aproximación de Taylor. 
  #A cada iteración se eleva el grado del polinomio de 
  #aproximación en 1, se evaluan los datos de prueba en él, 
  #y se compara con los valores anteriores bajo la norma 
  #definida arriba. Esto se repite hasta que el error sea 
  #menor al valor ingresado por el usuario.
  cat("Polinomio =",Aprox_Pol,"\n")
  cat("Operaciones =",Operaciones,"\n")
  #Se imprimen los coeficientes del polinomio 
  #que mejor aproxima la función en el intervalo 
  #dado y el número de operaciones principales 
  #que se realizaron.
}





install.packages("limSolve")
install.packages("signal")
library(signal)
library(limSolve)
#Se instalan dos paquetes, uno para poder utilizar la función
#"polyval", y el otro para utilizar la función "Solve" más adelante.

Norma_f <- function(p){
  inf <- 0
  ans <- c(0,0)
  for(i in seq(from=-pi/64,to=pi/64,by=0.001)){
    y = abs(sin(i)-polyval(p[length(p):1],i))
    Operaciones <- Operaciones + 1
    if(y<inf){
      inf <- y
      ans[1] <- inf
      ans[2] <- i
    }
  }
  return(ans)
}
#Esta función estima el valor de la norma que usa el
#método de Remez, devuelve el valor de la norma y
#el punto del intervalo en dónde da dicho valor.
#La función seno ya está predeterminada en el código.

Remez <- function(n,error_min){
  #El usuario debe ingresar el error_min, el cual es el epsilon
  #que aparece en el resúmen del método, y el "n" para la
  #cantidad de datos a tomar al comienzo.
  Iteraciones <- 0
  Operaciones <- 0
  Ptos_Muestra <- c()
  
  
  #USAR DATOS UNIFORMEMENTE ELEGIDOS:
  dx = (pi/32)/(n+1)
  for(x in seq(from=-pi/64,to=pi/64,by=dx)){
    Ptos_Muestra <- c(Ptos_Muestra,x)
  }
  #USAR DATOS ALEATORIAMENTE ELEGIDOS
  #Ptos_Muestra <- runif((n+2),min=-pi/64,max=pi/64)
  
  
  #Se toman "n+2" datos distribuidos uniformemente en el
  #intervalo de interés.
  while(TRUE){
    cat("Iteracion =",Iteraciones,"\n")
    A <- matrix(Ptos_Muestra,n+2,n+2)
    for(i in seq(from=1,to=n+2,by=1)){
      A[i,1] <- 1
      A[i,n+2] <- (-1)**i
    } 
    for(i in seq(from=1,to=n+2,by=1)){
      for(j in seq(from=1,to=n+2,by=1)){
        if(j!=1 && j!=(n+2)){
          A[i,j] <- A[i,j]^j
        }
      }
    } 
    #Se crea la matriz que codifica el sistema de
    #ecuaciones linealess que hay que resolver.
    b <- c()
    for(i in Ptos_Muestra){
      b <- rbind(b,sin(i))
    }
    #Se crea el vector que codifica el otro lado de la
    #igualdad del sistema de ecuaciones lineales; en este
    #caso la función "f" es seno.
    B <- Solve(A,b)
    #Se usa el comando "Solve" para hallar la solución "x"
    #al sistema de ecuaciones lineales Ax=b.
    Operaciones <- Operaciones + 1
    Pol <- B[0:n+1]
    e <- B[n+2]
    #Se guardan los coeficientes del polinomio y el error.
    norma <- Norma_f(Pol)
    #Se estima la norma entre el polinomio y el seno.
    cat("Error =",abs(norma[1]-abs(e)),"\n")
    if(abs((norma[1]-abs(e)))<=error_min){
      return(Pol)
      #Se revisa la condición de parada.
    }else{
      E = norma[2]
      for(j in 1:length(Ptos_Muestra)){
        if(E<=Ptos_Muestra[j]){
          Ptos_Muestra[j] <- E
          Iteraciones <- Iteraciones + 1
          break
          #Se agrega el punto (en donde la norma equivale a la
          #resta entre el seno y el polinomio) al conjunto de "n+2" 
          #valores y se retira el punto más cercano a éste.
        }
      }
    }
  }
}
#Codigo Realizado por: Gabriel Gomez Corredor
#                      Simon Davila
#                      Juan Pablo Mendez

# PUNTO 5 DOCUMENTO 1

#Informacion: Para evaluar el valor de un polinomio tomamos el 
#             metodo de biseccion, mas adelante el metodo de Newton 
#             tambien sirve para evaluar el valor de un polinomio

rm(list=ls())

mifuncion <- function(a,b,tol){
  
  error=0
  xa=a
  xb=b
  xpuntoMedio=(xa+xb)/2
  xanterior=0
  iteraciones=0
  bandera=FALSE
  vectorErrores<-(0)
  indice=0
  iteracionesMax=0
  variable1=log(abs((xa-xb)/tol))          
  variable2=(1/log(2)) 
  ErroresSig<-c(0)
  j=0
  
  iteracionesMax=variable1*variable2

  if(sign( exp(xa)-(pi*xa)) ==  sign( exp(xpuntoMedio)-(pi*xpuntoMedio))){
    
    bandera=TRUE
  }
  
  if(bandera==TRUE){
    
    cat("Son iguales los signos\n")
  }
  if(bandera==FALSE){
    
    if(exp(xa)-(pi*xa)  *  exp(xpuntoMedio)-(pi*xpuntoMedio) > 0){
      
      xanterior=xpuntoMedio
      xa=xpuntoMedio
      xpuntoMedio=(xpuntoMedio+xb)/2

      
    }else if(exp(xa)-(pi*xa)  *  exp(xpuntoMedio)-(pi*xpuntoMedio) < 0){
      
      xanterior=xpuntoMedio
      xb=xpuntoMedio
      xpuntoMedio=(xa+xpuntoMedio)/2
      
    }
    
    error=abs((xpuntoMedio+xanterior)/xpuntoMedio)
    
    vectorErrores[indice]=error
    

    while( error > tol && iteraciones < iteracionesMax){
      
      iteraciones=iteraciones+1
      indice=indice+1
      
      if( exp(xa)-(pi*xa) *  exp(xpuntoMedio)-(pi*xpuntoMedio) > 0){
        
        xa=xpuntoMedio
        xanterior=xpuntoMedio
        # cat("punto medio: ",xpuntoMedio,"\n")
        # cat("xa: ",xa,"\n")
        # cat("punto anterior: ",xanterior,"\n")
        xpuntoMedio=(xpuntoMedio+xb)/2
      }
      if( exp(xa)-(pi*xa)  *  exp(xpuntoMedio)-(pi*xpuntoMedio) < 0){
        
        xb=xpuntoMedio
        xanterior=xpuntoMedio
        xpuntoMedio=(xa+xpuntoMedio)/2
        
      }

      
      error=abs((xpuntoMedio+xanterior)/xpuntoMedio)
      vectorErrores[indice]=error
      ErroresSig[j]=error
      j=j+1
      
      
    }
    
    ErroresSig[j]=error
    
    # cat("Iteraciones: ",iteraciones,"\n")
    # cat("Respuesta: ",xpuntoMedio,"\n")
    # cat("Error: ",abs((xpuntoMedio+xanterior)/2),"\n")
    # cat(vectorErrores)
    itrs=seq(1,iteraciones)
    tabla=data.frame(itrs,vectorErrores)
    print(tabla)
    
    plot(x = vectorErrores, y = ErroresSig,type = 'o',main = "GRAFICA ERRORES",ylab = "Error i+1",xlab = "Error i", xlim=c(2,3),ylim=c(2,2.25),xaxp = c(2,3,10),col="orange")

  }

}

mifuncion(0,1.6,10e-7)


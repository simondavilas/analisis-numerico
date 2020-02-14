#Codigo Realizado por: Gabriel Gomez Corredor
#                      Simon Davila
#                      Juan Pablo Mendez

# PUNTO 2 DOCUMENTO 1


rm(list=ls())

mifuncion <- function(x,n,e){
  
  Xinvalido=FALSE
  valorNegativo=FALSE
  iteraciones=0
  errores<-c(0)
  contError=0
  erroresSig<-c(0)
  
  
  if(x==0){
    cat("el valor x (valor inicial) no puede ser 0")
    Xinvalido=TRUE
  }
  if(n<0){
    cat("el numero para la raiz tiene que ser un numero positivo")
    valorNegativo=TRUE
  }
  
  if(Xinvalido==FALSE && valorNegativo==FALSE){
    
    y=((0.5)*(x+(n/x)))
    
    while(abs(x-y)>e){
      
      if(contError > 0 ){
        
        erroresSig[iteraciones]=abs(x-y)
        
      }
      
      contError=contError+1
      
      iteraciones=iteraciones+1
      errores[iteraciones]=abs(x-y)
      x=y
      y=((0.5)*(x+(n/x)))
    }
    cat("resultado final: ",y," \nIteraciones totales: ",iteraciones,"\n")
    cat("Comprobar: ",y," X ",y," = ",y*y,"\n")
    #plot(errores,type = 'o',main = "GRAFICA ERRORES",ylab = "Error i+1",xlab = "Error i")
    erroresSig[iteraciones]=errores[iteraciones]
    
    plot(x = errores, y = erroresSig,type = 'o',main = "GRAFICA ERRORES",ylab = "Error i+1",xlab = "Error i",col="blue")
    
  }

}

mifuncion(3,7,10e-8)

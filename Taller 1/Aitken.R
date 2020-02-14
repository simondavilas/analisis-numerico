#Codigo Realizado por: Gabriel Gomez Corredor
#                      Simon Davila
#                      Juan Pablo Mendez

# PUNTO 7 DOCUMENTO 2

#Informacion: Para realizar el metodo de Aitken se tomaron
#             los valores generados por biseccion, con la funcion 
#             que el problema planteaba " cos(1/x) ", entonces
#             se tomo el intervalo [0.002 , 2] ya que con este intervalo
#             en la funcion da signos diferentes y se puede realizar 
#             biseccion normalmente para luego tomar Xi valores en Aitken



rm(list=ls())




mifuncion<-function(a,b,tol){
  
  
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
  
  if(sign( cos(1/xa)  ) ==  sign( cos(1/xpuntoMedio) ) ){
    
    bandera=TRUE
  }
  
  if(bandera==TRUE){
    
    cat("Son iguales los signos\n")
  }
  if(bandera==FALSE){
    
    if(cos(1/xa)   *  cos(1/xpuntoMedio)  > 0){
      
      xanterior=xpuntoMedio
      xa=xpuntoMedio
      xpuntoMedio=(xpuntoMedio+xb)/2
      
      
    }else if(cos(1/xa)   *  cos(1/xpuntoMedio)  < 0){
      
      xanterior=xpuntoMedio
      xb=xpuntoMedio
      xpuntoMedio=(xa+xpuntoMedio)/2
      
    }
    
    error=abs((xpuntoMedio+xanterior)/xpuntoMedio)
    
    vectorErrores[indice]=error
    
    
    while( error > tol && iteraciones < iteracionesMax){
      
      iteraciones=iteraciones+1
      indice=indice+1
      
      if( cos(1/xa)  *  cos(1/xpuntoMedio)  > 0){
        
        xa=xpuntoMedio
        xanterior=xpuntoMedio
        # cat("punto medio: ",xpuntoMedio,"\n")
        # cat("xa: ",xa,"\n")
        # cat("punto anterior: ",xanterior,"\n")
        xpuntoMedio=(xpuntoMedio+xb)/2
      }
      if( cos(1/xa)  *  cos(1/xpuntoMedio)  < 0){
        
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
    
    itrs=seq(1,iteraciones)
    tabla=data.frame(itrs,vectorErrores)
    print(tabla)
    
    plot(x = vectorErrores, y = ErroresSig,type = 'o',main = "GRAFICA ERRORES",ylab = "Error i+1",xlab = "Error i", xlim=c(2,2.2),ylim=c(2,2.1),xaxp = c(2,3,10),col="red")
    
    cat("\n")
    cat("\n")
    cat("\n")
    cat("\n")
    
  }
  
  #Formula de Aitken
  
  valoresAitken<-c(0)
  j=1
  indice=1
  longitud=length(vectorErrores)-3
  iteraciones=1
  
  valor = vectorErrores[indice+2] - ((vectorErrores[indice+2] - vectorErrores[indice+1]) * (vectorErrores[indice+2] - vectorErrores[indice+1])/ (vectorErrores[indice+2] - (2*vectorErrores[indice+1]) + vectorErrores[+1] ))
  
  valoresAitken[j]=valor
  j=j+1
  indice=indice+1
  
  #cat(valor,"\n")
  #cat(longitud,"\n")
  
  while(longitud > 0 ){
    iteraciones=iteraciones+1
    #cat(longitud,"\n")
    #cat("entro","\n")
    valor = vectorErrores[indice+2] - ((vectorErrores[indice+2] - vectorErrores[indice+1]) * (vectorErrores[indice+2] - vectorErrores[indice+1])/ (vectorErrores[indice+2] - (2*vectorErrores[indice+1]) + vectorErrores[+1] ))
    valoresAitken[j]=valor
    j=j+1
    indice=indice+1
    longitud=longitud-1
  }
  
  itrs=seq(1,iteraciones)
  tabla=data.frame(itrs,valoresAitken)
  print(tabla)

}


mifuncion(0.002,2,10e-7)

#Codigo Realizado por: Gabriel Gomez Corredor
#                      Simon Davila
#                      Juan Pablo Mendez

# PUNTO 3 DOCUMENTO 1

#Informacion: En cada iteracion se tenia que multiplicar el factor ( b-a ) 
#             en donde nuestra a es 0.5 porque el problema lo pedia pero
#             se necesitaba otro valor b, nosotros tomamos 1, de esta manera
#             se evalua en el intervalo [a,b] (parametros)

rm(list=ls())

mifuncion<-function(a,b){
  
  iteraciones=0
  contFactorial=0
  respuesta=0
  cont=0
  errores<-c(0)
  xanterior=0
  indice=0
  
  while(iteraciones < 5){
    
    iteraciones=iteraciones+1
    

    
    if(contFactorial==0){
      
      respuesta=exp(a)
      contFactorial=contFactorial+1
      errores[indice]=abs(respuesta-xanterior)
      indice=indice+1
      xanterior=respuesta
      
    }else{
      
      respuesta=respuesta+((exp(a)/factorial(contFactorial))*(b-a)^contFactorial)
      contFactorial=contFactorial+1
      errores[indice]=abs(respuesta-xanterior)
      indice=indice+1
      xanterior=respuesta
      
    }

    
  }
  
  errores[indice]=0
  
  numero=as.character(respuesta)
  cont=nchar(numero)
  CifraSignificaticas=0
  
  cat("\n")
  cat("\n")
  
  cat("Respuesta: ")
  
  while(CifraSignificaticas < 7){
    
    CifraSignificaticas=CifraSignificaticas+1
    
    cat(substr(numero,nchar(numero)-cont,nchar(numero)-cont))
    
    cont=cont-1
  }
  
  cat("\n")
  cat("\n")
  itrs=seq(1,iteraciones)
  tabla=data.frame(itrs,errores)
  print(tabla)
  

}

mifuncion(0.5,1)


#rm(list=ls())

mifuncion<-function(xo,tolerancia){
  
  
  iteraciones=0
  error=0
  xanterior=0
  aproximaciones<-c(0)
  indice=0
  aproxFinal=0
  
  respuesta=exp(xo)/pi
  
  aproximaciones[indice]=xanterior
  indice=indice+1
  
  xanterior=respuesta
  
  respuesta=exp(respuesta)/pi
  
  aproximaciones[indice]=respuesta
  
  indice=indice+1
  

  
  while(   abs((respuesta-xanterior)/respuesta)   > tolerancia ){
    
    
    iteraciones=iteraciones+1
    
    xanterior=respuesta
    
    respuesta=exp(respuesta)/pi
    
    aproximaciones[indice]=respuesta
    
    indice=indice+1
    
    
  }
  
  aproxFinal=exp(aproximaciones[indice-1])-(pi*(aproximaciones[indice-1]))
  
  
  cat("Valor final para reemplazar en funcion que tiende a 0: ",aproximaciones[indice-1],"\n")
  
  cat("Aproximacion Final: ",aproxFinal)
  
  
}


mifuncion(1,10e-6)

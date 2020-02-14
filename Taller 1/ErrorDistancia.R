#Codigo Realizado por: Gabriel Gomez Corredor
#                      Simon Davila
#                      Juan Pablo Mendez

# PUNTO 4 DOCUMENTO 1

rm(list=ls())

mifuncion<-function(v,ev,t,et){
  
  distancia=v*t
  
  ErrorAbsoluto=v*ev + t*et
  
  ErrorRelativo=((ev/v)+(et/t) )*100
  
  cat("Distancia: ",distancia-ErrorAbsoluto," < d < ",distancia+ErrorAbsoluto,"\n")
  cat("Error Relativo: ",ErrorRelativo ,"%")
  
  
}

mifuncion(4,0.1,5,0.1)

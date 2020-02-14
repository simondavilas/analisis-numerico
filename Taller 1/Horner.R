#Codigo Realizado por: Gabriel Gomez Corredor
#                      Simon Davila
#                      Juan Pablo Mendez

# PUNTO 1 DOCUMENTO 2

#Informacion: Para realizar el metodo de horner pedimos que el usuario nos de los 
#             coeficientes de la funcion y un valor x0 para poder calcular por el 
#             metodo de horner 


rm(list=ls())

mifuncion = function(x0,coeficientes){
 
  resultado = 0
  
  for(i in 1:length(coeficientes))
  {
    resultado= resultado * x0 + coeficientes[i] 
  }
  
  resultado2 = (x0^51-1)/(x0-1)
  
  cat(resultado)
  cat("\n")
  cat(resultado2)
  cat("\n")
  cat(resultado-resultado2)
  
   
}

mifuncion(1.0001 ,c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
                    ,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))

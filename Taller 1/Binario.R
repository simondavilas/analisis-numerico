#Codigo Realizado por: Gabriel Gomez Corredor
#                      Simon Davila
#                      Juan Pablo Mendez

# PUNTO 2 DOCUMENTO 2

#Informacion: La funcion tiene 2 parametros, numero: el numero ya sea en 
#             decimal o binario y opcion: toma el valor 1/0.  1: para convertir 
#             de binario a decimal y 0: para convertir de decimal a binario




mifuncion<-function(numero,opcion){
  
  if(opcion == 0){
    
    
    #Parte entera del binario
    
    
    bandera=FALSE
    original=numero
    tamVector=0
    vectorBinario<-c(0)
    
    
    entero=floor(numero)
    
    while(entero >= 1){
      
      tamVector=tamVector+1
      entero=entero/2
      entero=floor(entero)
    }
    
    entero=floor(numero)
    indice=tamVector
    
    
    
    while(entero >= 1){
      
      vectorBinario[indice]=entero%%2
      entero=entero/2
      entero=floor(entero)
      indice=indice-1
    }
    
    
    
    numero=floor(numero)
    
    numero=original-numero
    
    if(numero > 0){
      
      indice=tamVector+1
      
      vectorBinario[indice]=','
      
      indice=indice+1
      
      digito=floor(numero*2)
      
      
      while(  digito <= 1  ){
        
        
        vectorBinario[indice]= digito
        numero=numero*2
        digito=floor(numero*2)
        indice=indice+1
      }
      
    }
    
    cat("Numero decimal ingresado: ",original,"\n")
    cat("Numero binario generado: ",vectorBinario,"\n")
    
    
  }
  if(opcion == 1){
    
    bandera=FALSE
    respuesta=0
    original=numero
    longitud=nchar(numero)
    
    entero=floor(numero)
    expo=nchar(entero)
    
    if((original-entero) == 0){
      bandera=TRUE
    }
    
    
    while(longitud>=0){
      
      digito=substr(numero,nchar(numero)-longitud,nchar(numero)-longitud)
      
      
      if(bandera==FALSE){
        
        
        if(longitud==0){
          
          expo=expo+1
          
          digito=as.numeric(digito)
          respuesta=respuesta+ ( digito* (2^expo))
          break
          
          
        }
        
      }

      
      if(digito==1){
        digito=as.numeric(digito)
        
        respuesta=respuesta+ ( digito* (2^expo))

      }
      
      
      longitud=longitud-1
      expo=expo-1
      
    }
    
    cat("Numero decimal generado: ",respuesta,"\n")
    
    
  }
  
}

mifuncion(1010101,1)


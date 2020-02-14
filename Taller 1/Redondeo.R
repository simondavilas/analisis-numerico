#Codigo Realizado por: Gabriel Gomez Corredor
#                      Simon Davila
#                      Juan Pablo Mendez

# PUNTO 1 DOCUMENTO 1

#Elimina valores almacenados en memoria
rm(list=ls())

mifuncion = function(numero){
  
  ValorReal=numero
  vector<-c(0)
  indice=0
  as.character(numero)
  cont=nchar(numero)
  evaluar=FALSE
  contSig=nchar(numero)-1
  numeroActual=0
  numeroSig=0
  decimales<-c(0)
  
  while( cont >= 0){
    
    digito=substr(numero,nchar(numero)-cont,nchar(numero)-cont)
    
    
    if(evaluar == FALSE){
      vector[indice]=digito
    }
    
    if(evaluar == TRUE){
      
      contPunto=contPunto+1
      
      if(contPunto <= 4){
        
        if(contPunto == 4){
          
          numeroActual=as.numeric(substr(numero,nchar(numero)-cont,nchar(numero)-cont))
          numeroSig=as.numeric(substr(numero,nchar(numero)-contSig,nchar(numero)-contSig))
          
          if(numeroSig > 4){
            
            numeroActual=numeroActual+1
          }
          
          vector[indice]=numeroActual
          decimales[j]=numeroActual
          j=j+1
          break
        }
          
          if(contSig==0){
            
            numeroActual=as.numeric(substr(numero,nchar(numero)-cont,nchar(numero)-cont))
            numeroSig=as.numeric(substr(numero,nchar(numero)-contSig,nchar(numero)-contSig))
            
            
            if(numeroSig > 4){
              
              numeroActual=numeroActual+1
            }
          
            vector[indice]=numeroActual
            decimales[j]=numeroActual
            j=j+1
          }else if(contSig != -1){
            vector[indice]=numeroActual=as.numeric(substr(numero,nchar(numero)-cont,nchar(numero)-cont))
            decimales[j]=numeroActual=as.numeric(substr(numero,nchar(numero)-cont,nchar(numero)-cont))
            j=j+1
          }
      }

    }
    
    if(digito == "."){
      
      evaluar=TRUE
      contPunto=0
      j=0
      decimales[0]=as.numeric(substr(numero,nchar(numero)-contSig,nchar(numero)-contSig))
    }
    
    cont=cont-1
    contSig=contSig-1
    indice=indice+1
    
    
  }
  
  cat("Numero redondeado: ",vector,"\n")
  cat("Valor Real: ",ValorReal)
  
}

mifuncion(536.78)

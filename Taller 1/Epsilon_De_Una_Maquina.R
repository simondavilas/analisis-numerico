#Codigo Realizado por: Gabriel Gomez Corredor
#                      Simon Davila
#                      Juan Pablo Mendez

#PUNTO 3 DOCUMENTO 2


# 1. ¿ Cómo se ajusta un número binario infinito en un número finito de bits?
# R: Por la norma IEEE P754, se define el formato que se debe respetar para la 
#    representación de un número binario con signo en punto flotante. Este se 
#    puede expresar como punto flotante de simple precisión, punto flotante de 
#    doble precisión, punto flotante de precisión extendida y punto flotante de
#    formato BCD empaquetado. La variación de los formatos corresponde a que
#    existen casos en los cuales al realizar una operación con números de un 
#    rango simlar, el resultado se puede expresar en el mismo formato sin errores
#    apreciables. El formato de precisión simple y precisión doble cuentan 
#    con un signo (representa si el número es positivo o negativo), exponente
#    (representa un número relacionado con el exponente) y fracción del 
#    sginificando (la mantisa del número)

# 2. ¿Cuál es la diferencia entre redondeo y recorte?
# R: Al redondear se escoge el número más cercano que tenga la cantidad de 
#    dígitos significativos escogida, mientras que al realizar el recorte no se 
#    realiza ninguna aproximación, únicamente se dejan los números tal como 
#    estaban acorde al número de dígitos escogidos.

#include <cmath>

  fl<-function(numero)
  {
      #Parte entera del binario
      
      bandera=FALSE
      original=numero
      tamVector=0
      vectorBinario<-c(0)
      vectorSigni<-c(0)
      cat("Número decimal ingresado: ",original,"\n")
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
      if(original>0)
      {
        cat("S: ",0,"\n")
      }
      else if(orginal < 0)
      {
        cat("S: ",0,"\n")
      }
      
      if(numero > 0){
        
        indice=tamVector+1
        if(original < 1)
        vectorBinario[indice]='0 .'
        else{
          vectorBinario[indice]='.'
        }
        
        indice=indice+1
        
        digito=floor(numero*2)
        
        aux=0
        indiceAux=0
        while(  aux <= 52  ){
          
          numero=numero*2
          
          if(numero>=1)
          {
            digito=1
            numero=numero-1
          }
          else{
            digito = 0
          }
          vectorBinario[indice]= digito
          indice=indice+1
          aux=aux+1
        }
        
      }
      aux2=0
      auxI=1
      auxF=0
      while(aux2<1)
      {
        if(vectorBinario[auxI]==1)
        {
          aux2=1
          auxF=auxI-1
        }
        else
        {
          auxI=auxI+1
        }
      }
      format=127+auxF
      
      
      bandera=FALSE
      originalB=format
      tamVectorB=0
      vectorBinarioB<-c(0)
      
      
      enteroB=floor(format)
      
      while(enteroB >= 1){
        
        tamVectorB=tamVectorB+1
        enteroB=enteroB/2
        enteroB=floor(enteroB)
      }
      
      enteroB=floor(format)
      indiceB=tamVectorB
      
      
      
      while(enteroB >= 1){
        
        vectorBinarioB[indiceB]=enteroB%%2
        enteroB=enteroB/2
        enteroB=floor(enteroB)
        indiceB=indiceB-1
      }
      
      
      
      format=floor(format)
      
      format=originalB-format
      
      if(format > 0){
        
        indiceB=tamVectorB+1
        
        vectorBinarioB[indiceB]=','
        
        indiceB=indiceB+1
        
        digitoB=floor(format*2)
      }
      
      cat("exponente: ",vectorBinarioB,"\n")
      cat("fracción del significado: ",vectorBinario[auxF+2:52],"\n")
      
      cat("Número binario generado: ",vectorBinario,"\n")
      
  }
  fl(0.4)
  
  
  
# 6. Verificar si el tipo de datos básico de R y Python es de precisión doble IEEE 
#    y Revisar en R y Phython el format long
# R: Todos los números reales en R utilizan el tipo de dato numeric en el que son
#    guardados en el formato de precisión doble IEEE. Por su parte en Python los números
#    reales utilizan el tipo de dato float en el que son guardados en el formato de 
#    precisión doble IEEE.
#    En R el tipo de dato long no se maneja, por lo que para representar números grandes
#    se utiliza la ntoación de punto flotante. Mientras que en Python el tipo de dato long
#    si es utilizado para representar números reales bastante grandes.
  
  
  verificar<-4503599627370496
  verificar
  
  
  realToHex<-function(x)
  {
    decimal=floor(x)
    vectorHexa<-c(0)
    indice=1
    if(decimal>0 && decimal<16)
    {
      if(decimal==1)
      {
        vectorHexa[indice]=decimal
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==2)
      {
        vectorHexa[indice]=decimal
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==3)
      {
        vectorHexa[indice]=decimal
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==4)
      {
        vectorHexa[indice]=decimal
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==5)
      {
        vectorHexa[indice]=decimal
        indice=indice+1
        vectorHexa[indice]='.'
        
      }
      if(decimal==6)
      {
        vectorHexa[indice]=decimal
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==7)
      {
        vectorHexa[indice]=decimal
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==8)
      {
        vectorHexa[indice]=decimal
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==9)
      {
        vectorHexa[indice]=decimal
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==10)
      {
        vectorHexa[indice]='A'
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==11)
      {
        vectorHexa[indice]='B'
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==12)
      {
        vectorHexa[indice]='C'
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==13)
      {
        vectorHexa[indice]='D'
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==14)
      {
        vectorHexa[indice]='E'
        indice=indice+1
        vectorHexa[indice]='.'
      }
      if(decimal==15)
      {
        vectorHexa[indice]='F'
        indice=indice+1
        vectorHexa[indice]='.'
      }
    }
    aux=0
    num=x-decimal
    indice=indice+1
    while(aux<15)
    {
      num=num*16
      cat("Número de máquina hexadecimal generado: ",num,"\n")
      auxNum=floor(num)
      num=num-auxNum
      
      if(auxNum==0)
      {
        vectorHexa[indice]=auxNum
      }
      if(auxNum==1)
      {
        vectorHexa[indice]=auxNum
      }
      if(auxNum==2)
      {
        vectorHexa[indice]=auxNum
      }
      if(auxNum==3)
      {
        vectorHexa[indice]=auxNum
      }
      if(auxNum==4)
      {
        vectorHexa[indice]=auxNum
      }
      if(auxNum==5)
      {
        vectorHexa[indice]=auxNum
      }
      if(auxNum==6)
      {
        vectorHexa[indice]=auxNum
      }
      if(auxNum==7)
      {
        vectorHexa[indice]=auxNum
      }
      if(auxNum==8)
      {
        vectorHexa[indice]=auxNum
      }
      if(auxNum==9)
      {
        vectorHexa[indice]=auxNum
      }
      if(auxNum==10)
      {
        vectorHexa[indice]='A'
      }
      if(decimal==11)
      {
        vectorHexa[indice]='B'
      }
      if(decimal==12)
      {
        vectorHexa[indice]='C'
      }
      if(decimal==13)
      {
        vectorHexa[indice]='D'
      }
      if(decimal==14)
      {
        vectorHexa[indice]='E'
      }
      if(decimal==15)
      {
        vectorHexa[indice]='F'
      }
      
      cat("auxNum: ",auxNum,"\n")
      cat("num2: ", num,"\n")
      indice=indice+1
      aux=aux+1
      
    }
    cat("Número de máquina hexadecimal generado: ",vectorHexa,"\n")
    
  }
  realToHex(9.4)
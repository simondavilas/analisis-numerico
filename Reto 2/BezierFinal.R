library(pracma)

rm(list=ls())

#Los puntos fueron sacados del programa Tracker, ubicando un mortero de ejemplo 
#               y estableciendo unas coordenadas en el como punto de referencia
x<-c(0,20.77,37.92,55.08,73.14,88.49,113.8,130.9,148.1,156.2,162.5,167.9,173.4,176.1,177,161.6,142.7,124.6,102.9,80.36,49.66,22.57,0,-26.18,-55.98,-78.55,-100.2,-128.2,-143.6,-159.8,-171.6,-176.1,-176.1,-173.4,-170.7,-164.3,-156.2,-144.5,-132.7,-113.8,-95.71,-78.55,-55.8,-28.89,0,0,23.48,56.88,83.97,118.3,140.9,158,167,176.1,-25.28,-49.66,-75.85,-98.42,-116.5,-137.2,-154.4,-170.7,-175.2,0,21.67,46.95,70.43,92.10,114.7,127.3,136.3,128.2,111.1,87.58,60.50,31.60,0,-24.38,-53.27,-77.65,-94.81,-114.7,-127.3,-136.3,-136.3,-120.1,-99.32,-80.36,-56.88,-35.21)
y<-c(0,3.612,5.418,9.932,15.35,22.57,36.12,50.56,69.53,85.78,102,126.4,146.3,179.7,209.5,237.5,252.2,270.9,279.9,288.9,294.4,299.8,300,298,293.5,287.1,281.7,271.8,260,246.5,230.2,205.9,178.8,146.3,128.2,108.4,88.49,70.43,55.98,37.02,22.57,16.25,9.029,4.515,0,120.1,117.4,118.3,127.3,139.1,152.6,164.3,179.7,200,116.5,121.9,127.3,133.6,144.5,156.2,170.7,186,199.5,140.9,141.8,144.5,148.1,155.3,170.7,180.6,200,227.5,238.4,253.7,265.5,273.6,275.4,141.8,146.3,152.6,160.7,173.4,186,200,223,239.3,251,260,266.4,269.1)
plot(x,y,xlim=c(-300,300),ylim =c(0,300),xlab = "x", ylab = "y",main = "Mortero puntos Tracker")

print(length(x))   # para graficar el mortero sacamos un total de 90 puntos


curva_bezier<- function(cord_x, cord_y, n)
{
  x1 = NULL                                               #se usa NULL para asegurarse que la lista no tenga ningun valor antes de inciar
  y1 = NULL
  cont= 1

  
  for (t in seq(0, 1, length.out = n))                     #el for se realiza en una secuencia de 0-1 en n particiones dependiendo del tamano del vector 
  {
    pivote = polinomios_Bernstein(cord_x,cord_y, t)        # se hace el llamado a la funcion bezier 
    x1[cont] = pivote[1]                                   # se accede al elemento x de la lista retornada
    y1[cont] = pivote[2]                                   # se accede al elemento y de la lista retornada
    
    cont=cont+1
  }
  
  return (list(x=x1, y=y1))
}
polinomios_Bernstein <- function(cord_x, cord_y, t)
{
  x1 = 0
  y1 = 0
  n = length(cord_x)-1                                  #se le resta 1 al tamaño del vector de coordenadas x
                                                        # porque los polinomios de Bernstein demuestran que 
                                                        #una sucesión de puntos converge uniformemente a X ([X,Y])
  for (i in 0:n)
  {
    x1 = x1 + choose(n, i)*((1-t)^(n-i))*t^i*cord_x[i+1]     # Se utiliza la formula de combinatoria para generar la curva
    y1 = y1 + choose(n, i)*((1-t)^(n-i))*t^i*cord_y[i+1]     # de Bezier tomando como referencia los vertices del poligono
  }                                                     # de control
  
  return (list(x=x1, y=y1))
}


#Graficar el Mortero con curvas de Bezier

#Curva externa derecha
x<-c(0,20.77,37.92,55.08,73.14,88.49,113.8,130.9,148.1,156.2,162.5,167.9,173.4,176.1,177,161.6,142.7,124.6,102.9,80.36,49.66,22.57,0)
y<-c(0,3.612,5.418,9.932,15.35,22.57,36.12,50.56,69.53,85.78,102,126.4,146.3,179.7,209.5,237.5,252.2,270.9,279.9,288.9,294.4,299.8,300)
plot('','',xlim=c(-300,300),ylim =c(0,300),main = "Curvas de Bezier",xlab = "x", ylab = "y")
points(curva_bezier(x,y,length(x)), type="l", col="salmon")           #se utliza la funcion points para graficar la secuencia de puntos
par (new = TRUE)                                                      #que devuelve la funcion curvas_bezier

#Curva externa izquierda
x<-c(-26.18,-55.98,-78.55,-100.2,-128.2,-143.6,-159.8,-171.6,-176.1,-176.1,-173.4,-170.7,-164.3,-156.2,-144.5,-132.7,-113.8,-95.71,-78.55,-55,-28.89,0)
y<-c(298,293.5,287.1,281.7,271.8,260,246.5,230.2,205.9,178.8,146.3,128.2,108.4,88.49,70.43,55.98,37.02,22.57,16.25,9.029,4.515,0)
plot('','',xlim=c(-300,300),ylim =c(0,300),main = "Curvas de Bezier",xlab = "x", ylab = "y")
points(curva_bezier(x,y,length(x)), type="l", col="salmon")
par (new = TRUE)

#Un cuarto de circulo exterior derecho
x<-c(0,23.48,56.88,83.97,118.3,140.9,158,167,176.1)
y<-c(120.1,117.4,118.3,127.3,139.1,152.6,164.3,179.7,200)
plot('','',xlim=c(-300,300),ylim =c(0,300),main = "Curvas de Bezier",xlab = "x", ylab = "y")
points(curva_bezier(x,y,length(x)), type="l", col="green")
par (new = TRUE)

#Un cuarto de circulo exterior izquierdo
x<-c(-25.28,-49.66,-75.85,-98.42,-116.5,-137.2,-154.4,-170.7,-175.2)
y<-c(116.5,121.9,127.3,133.6,144.5,156.2,170.7,186,199.5)
plot('','',xlim=c(-300,300),ylim =c(0,300),main = "Curvas de Bezier",xlab = "x", ylab = "y")
points(curva_bezier(x,y,length(x)), type="l", col="green")
par (new = TRUE)

#Semicirculo interno Derecho
x<-c(0,21.67,46.95,70.43,92.10,114.7,127.3,136.3,128.2,111.1,87.58,60.50,31.60,0)
y<-c(140.9,141.8,144.5,148.1,155.3,170.7,180.6,200,227.5,238.4,253.7,265.5,273.6,275.4)
plot('','',xlim=c(-300,300),ylim =c(0,300),main = "Curvas de Bezier",xlab = "x", ylab = "y")
points(curva_bezier(x,y,length(x)), type="l", col="blue")
par (new = TRUE)

#Semicirculo interior Izquierdo
x<-c(-24.38,-53.27,-77.65,-94.81,-114.7,-127.3,-136.3,-136.3,-120.1,-99.32-80.36,-56.88,-35.21)
y<-c(141.8,146.3,152.6,160.7,173.4,186,200,223,239.3,251,260,266.4,269.1)
plot('','',xlim=c(-300,300),ylim =c(0,300),main = "Curvas de Bezier",xlab = "x", ylab = "y")
points(curva_bezier(x,y,length(x)), type="l", col="blue")
par (new = TRUE)


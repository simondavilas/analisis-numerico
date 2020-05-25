
# PROYECTO TERCER CORTE ANALISIS NUMERICO
#INTEGRANTES: GABRIEL GOMEZ CORREDOR 
#             JUAN PABLO MENDEZ PERDOMO
#             SIMON DAVILA SARAVIA


library(shiny)

ui <- fluidPage(
  
  titlePanel("Prueba modelo SIR Y SIS"),
  titlePanel("Parametros y Datos Para SIR"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Modelo",
                  label = "Seleccionar modelo epidiomologico",
                  choices = list("SIR","SIS"),
                  selected = "SIR"),
      selectInput("MetodoNumerico",
                  label = "Seleccionar metodo numerico",
                  choices = list("rk4","adams","euler"),
                  selected = "rk4"),
      sliderInput("beta",
                  "Valor beta (tasa de infeccion):",
                  min = 0.100,
                  max = 0.500,
                  value = 0.1),
      sliderInput("gamma",
                  "Valor gamma (tasa de recuperacion):",
                  min = 0.04,
                  max = 0.15,
                  value = 0.09),
      sliderInput("Suceptibles",
                  "Valor de total de suceptibles: ",
                  min = 1,
                  max = 5,
                  value = 1),
      sliderInput("Infectados",
                  "Valor de total de infectados: ",
                  min = 1,
                  max = 5,
                  value = 3),
      sliderInput("Recuperados",
                  "Valor de total de recuperados: ",
                  min = 0,
                  max = 5,
                  value = 0),
      sliderInput("Dias",
                  "Dias evaluados",
                  min = 1,
                  max = 134,
                  value = 100),
      sliderInput("ValParticion",
                  "Escala de dias:",
                  min = 0.1,
                  max = 10,
                  value = 1),
      selectInput("datosAMostrar",
                  "Seleccione la poblacion de la que quiere ver los datos en la tabla",
                  choices = list("Suceptibles",
                                 "Infectados",
                                 "Recuperados"),
                  selected = "Suceptibles"),
      titlePanel("Parametros Y Datos Para SIS"),
      
      sliderInput("beta_sis",
                  "Valor beta (tasa de infeccion):",
                  min = 0.100,
                  max = 0.500,
                  value = 0.1),
      sliderInput("gamma_sis",
                  "Valor gamma (tasa de recuperacion):",
                  min = 0.04,
                  max = 0.15,
                  value = 0.09),
      sliderInput("Suceptibles_sis",
                  "Valor de total de suceptibles: ",
                  min = 1,
                  max = 5,
                  value = 1),
      sliderInput("Infectados_sis",
                  "Valor de total de infectados: ",
                  min = 1,
                  max = 5,
                  value = 3),
      sliderInput("Dias_sis",
                  "Dias evaluados",
                  min = 1,
                  max = 134,
                  value = 100),
      sliderInput("ValParticion_sis",
                  "Escala de dias:",
                  min = 0.1,
                  max = 10,
                  value = 1),
      selectInput("datosAMostrar_sis",
                  "Seleccione la poblacion de la que quiere ver los datos en la tabla",
                  choices = list("Suceptibles",
                                 "Infectados"
                  ),
                  selected = "Suceptibles")
    ),
    
    
    mainPanel(plotOutput("distPlot"),dataTableOutput("Table"), plotOutput("Error"),dataTableOutput("TablaError"),dataTableOutput("TablaErrorBD")
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    
    if( input$Modelo == "SIR" ){
      
      #Librerias para el correcto funcionamiento de la solucion
      library(deSolve)
      library(EpiDynamics)
      
      #Modelo SIR tomado de rpubs https://rpubs.com/dsfernandez/422937 
      
      SIR <- function(t, x, parametros){
        with(as.list(c(parametros, x)),{
          dS <- -(beta*S*I)
          dI <- +(beta*S*I)- gamma*I
          dR <- gamma*I
          
          derivadas <- c(dS, dI, dR)
          return(list(derivadas))
        })
      }
      
      #Parametros Beta y Gamma para la funcion SIR
      parametros <- c(beta= input$beta, gamma = input$gamma)
      #Valores iniciales: suceptibles, infectados y recuperados (pueden variar dentro de la interfaz)
      v_iniciales <- c(S=input$Suceptibles, I=input$Infectados, R=input$Recuperados)
      #La variacion del tiempo entre total de dias y la frecuencia de toma de datos(particiones)
      dt <- seq(0, input$Dias, input$ValParticion)
      #Se utiliza la funcion ode del paquete deSolve para solucionar el sistema pasandole como funcion la SIR de Rpubs (solucion experimental)
      sol = ode(y=v_iniciales, times=dt, func=SIR,parms=parametros, method = input$MetodoNumerico)
      #Usamos la funcion SIR del paquete EpiDynamics para tener una solucion teorica
      result = EpiDynamics::SIR(pars = parametros, init = v_iniciales, time = dt)
      
      simulacion.si <- as.data.frame(sol)
      
      attach(simulacion.si)
      
      #Las primeras tres graficas (suceptibles,Infectados,Recuperados) se muestran en una misma grafica con las soluciones que arroja EpiDynamics
      
      N <- sum(v_iniciales)
      plot(dt, S, type="l", col="blue", ylim=c(0,sum(v_iniciales)), xlab = "Tiempo (Dias)", ylab="Numero de individuos (en miles)")
      lines(dt, I, type="l", col="red")
      lines(dt, R, type="l", col="green")
      lines(dt, result$results[,2], type = "l", col = "brown")
      lines(dt, result$results[,3], type = "l", col = "orange")
      lines(dt, result$results[,4], type = "l", col = "purple")
      title("Modelo SIR")
      legend((input$Dias)/2, N + 0.25, 
             legend=c("Suceptibles por ODE", "Infectados por ODE", "Recuperados por ODE", "Suceptibles por EpiDynamics", "Infectados por EpiDynamics", "Recuperados por por EpiDynamics"),
             col=c("blue", "red", "green","brown", "orange", "purple"), lty=rep(1, 2))
      
      
      # TABLA DE DATOS 
      
      
      output$Table <- renderDataTable({
        
        library(deSolve)
        
        #Modelo SIR tomado de rpubs
        
        SIR <- function(t, x, parametros){
          with(as.list(c(parametros, x)),{
            dS <- -(beta*S*I)/(S+I+R)
            dI <- +((beta*S*I)/(S+I+R))- gamma*I
            dR <- gamma*I
            
            derivadas <- c(dS, dI, dR)
            return(list(derivadas))
          })
        }
        
        parametros <- c(beta= input$beta, gamma = input$gamma)
        v_iniciales <- c(S=input$Suceptibles, I=input$Infectados, R = input$Recuperados)
        dt <- seq(0, input$Dias, input$ValParticion)
        sol = ode(y=v_iniciales, times=dt, func=SIR,parms=parametros, method = input$MetodoNumerico)
        
        N <- (S+I+R)
        
        if( input$datosAMostrar == "Suceptibles" ){
          tabla = cbind(dt, round((sol[,2]/N)*100,5))             
          colnames(tabla) = c("Tiempo", "Suceptibles (% de poblacion)")
          tabla
        }else if(input$datosAMostrar == "Infectados"){
          tabla = cbind(dt, round((sol[,3]/N)*100,5))
          colnames(tabla) = c("Tiempo", "Infectados (% de poblacion)")
          tabla
        }else if(input$datosAMostrar == "Recuperados"){
          tabla = cbind(dt, round((sol[,4]/N)*100,5))
          colnames(tabla) = c("Tiempo", "Recuperados (% de poblacion)")
          tabla
        }
        
      })
      
      # GRAFICAR ERRORES 
      
      output$Error <- renderPlot({
        
        library(deSolve)
        library(EpiDynamics)
        
        SIR <- function(t, x, parametros){
          with(as.list(c(parametros, x)),{
            dS <- -(beta*S*I)
            dI <- +(beta*S*I)- gamma*I
            dR <- gamma*I
            
            derivadas <- c(dS, dI, dR)
            return(list(derivadas))
          })
        }
        
        parametros <- c(beta= input$beta, gamma = input$gamma)
        v_iniciales <- c(S=input$Suceptibles, I=input$Infectados, R = input$Recuperados)
        dt <- seq(0, input$Dias, input$ValParticion)
        sol = ode(y=v_iniciales, times=dt, func=SIR,parms=parametros, method = input$MetodoNumerico)
        result = EpiDynamics::SIR(pars = parametros, init = v_iniciales, time = dt)
        
        N <- sum(v_iniciales)
        
        if( input$datosAMostrar == "Suceptibles" ){
          valODESuceptibles = c(sol[,2])           #Se almacenara la columna  que arroja ODE  como 
          # resultado la funcion ode que corresponde a los suceptibles
          valEpiSuceptibles = c(result$results[,2]) #Se almacenara la columna  que arroja EpiDynamics como suceptibles
          errores = c()
          i = 1
          max = length(valODESuceptibles)
          while(i <= max){
            errorActual =(abs(valODESuceptibles[i] - valEpiSuceptibles[i] )/valEpiSuceptibles[i]) 
            errores[i] = errorActual
            i = i + 1
          }
          plot(dt, errores, type="l", col= "red", xlab = "Dias", ylab = "Error relativo")
          title("Error relativo para Suceptibles")
        }else if(input$datosAMostrar == "Infectados"){
          valODEInfectados = c(sol[,3])
          valEpiInfectados = c(result$results[,3])
          errores = c()
          i = 1
          max = length(valODEInfectados)
          while(i <= max){
            errorActual = (abs(valODEInfectados[i] - valEpiInfectados[i] )/valEpiInfectados[i])
            errores[i] = errorActual
            i = i + 1
          }
          plot(dt, errores, type="l", col= "blue", xlab = "Dias", ylab = "Error relativo")
          title("Error relativo para Infectados")
        }else if(input$datosAMostrar == "Recuperados"){
          valODERecuperados = c(sol[,4])
          valEpiRecuperados = c(result$results[,4])
          errores = c()
          i = 1
          max = length(valODERecuperados)
          while(i <= max){
            errorActual = (abs(valODERecuperados[i] - valEpiRecuperados[i] )/valEpiRecuperados[i])
            errores[i] = errorActual
            i = i + 1
          }
          plot(dt, errores, type="l", col= "blue", xlab = "Dias", ylab = "Error relativo")
          title("Error relativo para Recuperados")
        }
      })
      
      # TABLA DE ERRORES
      
      output$TablaError <- renderDataTable({
        
        library(deSolve)
        library(EpiDynamics)
        
        SIR <- function(t, x, parametros){
          with(as.list(c(parametros, x)),{
            dS <- -(beta*S*I)
            dI <- +(beta*S*I)- gamma*I
            dR <- gamma*I
            
            derivadas <- c(dS, dI, dR)
            return(list(derivadas))
          })
        }
        
        parametros <- c(beta= input$beta, gamma = input$gamma)
        v_iniciales <- c(S=input$Suceptibles, I=input$Infectados, R = input$Recuperados)
        dt <- seq(0, input$Dias, input$ValParticion)
        sol = ode(y=v_iniciales, times=dt, func=SIR,parms=parametros, method = input$MetodoNumerico)
        result = EpiDynamics::SIR(pars = parametros, init = v_iniciales, time = dt)
        
        N <- sum(v_iniciales)
        
        if( input$datosAMostrar == "Suceptibles" ){
          valODESuceptibles = c(sol[,2])
          valEpiSuceptibles = c(result$results[,2])
          errores = c()
          i = 1
          max = length(valODESuceptibles)
          while(i <= max){
            errorActual =(abs(valODESuceptibles[i] - valEpiSuceptibles[i] )/valEpiSuceptibles[i]) 
            errores[i] = errorActual
            i = i + 1
          }
          tabla = cbind(dt, errores)
          colnames(tabla) = c("Tiempo", "Error relativo")
          tabla
        }else if(input$datosAMostrar == "Infectados"){
          valODEInfectados = c(sol[,3])
          valEpiInfectados = c(result$results[,3])
          errores = c()
          i = 1
          max = length(valODEInfectados)
          while(i <= max){
            errorActual = (abs(valODEInfectados[i] - valEpiInfectados[i] )/valEpiInfectados[i])
            errores[i] = errorActual
            i = i + 1
          }
          tabla = cbind(dt, errores)
          colnames(tabla) = c("Tiempo", "Error relativo")
          tabla
        }else if(input$datosAMostrar == "Recuperados"){
          valODERecuperados = c(sol[,4])
          valEpiRecuperados = c(result$results[,4])
          errores = c()
          i = 1
          max = length(valODERecuperados)
          while(i <= max){
            errorActual = (abs(valODERecuperados[i] - valEpiRecuperados[i] )/valEpiRecuperados[i])
            errores[i] = errorActual
            i = i + 1
          }
          tabla = cbind(dt, errores)
          colnames(tabla) = c("Tiempo", "Error relativo")
          tabla
        }
      })
      
      output$TablaErrorBD <- renderDataTable({
        
        library(deSolve)
        library(EpiDynamics)
        
        SIR <- function(t, x, parametros){
          with(as.list(c(parametros, x)),{
            dS <- -(beta*S*I)
            dI <- +(beta*S*I)- gamma*I
            dR <- gamma*I
            
            derivadas <- c(dS, dI, dR)
            return(list(derivadas))
          })
        }
        
        parametros <- c(beta= input$beta, gamma = input$gamma)
        v_iniciales <- c(S=input$Suceptibles, I=input$Infectados, R = input$Recuperados)
        dt <- seq(0, input$Dias, input$ValParticion)
        sol = ode(y=v_iniciales, times=dt, func=SIR,parms=parametros, method = input$MetodoNumerico)
        
      
       
        if(input$datosAMostrar == "Suceptibles"){
          
          infectados_BD<-c(0,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           17,
                           79,
                           132,
                           229,
                           322,
                           400,
                           650,
                           888,
                           1128,
                           1689,
                           2036,
                           2502,
                           3089,
                           3858,
                           4636,
                           5883,
                           7375,
                           9172,
                           10149,
                           12462,
                           15113,
                           17660,
                           21157,
                           23980,
                           27980,
                           31506,
                           35713,
                           41035,
                           47021,
                           53578,
                           59138,
                           63927,
                           69176,
                           74386,
                           80539,
                           86498,
                           92472,
                           97689,
                           101739,
                           105792,
                           110574,
                           115242,
                           119827,
                           124632,
                           128948,
                           132547,
                           135586,
                           139422,
                           143626,
                           147577,
                           152271,
                           156363,
                           159516,
                           162488,
                           165155,
                           168941,
                           172434,
                           175925,
                           178972,
                           181228,
                           183957,
                           187327,
                           189973,
                           192994,
                           195351,
                           197675,
                           199414,
                           201505,
                           203591,
                           205463,
                           207428,
                           209328,
                           210717,
                           211938,
                           213013,
                           214457,
                           215858,
                           217185,
                           218268,
                           219070,
                           219814
          )
          
          poblacion_total=60461828
          
          valODESuceptibles = c(sol[,2])
          
          errores = c()
          i = 1
          max = length(infectados_BD)
          while(i <= max){
            errorActual =(abs(valODESuceptibles[i] - (poblacion_total-infectados_BD[i]) )/(poblacion_total-infectados_BD[i])) 
            errores[i] = errorActual
            i = i + 1
          }
          tabla = cbind(dt, errores)
          colnames(tabla) = c("Tiempo", "Error relativo")
          tabla
        }else if(input$datosAMostrar == "Infectados"){
          
          infectados_BD<-c(0,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           17,
                           79,
                           132,
                           229,
                           322,
                           400,
                           650,
                           888,
                           1128,
                           1689,
                           2036,
                           2502,
                           3089,
                           3858,
                           4636,
                           5883,
                           7375,
                           9172,
                           10149,
                           12462,
                           15113,
                           17660,
                           21157,
                           23980,
                           27980,
                           31506,
                           35713,
                           41035,
                           47021,
                           53578,
                           59138,
                           63927,
                           69176,
                           74386,
                           80539,
                           86498,
                           92472,
                           97689,
                           101739,
                           105792,
                           110574,
                           115242,
                           119827,
                           124632,
                           128948,
                           132547,
                           135586,
                           139422,
                           143626,
                           147577,
                           152271,
                           156363,
                           159516,
                           162488,
                           165155,
                           168941,
                           172434,
                           175925,
                           178972,
                           181228,
                           183957,
                           187327,
                           189973,
                           192994,
                           195351,
                           197675,
                           199414,
                           201505,
                           203591,
                           205463,
                           207428,
                           209328,
                           210717,
                           211938,
                           213013,
                           214457,
                           215858,
                           217185,
                           218268,
                           219070,
                           219814
          )
          valODEInfectados = c(sol[,3])
          
          errores = c()
          i = 1
          max = length(infectados_BD)
          while(i <= max){
            errorActual =(abs(valODEInfectados[i] - infectados_BD[i] )/infectados_BD[i]) 
            errores[i] = errorActual
            i = i + 1
          }
          tabla = cbind(dt, errores)
          colnames(tabla) = c("Tiempo", "Error relativo")
          tabla
        }else if(input$datosAMostrar == "Recuperados"){
          
          recuperados_BD<-c(0,1,2,1,1,3,45,46,46,83,149,160,276,414,523,589,622,724,724,1045,1045,1439,1966,2335,2749,2941,4025,4440,4440,6072,7024,7024,8326,9362,10361,10950,12384,13030,14620,15729,16847,18278,19758,20996,21815,22837,24392,26491,28470,30455,32534,34211,35435,37130,38092,40164,42727,44927,47055,48877,51600,54543,57576,60498,63120,64928,66624,68941,71252,75945,78249,79914,81654,82879,85231,93245,96276,99023,103031,105186,106587,109039,112541,115288,120205,122810,125176,127326,129401,132282,134560,136720,138840)
          valODERecuperados = c(sol[,4])
          
          errores = c()
          i = 1
          max = length(recuperados_BD)
          while(i <= max){
            errorActual =(abs(valODERecuperados[i] - recuperados_BD[i] )/recuperados_BD[i]) 
            errores[i] = errorActual
            i = i + 1
          }
          tabla = cbind(dt, errores)
          colnames(tabla) = c("Tiempo", "Error relativo")
          tabla
          
          
        }

        
      })
      
    }else if(input$Modelo == "SIS"){
      
      output$distPlot <- renderPlot({
        
        #Librerias para el correcto funcionamiento de la solucion
        library(deSolve)
        library(EpiDynamics)
        
        #Modelo SIS tomado de biblioteca universidad Unirioja  https://biblioteca.unirioja.es/tfe_e/TFE002211.pdf
        
        SIS <- function(t, x, parametros){
          with(as.list(c(parametros, x)),{
            dS <- -(gamma*S*I) + (beta*I)
            dI <- +(gamma*S*I) - (beta*I)
            derivadas <- c(dS, dI)
            return(list(derivadas))       #Se devuelven los valores de las derivadas en una lista
          })
        } 
        
        #Parametros Beta y Gamma para la funcion SIS
        parametros <- c(beta= input$beta_sis, gamma = input$gamma_sis)
        #Valores iniciales: suceptibles e infectados (pueden variar dentro de la interfaz)
        v_iniciales <- c(S=input$Suceptibles_sis, I=input$Infectados_sis)
        #La variacion del tiempo entre total de dias y la frecuencia de toma de datos(particiones)
        dt <- seq(0, input$Dias_sis, input$ValParticion_sis)
        #Se utiliza la funcion ode del paquete deSolve para solucionar el sistema pasandole como funcion la SIS de Rpubs (solucion experimental)
        sol = ode(y=v_iniciales, times=dt, func=SIS,parms=parametros, method = input$MetodoNumerico)
        #Usamos la funcion SIS del paquete EpiDynamics para tener una solucion teorica
        result = EpiDynamics::SIS(pars = parametros, init = v_iniciales, time = dt)
        
        
        simulacion.si <- as.data.frame(sol)
        
        attach(simulacion.si)
        
        #Las primeras dos graficas (suceptibles,Infectados) se muestran en una misma grafica con las soluciones que arroja EpiDynamics
        
        N <- sum(v_iniciales)
        plot(dt, S, type="l", col="blue", ylim=c(0,sum(v_iniciales)), xlab = "Dias", ylab="Numero de individuos (en miles)")
        lines(dt, I, type="l", col="red")
        lines(dt, result$results[,2], type = "l", col = "brown")
        lines(dt, result$results[,3], type = "l", col = "orange")
        title("Modelo SIS")
        legend((input$Dias_sis)/2, N/1.5, legend=c("Susceptibles por ODE", "Infectados por ODE", "Suceptibles por EpiDynamics", "Infectados por EpiDynamics"), col=c("blue", "red", "brown", "orange"), lty=rep(1, 2))
        
      })
      
      
      # TABLA DE DATOS 
      
      output$Table <- renderDataTable({
        
        library(deSolve)
        
        #Modelo SIS tomado de biblioteca universidad Unirioja 
        
        SIS <- function(t, x, parametros){
          with(as.list(c(parametros, x)),{
            dS <- -(gamma*S*I) + (beta*I)
            dI <- +(gamma*S*I) - (beta*I)
            derivadas <- c(dS, dI)
            return(list(derivadas))       #Se devuelven los valores de las derivadas en una lista
          })
        } 
        
        
        parametros <- c(beta= input$beta_sis, gamma = input$gamma_sis)
        v_iniciales <- c(S=input$Suceptibles, I=input$Infectados)
        dt <- seq(0, input$Dias_sis, input$ValParticion_sis)
        sol = ode(y=v_iniciales, times=dt, func=SIS,parms=parametros, method = input$MetodoNumerico)
        
        N <- sum(v_iniciales)
        
        if( input$datosAMostrar_sis == "Suceptibles" ){
          tabla = cbind(dt, round((sol[,2]/N)*100,5))
          colnames(tabla) = c("Tiempo", "Suceptibles (% de poblacion)")
          tabla
        }else if(input$datosAMostrar_sis == "Infectados"){
          tabla = cbind(dt, round((sol[,3]/N)*100,5))
          colnames(tabla) = c("Tiempo", "Infectados (% de poblacion)")
          tabla
        }
        
      })
      
      # GRAFICAR ERRORES 
      
      output$Error <- renderPlot({
        
        library(deSolve)
        library(EpiDynamics)
        
        SIS <- function(t, x, parametros){
          with(as.list(c(parametros, x)),{
            dS <- -(gamma*S*I) + (beta*I)
            dI <- +(gamma*S*I) - (beta*I)
            derivadas <- c(dS, dI)
            return(list(derivadas))       #Se devuelven los valores de las derivadas en una lista
          })
        } 
        
        
        parametros <- c(beta= input$beta_sis, gamma = input$gamma_sis)
        v_iniciales <- c(S=input$Suceptibles_sis, I=input$Infectados_sis)
        dt <- seq(0, input$Dias_sis, input$ValParticion_sis)
        sol = ode(y=v_iniciales, times=dt, func=SIS,parms=parametros, method = input$MetodoNumerico)
        result = EpiDynamics::SIS(pars = parametros, init = v_iniciales, time = dt)
        
        N <- sum(v_iniciales)
        
        if( input$datosAMostrar_sis == "Suceptibles" ){
          valODESuceptibles = c(sol[,2])
          valEpiSuceptibles = c(result$results[,2])
          errores = c()
          i = 1
          max = length(valODESuceptibles)
          while(i <= max){
            errorActual =(abs(valODESuceptibles[i] - valEpiSuceptibles[i] )/valEpiSuceptibles[i]) 
            errores[i] = errorActual
            i = i + 1
          }
          plot(dt, errores, type="l", col= "red", xlab = "Dias", ylab = "Error relativo")
          title("Error relativo para Suceptibles")
        }else if(input$datosAMostrar_sis == "Infectados"){
          valODEInfectados = c(sol[,3])
          valEpiInfectados = c(result$results[,3])
          errores = c()
          i = 1
          max = length(valODEInfectados)
          while(i <= max){
            errorActual = (abs(valODEInfectados[i] - valEpiInfectados[i] )/valEpiInfectados[i])
            errores[i] = errorActual
            i = i + 1
          }
          plot(dt, errores, type="l", col= "blue", xlab = "Dias", ylab = "Error relativo")
          title("Error relativo para Infectados")
        }
      })
      
      # TABLA DE ERRORES
      
      output$TablaError <- renderDataTable({
        
        library(deSolve)
        library(EpiDynamics)
        
        SIS <- function(t, x, parametros){
          with(as.list(c(parametros, x)),{
            dS <- -(gamma*S*I) + (beta*I)
            dI <- +(gamma*S*I) - (beta*I)
            derivadas <- c(dS, dI)
            return(list(derivadas))       #Se devuelven los valores de las derivadas en una lista
          })
        } 
        
        
        parametros <- c(beta= input$beta_sis, gamma = input$gamma_sis)
        v_iniciales <- c(S=input$Suceptibles_sis, I=input$Infectados_sis)
        dt <- seq(0, input$Dias_sis, input$ValParticion_sis)
        sol = ode(y=v_iniciales, times=dt, func=SIS,parms=parametros, method = input$MetodoNumerico)
        result = EpiDynamics::SIS(pars = parametros, init = v_iniciales, time = dt)
        
        N <- sum(v_iniciales)
        
        if( input$datosAMostrar_sis == "Suceptibles" ){
          valODESuceptibles = c(sol[,2])
          valEpiSuceptibles = c(result$results[,2])
          errores = c()
          i = 1
          max = length(valODESuceptibles)
          while(i <= max){
            errorActual =(abs(valODESuceptibles[i] - valEpiSuceptibles[i] )/valEpiSuceptibles[i]) 
            errores[i] = errorActual
            i = i + 1
          }
          tabla = cbind(dt, errores)
          colnames(tabla) = c("Tiempo", "Error relativo")
          tabla
        }else if(input$datosAMostrar_sis == "Infectados"){
          valODEInfectados = c(sol[,3])
          valEpiInfectados = c(result$results[,3])
          errores = c()
          i = 1
          max = length(valODEInfectados)
          while(i <= max){
            errorActual = (abs(valODEInfectados[i] - valEpiInfectados[i] )/valEpiInfectados[i])
            errores[i] = errorActual
            i = i + 1
          }
          tabla = cbind(dt, errores)
          colnames(tabla) = c("Tiempo", "Error relativo")
          tabla
        }
      })
      
      
      
      
      output$TablaErrorBD <- renderDataTable({
        
        library(deSolve)
        library(EpiDynamics)
        
        SIR <- function(t, x, parametros){
          with(as.list(c(parametros, x)),{
            dS <- -(beta*S*I)
            dI <- +(beta*S*I)- gamma*I
            dR <- gamma*I
            
            derivadas <- c(dS, dI, dR)
            return(list(derivadas))
          })
        }
        
        parametros <- c(beta= input$beta, gamma = input$gamma)
        v_iniciales <- c(S=input$Suceptibles, I=input$Infectados, R = input$Recuperados)
        dt <- seq(0, input$Dias, input$ValParticion)
        sol = ode(y=v_iniciales, times=dt, func=SIR,parms=parametros, method = input$MetodoNumerico)
        
        
        
        if(input$datosAMostrar == "Suceptibles"){
          
          infectados_BD<-c(0,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           17,
                           79,
                           132,
                           229,
                           322,
                           400,
                           650,
                           888,
                           1128,
                           1689,
                           2036,
                           2502,
                           3089,
                           3858,
                           4636,
                           5883,
                           7375,
                           9172,
                           10149,
                           12462,
                           15113,
                           17660,
                           21157,
                           23980,
                           27980,
                           31506,
                           35713,
                           41035,
                           47021,
                           53578,
                           59138,
                           63927,
                           69176,
                           74386,
                           80539,
                           86498,
                           92472,
                           97689,
                           101739,
                           105792,
                           110574,
                           115242,
                           119827,
                           124632,
                           128948,
                           132547,
                           135586,
                           139422,
                           143626,
                           147577,
                           152271,
                           156363,
                           159516,
                           162488,
                           165155,
                           168941,
                           172434,
                           175925,
                           178972,
                           181228,
                           183957,
                           187327,
                           189973,
                           192994,
                           195351,
                           197675,
                           199414,
                           201505,
                           203591,
                           205463,
                           207428,
                           209328,
                           210717,
                           211938,
                           213013,
                           214457,
                           215858,
                           217185,
                           218268,
                           219070,
                           219814
          )
          
          poblacion_total=60461828
          
          valODESuceptibles = c(sol[,2])
          
          errores = c()
          i = 1
          max = length(infectados_BD)
          while(i <= max){
            errorActual =(abs(valODESuceptibles[i] - (poblacion_total-infectados_BD[i]) )/(poblacion_total-infectados_BD[i])) 
            errores[i] = errorActual
            i = i + 1
          }
          tabla = cbind(dt, errores)
          colnames(tabla) = c("Tiempo", "Error relativo")
          tabla
        }else if(input$datosAMostrar == "Infectados"){
          
          infectados_BD<-c(0,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           3,
                           17,
                           79,
                           132,
                           229,
                           322,
                           400,
                           650,
                           888,
                           1128,
                           1689,
                           2036,
                           2502,
                           3089,
                           3858,
                           4636,
                           5883,
                           7375,
                           9172,
                           10149,
                           12462,
                           15113,
                           17660,
                           21157,
                           23980,
                           27980,
                           31506,
                           35713,
                           41035,
                           47021,
                           53578,
                           59138,
                           63927,
                           69176,
                           74386,
                           80539,
                           86498,
                           92472,
                           97689,
                           101739,
                           105792,
                           110574,
                           115242,
                           119827,
                           124632,
                           128948,
                           132547,
                           135586,
                           139422,
                           143626,
                           147577,
                           152271,
                           156363,
                           159516,
                           162488,
                           165155,
                           168941,
                           172434,
                           175925,
                           178972,
                           181228,
                           183957,
                           187327,
                           189973,
                           192994,
                           195351,
                           197675,
                           199414,
                           201505,
                           203591,
                           205463,
                           207428,
                           209328,
                           210717,
                           211938,
                           213013,
                           214457,
                           215858,
                           217185,
                           218268,
                           219070,
                           219814
          )
          valODEInfectados = c(sol[,3])
          
          errores = c()
          i = 1
          max = length(infectados_BD)
          while(i <= max){
            errorActual =(abs(valODEInfectados[i] - infectados_BD[i] )/infectados_BD[i]) 
            errores[i] = errorActual
            i = i + 1
          }
          tabla = cbind(dt, errores)
          colnames(tabla) = c("Tiempo", "Error relativo")
          tabla
        }
        
        
      })
      
      
      
      
      
      
      
      
      
      
    }
  })
  
}

shinyApp(ui = ui, server = server)

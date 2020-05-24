  
  
  library(shiny)
  
  ui <- fluidPage(
    
    titlePanel("Prueba modelo SIS"),
    
    sidebarLayout(
      sidebarPanel(
        helpText("Cambia el valor de los parametros del modelo"),
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
                    selected = "Suceptibles")
      ),
      
      
      mainPanel(plotOutput("distPlot"),dataTableOutput("Table"), plotOutput("Error"),dataTableOutput("TablaError")
      )
    )
  )
  
  server <- function(input, output) {
    
    output$distPlot <- renderPlot({
      
      #Librerias para el correcto funcionamiento de la solucion
      library(deSolve)
      library(EpiDynamics)
      
      #Modelo SIS tomado de biblioteca universidad Unirioja  
      
      SIS <- function(t, x, parametros){
        with(as.list(c(parametros, x)),{
          dS <- -(gamma*S*I) + (beta*I)
          dI <- +(gamma*S*I) - (beta*I)
          derivadas <- c(dS, dI)
          return(list(derivadas))       #Se devuelven los valores de las derivadas en una lista
        })
      } 
      
      #Parametros Beta y Gamma para la funcion SIS
      parametros <- c(beta= input$beta, gamma = input$gamma)
      #Valores iniciales: suceptibles e infectados (pueden variar dentro de la interfaz)
      v_iniciales <- c(S=input$Suceptibles, I=input$Infectados)
      #La variacion del tiempo entre total de dias y la frecuencia de toma de datos(particiones)
      dt <- seq(0, input$Dias, input$ValParticion)
      #Se utiliza la funcion ode del paquete deSolve para solucionar el sistema pasandole como funcion la SIS de Rpubs (solucion experimental)
      sol = ode(y=v_iniciales, times=dt, func=SIS,parms=parametros, method = input$MetodoNumerico)
      #Usamos la funcion SIS del paquete EpiDynamics para tener una solucion teorica
      result = EpiDynamics::SIS(pars = parametros, init = v_iniciales, time = dt)
      
      simulacion.si <- as.data.frame(sol)
      
      attach(simulacion.si)
      
      #Las primeras tres graficas (suceptibles,Infectados) se muestran en una misma grafica con las soluciones que arroja EpiDynamics
      
      N <- sum(v_iniciales)
      plot(dt, S, type="l", col="blue", ylim=c(0,sum(v_iniciales)), xlab = "Tiempo (Dias)", ylab="Numero de individuos (en miles)")
      lines(dt, I, type="l", col="red")
      lines(dt, result$results[,2], type = "l", col = "brown")
      lines(dt, result$results[,3], type = "l", col = "orange")
      title("Modelo SIS")
      legend((input$Dias)/2, N/1.5, legend=c("Susceptibles por ODE", "Infectados por ODE", "Suceptibles por EpiDynamics", "Infectados por EpiDynamics"), col=c("blue", "red", "brown", "orange"), lty=rep(1, 2))
      
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
      
      
      parametros <- c(beta= input$beta, gamma = input$gamma)
      v_iniciales <- c(S=input$Suceptibles, I=input$Infectados)
      dt <- seq(0, input$Dias, input$ValParticion)
      sol = ode(y=v_iniciales, times=dt, func=SIS,parms=parametros, method = input$MetodoNumerico)
      
      N <- sum(v_iniciales)
      
      if( input$datosAMostrar == "Suceptibles" ){
        tabla = cbind(dt, round((sol[,2]/N)*100,5))
        colnames(tabla) = c("Tiempo", "Suceptibles (% de poblacion)")
        tabla
      }else if(input$datosAMostrar == "Infectados"){
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
      
      
      parametros <- c(beta= input$beta, gamma = input$gamma)
      v_iniciales <- c(S=input$Suceptibles, I=input$Infectados)
      dt <- seq(0, input$Dias, input$ValParticion)
      sol = ode(y=v_iniciales, times=dt, func=SIS,parms=parametros, method = input$MetodoNumerico)
      result = EpiDynamics::SIS(pars = parametros, init = v_iniciales, time = dt)
      
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
        plot(dt, errores, type="l", col= "red", xlab = "Tiempo (en Horas)", ylab = "Error relativo")
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
        plot(dt, errores, type="l", col= "blue", xlab = "Tiempo (en Horas)", ylab = "Error relativo")
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
      
      
      parametros <- c(beta= input$beta, gamma = input$gamma)
      v_iniciales <- c(S=input$Suceptibles, I=input$Infectados)
      dt <- seq(0, input$Dias, input$ValParticion)
      sol = ode(y=v_iniciales, times=dt, func=SIS,parms=parametros, method = input$MetodoNumerico)
      result = EpiDynamics::SIS(pars = parametros, init = v_iniciales, time = dt)
      
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
      }
    })
  }
  
  shinyApp(ui = ui, server = server)

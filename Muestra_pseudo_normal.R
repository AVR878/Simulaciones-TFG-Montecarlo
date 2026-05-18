#Para comenzar instalamos, si hiciera falta, y cargamos el set de librerías que vamos a usar:

#INSTALACIÓN:

paquetes <- c("nortest","ggplot2","tseries","randtests","randtoolbox","qqplotr")

faltan <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(faltan) > 0) install.packages(faltan)

#CARGA:

library(nortest)
library(ggplot2)
library(tseries)
library(randtests)
library(randtoolbox)
library(qqplotr)

#Controlamos la precisión decimal:

options(digits = 10)

#A continuación, en base a la estructura del Algoritmo 5 documentado en el capítulo 2 de la memoria, construimos el generador de valores pseudoaleatorios de la normal estándar de parámetros:

# - muestra_uniforme: vector que almacena los valores de la uniforme necesarios en cada iteración.
# - size: tamaño de la muestra deseado en la salida.

generador_normal=function(muestra_uniforme,size){
  
  muestra=c()#Vector que almacenará la muestra objetivo.
  aceptados=0#Contador de aceptaciones en las iteraciones.
  rechazados=0#Contador de rechazos en las iteraciones.
  n=0#Variable auxiliar para la selección de valores de la uniforme.
  
  for (i in 1:length(muestra_uniforme)) {
    #Como necesitamos 2 valores de la uniforme para cada iteración, con la variable n seleccionamos los valores indexados por los pares: (1,2), (3,4),...
    u=muestra_uniforme[i+n]
    v=muestra_uniforme[i+n+1]
    n=n+1
    #Ahora simulamos el valor correspondiente a la doble exponencial:
    if(v<0.5){
      y=log(2*v)
    }else{
      y=-log(2*(1-v))
    }
    #Y comprobamos la condición de Aceptación/Rechazo incrementando los contadores en cada caso:
    if(u*exp(0.5-abs(y)+0.5*y^2)<=1){
      aceptados=aceptados+1
      muestra=c(muestra,y)
    }else{
      rechazados=rechazados+1
    }
    #Si la cantidad de valores aceptados ya llegase al mínimo deseado establecido en la variable size, nos detenemos:
    if(aceptados>=size){
      break
    }
    
  }
  #Si al recorrer toda la muestra de la uniforme no llegamos al mínimo requerido en size, no devolvemos nada y lo mostramos por pantalla:
  if(aceptados<size){
    cat("La muestra no llega al tamaño mínimo.")
  }else{
    #En caso contrario, devolvemos tanto la muestra de valores pseudoaleatorios generada como una estimación de la tasa de aceptación:
    lista=list(
      sample=muestra,
      tasa_aceptacion=aceptados/(aceptados+rechazados)
    )
    return(lista)
  }
}

#Ahora, generamos la muestra objetivo de tamaño 5000 a través de la muestra uniforme obtenida en el script Muestra_pseudo_uniforme.R:

muestra_pseudo_normal_lista=generador_normal(muestra_uniforme = muestra_pseudo_unif,size = 5000)

#Y extraemos, por separado, la muestra y la estimación de la tasa de aceptación del algoritmo:

muestra_pseudo_normal=muestra_pseudo_normal_lista$sample
tasa_aceptados=muestra_pseudo_normal_lista$tasa_aceptacion

#Calculamos la desviación de la tasa de aceptación respecto a la teórica predicha en la memoria:

tasa_teorica=sqrt(pi/(2*exp(1)))
desviacion=abs(tasa_teorica - tasa_aceptados)

#Una vez generada, vamos a someterla, de forma análoga, a una batería de pruebas estadísticas, basadas en visualizaciones gráficas y contrastes de hipótesis, para comprobar su calidad y bondad de ajuste.


#VISUALIZACIONES GRÁFICAS

#HISTOGRAMA FRECUENCIAS MUESTRA VS DENSIDAD TEÓRICA

#Realizamos un histograma auxiliar

h2 <- hist(muestra_pseudo_normal, breaks = 30, plot = FALSE)

#Usamos los breaks en ggplot

df2 <- data.frame(muestra = muestra_pseudo_normal)

ggplot(df2, aes(x = muestra)) +
  geom_histogram(
    aes(y = after_stat(density)),
    breaks = h2$breaks,
    fill = "green",
    color = "white",
    alpha = 0.8
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    color = "red",
    linewidth = 1.2
  ) +
  scale_x_continuous(limits = c(-4, 4)) +
  labs(
    title = "Histograma vs densidad teórica N(0,1)",
    x = "Valor",
    y = "Densidad"
  ) +
  theme_gray()


#GRÁFICA QQ-LINE

ggplot(df2, aes(sample = muestra_pseudo_normal)) +
  qqplotr::stat_qq_band(distribution = "norm",dparams = list(mean=0,sd=1),conf = 0.95,
                        alpha = 0.5,
                        fill = "blue") +
  ggplot2::stat_qq(distribution = stats::qnorm, dparams = list(mean=0,sd=1)) +
  ggplot2::stat_qq_line(distribution = stats::qnorm, dparams = list(mean=0,sd=1),
                        color = "red", linewidth = 1) +
  labs(
    title = "QQ-plot datos simulados N(0,1)",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_gray()


#CONTRASTES DE HIPÓTESIS NO PARAMÉTRICOS


#ALEATORIEDAD

## Test Rachas

randtests::runs.test(muestra_pseudo_normal,plot = FALSE)

#INDEPENDENCIA

## Test Ljung-Box (Independencia lineal)

#Realizamos el contraste para distintos lags:

Box.test(muestra_pseudo_normal,lag = 1,type = "Ljung-Box")
Box.test(muestra_pseudo_normal,lag = 5,type = "Ljung-Box")
Box.test(muestra_pseudo_normal,lag = 10,type = "Ljung-Box")
Box.test(muestra_pseudo_normal,lag = 15,type = "Ljung-Box")
Box.test(muestra_pseudo_normal,lag = 20,type = "Ljung-Box")


#Como complemento visual, calculamos los gráficos de autocorrelaciones simple y parcial:

par(mfrow = c(1, 2))

acf(muestra_pseudo_normal,
    main="Autocorrelación simple",
    lag.max = 30,
    ci.col="red",
    ci.type="ma")

pacf(muestra_pseudo_normal,
     main="Autocorrelación parcial",
     lag.max = 30,
     ci.col="red")

par(mfrow = c(1, 1))

#BONDAD DEL AJUSTE

## Test Shapiro-Wilk

shapiro.test(muestra_pseudo_normal)

##Test Kolmogorov-Smirnov-Lilliefors

nortest::lillie.test(muestra_pseudo_normal)

## Test Anderson-Darling

nortest::ad.test(muestra_pseudo_normal)

## Test Chi-Cuadrado

nortest::pearson.test(muestra_pseudo_normal)






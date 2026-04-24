#Primero, cargamos la librerías que vamos a emplear:

library(ggplot2)
library(tseries)
library(randtests)
library(randtoolbox)

#Ahora, construimos la función generadora congruencial de la muestra pseudoaleatoria uniforme donde:

# - a, c y M son los parámetros que ajustan el generador.
# - x_0 es la semilla con la que empezamos la secuencia de la muestra.
# - N es el tamaño de la muestra deseado en la salida.

generador_congruencial=function(a,c,M,semilla,N){
  
  muestra=c(semilla)#Empezamos la muestra con el valor escogido de la semilla.
  
  for (i in 2:N) {#Vamos calculando los residuos de las congruencias y añadiéndolos al vector muestra
    x=(a*semilla+c) %% M
    muestra=c(muestra,x)
    semilla=x#Reutilizamos la variable semilla para iniciar el proceso iterativo.
  }
  muestra=muestra/M #Una vez tenemos los residuos, los normalizamos entre 0 y 1.
  return(muestra) #Retornamos la muestra
}

#En base a las condiciones del Teorema de Hull-Dobell, escogemos los siguientes parámetros para ajustar el generador:

# a=39062953 (primo de la forma 4n+1)
# c=37777373 (primo)
# M=2^32
# Semilla => x_0= 6777
# Tamaño muestra => N= 20000

#Y ahora generamos y almacenamos la muestra

muestra_pseudo_unif=generador_congruencial(a=39062953,
                                           c=37777373,
                                           M=2^32,
                                           semilla = 6777,
                                           N=20000)


#Una vez la hemos generado, vamos a someterla a una batería de pruebas estadísticas, tanto gráficas como en forma de contraste de hipótesis, para comprobar su calidad y bondad de ajuste.


#VISUALIZACIONES GRÁFICAS

#HISTOGRAMA FRECUENCIAS MUESTRA VS DENSIDAD TEÓRICA

#Convertimos la muestra a data.frame para ggplot
df=data.frame(muestra = muestra_pseudo_unif)

ggplot(df, aes(x = muestra)) +
  #Histograma con densidad
  geom_histogram(aes(y = after_stat(density)),
                 bins = 20,
                 fill = "green",
                 color = "white",
                 alpha = 0.8) +
  
  #Curva teórica U(0,1)
  stat_function(fun = dunif,
                args = list(min = 0, max = 1),
                color = "red",
                linewidth = 1.1) +
  
  #Título y ejes
  labs(
    title = "Histograma muestra vs densidad teórica uniforme",
    x = "Valor",
    y = "Densidad",
    fill = ""
  ) +
  
  #Tema visual predeterminado
  theme_gray()



#GRÁFICA QQ-LINE

ggplot(df, aes(sample = muestra_pseudo_unif)) +
  stat_qq(distribution = stats::qunif, dparams = list(min = 0, max = 1)) +
  ggplot2::stat_qq_line(distribution = stats::qunif, dparams = list(min = 0, max = 1),
               color = "red", linewidth = 1)  +
  labs(
    title = "QQ-plot datos simulados U(0,1)",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  theme_gray()


#CONTRASTES DE HIPÓTESIS NO PARAMÉTRICOS

#ALEATORIEDAD

## Test Rachas

randtests::runs.test(muestra_pseudo_unif,plot = FALSE)

## Test Poker

randtoolbox::poker.test(muestra_pseudo_unif,nbcard=5)
randtoolbox::poker.test(muestra_pseudo_unif,nbcard = 10)

#INDEPENDENCIA

## Test Ljung-Box(Independencia lineal)

#Realizamos el contraste para distintos lags:

Box.test(muestra_pseudo_unif,lag = 1,type = "Ljung-Box")
Box.test(muestra_pseudo_unif,lag = 5,type = "Ljung-Box")
Box.test(muestra_pseudo_unif,lag = 10,type = "Ljung-Box")
Box.test(muestra_pseudo_unif,lag = 15,type = "Ljung-Box")
Box.test(muestra_pseudo_unif,lag = 20,type = "Ljung-Box")

#Como complemento visual, calculamos los gráficos de autocorrelaciones simple y parcial:

acf(muestra_pseudo_unif,
    main="Autocorrelación simple",
    lag.max = 30,
    ci.col="red",
    ci.type="ma")

pacf(muestra_pseudo_unif,
     main="Autocorrelación parcial",
     lag.max = 30,
     ci.col="red")


#BONDAD DEL AJUSTE

##Test Kolmogorov-Smirnov

ks.test(muestra_pseudo_unif,"punif",0,1)


##Test Chi Cuadrado

#Primero calculamos las frecuencias absolutas de un histograma de la muestra y luego realizamos el contraste:

frecuencias=hist(muestra_pseudo_unif,breaks = 20,plot = FALSE)$counts
chisq.test(frecuencias,p=rep(1/length(frecuencias),length(frecuencias)))















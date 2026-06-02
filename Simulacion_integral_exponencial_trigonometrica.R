#En primer lugar instalamos, si hiciera falta, y cargamos las librerías necesarias:


#INSTALACIÓN:

paquetes=c("dplyr","ggplot2","qqplotr","patchwork","nortest","magrittr")

faltan=paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(faltan) > 0) install.packages(faltan)

#CARGA:

library(dplyr)
library(ggplot2)
library(qqplotr)
library(patchwork)
library(nortest)
library(magrittr)

options(digits = 10)#Controlamos la precisión.



#GENERADOR CONGRUENCIAL

#Como vamos a requerir de la generación de varias muestras uniformes entre 0 y 1, importamos el generador congruencial de la simulación I:

generador_congruencial=function(a=39062953,c=37777373,M=2^32,semilla,N){#Definimos algunos parámetros por defecto para mayor comodidad.
  semilla=semilla %% M
  muestra=c(semilla)
  if(N>1){
    for (i in 2:N) {
      x=(a*semilla+c) %% M
      muestra=c(muestra,x)
      semilla=x
    }
  }
  muestra=muestra/M 
  return(muestra)
}



  
#PARÁMETROS COMUNES

#Según nuestro diseño experimental, definimos los siguientes parámetros de uso común:

n=10000#Tamaño máximo de las muestras.
sizes=seq(1,n,by=20)#Conjunto de tamaños muestrales con tamaño de paso 20.
cuantil=qnorm(1-0.05/2,mean = 0,sd=1)#Cuantil de la cota probabilística del error absoluto con un nivel de significación de 0.05.

#Declaramos la función objetivo a integrar:
f_expresion= expression(exp(-(pi^2)*x)*cos(pi*x/2))
f=function(x){
  eval(f_expresion,list(x=x))
}
#Y calculamos el valor real de la integral:
valor_real_integral=integrate(f,0,1)$value


#MÉTODO MONTECARLO CLÁSICO


#1)Estimación varianza teórica Montecarlo Clásico

#A continuación, vamos a estimar la varianza teórica de la variable aletoria del método Montecarlo Clásico mediante una muestra piloto


#Así pues, tengamos la siguiente muestra uniforme de tamaño 20000

muestra_piloto_unif_classical=generador_congruencial(semilla = 5890,N=20000)

#Y ahora estimamos la varianza con las imágenes mediante el estadístico varianza muestral que posee menor error cuandrático medio que la cuasivarianza muestral:

imagenes_classical=f(muestra_piloto_unif_classical)

estimacion_varianza_classical=var(imagenes_classical)


#2)Función classical Montecarlo

#Definamos ahora la función que nos devuelva las estimaciones de la integral:

classiccal_montecarlo=function(muestra_uniforme,funcion=f){#Declaramos el parámetro de la función por defecto por comodidad.
  N=length(muestra_uniforme)
  estimacion=(1/N)*sum(funcion(muestra_uniforme))
  return(estimacion)
}

#3)Cálculo estimaciones y errores

#Ahora calculamos las estimaciones y errores eligiendo sistemáticamente las semillas en cada ejecución:

vector_estimaciones_classical=c()
errores_classical=c()

for (i in sizes) {
  estim_classical=classiccal_montecarlo(muestra_uniforme = generador_congruencial(semilla = 37*i,N=i))
  error_classical=estim_classical-valor_real_integral
  
  vector_estimaciones_classical=c(vector_estimaciones_classical,estim_classical)
  errores_classical=c(errores_classical,error_classical)
  
}


#Ahora almacenamos la información en una tabla:

datos_simulacion_classical=data.frame(Tamaños=sizes,
                                      Estimacion=vector_estimaciones_classical,
                                      Error=errores_classical)



#MÉTODO VARIABLES ANTITÉTICAS


#1)Estimación varianza teórica

#Análogamente, vamos a estimar la varianza teórica mediante una muestra piloto de tamaño 20000:

muestra_piloto_unif_antitetica=generador_congruencial(semilla = 7000,N=20000)

#Y ahora estimamos la varianza con las imágenes:

imagenes_antiteticas=(1/2)*(f(muestra_piloto_unif_antitetica)+f(1-muestra_piloto_unif_antitetica))

estimacion_varianza_antitetica=var(imagenes_antiteticas)


#2)Función Variables Antitéticas

#Definamos ahora la función que nos devuelva las estimaciones de la integral:

antiteticas_montecarlo=function(muestra_uniforme,funcion=f){#Declaramos el parámetro de la función por defecto por comodidad.
  N=length(muestra_uniforme)
  estimacion=(1/(2*N))*sum(funcion(muestra_uniforme))+(1/(2*N))*sum(funcion(1-muestra_uniforme))
  return(estimacion)
}


#3)Cálculo estimaciones y errores

#Ahora calculamos las estimaciones y errores eligiendo sistemáticamente las semillas en cada ejecución:

vector_estimaciones_antiteticas=c()
errores_antiteticas=c()

for (i in sizes) {
  estim_antiteticas=antiteticas_montecarlo(muestra_uniforme = generador_congruencial(semilla = 73*i,N=i))
  error_antiteticas=estim_antiteticas-valor_real_integral
  
  vector_estimaciones_antiteticas=c(vector_estimaciones_antiteticas,estim_antiteticas)
  errores_antiteticas=c(errores_antiteticas,error_antiteticas)
  
}


#Ahora almacenamos la información en una tabla:

datos_simulacion_antiteticas=data.frame(Tamaños=sizes,
                                        Estimacion=vector_estimaciones_antiteticas,
                                        Error=errores_antiteticas)




#MÉTODO MUESTREO POR IMPORTANCIA


#1)Densidad auxiliar


#Declaramos la densidad auxiliar de la memoria:

densidad_auxiliar=function(x){
  c=(pi^2)/(1-exp(-pi^2))
  y=c*exp(-(pi^2)*x)
  return(y)
}


#2)Simulación método inversión valores densidad auxiliar

#A continuación, tenemos la versión computacional del Algoritmo 6 de la memoria:

generador_auxiliar=function(muestra_uniforme,size){#size es el parámetro que indica cuántos valores queremos simular.
  
  #Primero comprobamos que introducimos tantos valores uniformes como valores queremos simular.
  if(length(muestra_uniforme)!= size){
    cat("La cantidad de números pseudouniformes no coincide con el tamaño de muestra deseado.")
  }
  
  muestra=c()#Vector donde almacenamos los valores simulados.
  
  for (i in 1:size) {
    valor=-(1/(pi^2))*log(abs(1-(1-exp(-pi^2))*muestra_uniforme[i]))
    muestra=c(muestra,valor)
  }
  
  #Y devolvemos la muestra:
  return(muestra)
}


#3)Estimación varianza teórica Importance Sampling

#Una vez tenemos la capacidad de simular valores de la distribución auxiliar, vamos a estimar la varianza teórica mediante una muestra de tamaño 20000:

muestra_piloto_auxiliar_IS=generador_auxiliar(muestra_uniforme = generador_congruencial(semilla = 98765,N=20000),size = 20000)


#Y ahora estimamos la varianza

imagenes_IS=(f(muestra_piloto_auxiliar_IS)/densidad_auxiliar(muestra_piloto_auxiliar_IS))

estimacion_varianza_IS=var(imagenes_IS)



#4)Función Importance Sampling

#Definamos ahora la función que nos devuelva las estimaciones de la integral:

importance_sampling_montecarlo=function(muestra,funcion=f,densidad=densidad_auxiliar){#Definimos los parámetros función y densidad por defecto por comodidad.
  N=length(muestra)
  estimacion=(1/N)*sum(funcion(muestra)/densidad(muestra))
  return(estimacion)
}


#5)Cálculo estimaciones y errores

#Ahora calculamos las estimaciones y errores eligiendo sistemáticamente las semillas en cada ejecución:

vector_estimaciones_IS=c()
errores_IS=c()

for (i in sizes) {
  estim_IS=importance_sampling_montecarlo(muestra = generador_auxiliar(muestra_uniforme = generador_congruencial(semilla = 29*i,N=i),size=i))
  error_IS=estim_IS-valor_real_integral
  
  vector_estimaciones_IS=c(vector_estimaciones_IS,estim_IS)
  errores_IS=c(errores_IS,error_IS)
  
}

#Ahora almacenamos la información en una tabla:

datos_simulacion_IS=data.frame(Tamaños=sizes,
                               Estimacion=vector_estimaciones_IS,
                               Error=errores_IS)




#DATOS UNIFICADOS

#Por tal de poder obtener una imagen visual de los resultados de las simulaciones, vamos a unificar los datos de los tres métodos:

datos_unificados_error <- bind_rows(
  datos_simulacion_classical %>% mutate(
    Método = "Montecarlo clásico",
    Var = estimacion_varianza_classical
  ),
  datos_simulacion_antiteticas %>% mutate(
    Método = "Variables antitéticas",
    Var = estimacion_varianza_antitetica
  ),
  datos_simulacion_IS %>% mutate(
    Método = "Muestreo por importancia",
    Var = estimacion_varianza_IS
  )
)



  
#COMPARACIÓN VARIANZAS

#Ahora, hacemos una matriz de proporciones para comparar la reducción de la varianza entre los 3 métodos:

matriz_comparacion_varianzas <- matrix(
  c(
    1,
    estimacion_varianza_classical / estimacion_varianza_IS,
    estimacion_varianza_classical / estimacion_varianza_antitetica,
    
    estimacion_varianza_IS / estimacion_varianza_classical,
    1,
    estimacion_varianza_IS / estimacion_varianza_antitetica,
    
    estimacion_varianza_antitetica / estimacion_varianza_classical,
    estimacion_varianza_antitetica / estimacion_varianza_IS,
    1
  ),
  nrow = 3,
  byrow = TRUE
)

#Ponemos nombres a filas y columnas, y la mostramos por consola:

colnames(matriz_comparacion_varianzas) <- c("MC", "IS", "ANT")
rownames(matriz_comparacion_varianzas) <- c("MC", "IS", "ANT")

round(matriz_comparacion_varianzas)




# COTA SUPERIOR PROBABILÍSTICA DEL ERROR ABSOLUTO DE LAS ESTIMACIONES

f_teorica <- function(x, var) {#El parámetro x indica el tamaño muestral y var indica la varianza.
  cuantil * sqrt(var) / sqrt(x)
}



#COMPARATIVA CONVERGENCIA

ggplot(datos_unificados_error, aes(x = Tamaños, y = Estimacion, color = "Estimación")) +
  geom_line(linewidth = 0.8) +
  geom_hline(aes(yintercept = valor_real_integral, color = "Valor real"), linewidth = 1) +
  scale_color_manual(values = c("Estimación" = "blue", "Valor real" = "red")) +
  facet_wrap(~Método, ncol = 3, scales = "free_y") +
  labs(
    title = "Convergencia estimaciones métodos de Montecarlo",
    x = "Tamaño muestral",
    y = "",
    color = ""
  ) +
  theme_gray() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

#GRÁFICO CONTRASTE CONVERGENCIA

ggplot(datos_unificados_error,
       aes(x = Tamaños, y = Estimacion, color = Método)) +
  
  geom_line(aes(alpha = Método),
            linewidth = 1.2) +
  
  geom_hline(yintercept = valor_real_integral,
             color = "red", linewidth = 1) +
  
  coord_cartesian(
    ylim = c(valor_real_integral - 0.005,
             valor_real_integral + 0.005)#Se elige esta semianchura del zoom por el diminuto valor de la integral.
  ) +
  
  scale_color_manual(values = c(
    "Montecarlo clásico" = "#D32F2F",      # rojo intenso
    "Variables antitéticas" = "#1976D2",      # azul saturado
    "Muestreo por importancia"   = "#2E7D32"       # verde oscuro brillante
  )) +
  
  scale_alpha_manual(values = c(
    "Montecarlo clásico" = 0.22,
    "Variables antitéticas" = 0.41,
    "Muestreo por importancia"   = 1.2
  )) +
  
  labs(
    title = "Contraste convergencias métodos de Montecarlo",
    x = "Tamaño muestral",
    y = "Estimación"
  ) +
  
  theme_bw() +  # fondo blanco y líneas finas
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank()
  )





#COMPARATIVA ERROR (ESCALA LOGARÍTMICA)

ggplot(datos_unificados_error, aes(x = Tamaños)) +
  geom_line(aes(y = abs(Error), color = "Empírico"), linewidth = 1) +
  
  # Curvas teóricas por método
  stat_function(
    data = datos_unificados_error %>% filter(Método == "Montecarlo clásico"),
    aes(color = "Teórico"),
    fun = function(x) f_teorica(x, estimacion_varianza_classical),
    linewidth = 1,
    linetype = "dashed"
  ) +
  stat_function(
    data = datos_unificados_error %>% filter(Método == "Variables antitéticas"),
    aes(color = "Teórico"),
    fun = function(x) f_teorica(x, estimacion_varianza_antitetica),
    linewidth = 1,
    linetype = "dashed"
  ) +
  stat_function(
    data = datos_unificados_error %>% filter(Método == "Muestreo por importancia"),
    aes(color = "Teórico"),
    fun = function(x) f_teorica(x, estimacion_varianza_IS),
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = c("Empírico" = "blue", "Teórico" = "red")) +
  
  facet_wrap(~Método, ncol = 3, scales = "free_y") +
  
  labs(
    title = "Errores muestrales absolutos métodos de Montecarlo (log–log)",
    x = "Tamaño muestral",
    y = "",
    color = ""
  ) +
  theme_gray() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )



# CONTRASTE NORMALIDAD ERRORES ASINTÓTICOS

#Ahora, vamos a fijar un tamaño muestral suficientemente grande para comprobar si los errores "asintóticos" tienden a distribuirse por una normal estándar:

tamaño_fijo=8000#Fijamos el tamaño muestral común.
replicas=300#Indicamos cuántos errores tomaremos de cada método.

#Declaramos los vectores que almacenarán los errores por método:

errores_asintoticos_clasico=c()
errores_asintoticos_IS=c()
errores_asintoticos_antiteticas=c()

for (i in 1:replicas) {#Nótese que lo que hace diferente a cada error es la distinta elección de la semilla de cada generador en cada réplica.
  
  error_as_clasico=classiccal_montecarlo(muestra_uniforme = generador_congruencial(semilla = 47*i,N=tamaño_fijo))-valor_real_integral
  error_as_antiteticas=antiteticas_montecarlo(muestra_uniforme = generador_congruencial(semilla = 67*i,N=tamaño_fijo))-valor_real_integral
  error_as_IS=importance_sampling_montecarlo(muestra = generador_auxiliar(muestra_uniforme = generador_congruencial(semilla = 97*i,N=tamaño_fijo),size=tamaño_fijo))-valor_real_integral
  
  errores_asintoticos_clasico=c(errores_asintoticos_clasico,error_as_clasico)
  errores_asintoticos_antiteticas=c(errores_asintoticos_antiteticas,error_as_antiteticas)
  errores_asintoticos_IS=c(errores_asintoticos_IS,error_as_IS)
}

#Guardamos los datos en una tabla

metodo=c(rep("Montecarlo clásico",replicas),
         rep("Variables antitéticas",replicas),
         rep("Muestreo por importancia",replicas))

errores_asintoticos_globales=c(errores_asintoticos_clasico,
                               errores_asintoticos_antiteticas,
                               errores_asintoticos_IS)


#Estandarizamos los errores para contrastar su normalidad estándar asintótica.

error_asintotico_por_metodo=data.frame(Método=metodo,
                                       Error=errores_asintoticos_globales) %>% 
  mutate(Error_estandarizado=case_when(
    Método=="Montecarlo clásico" ~ Error/(sqrt(estimacion_varianza_classical)/sqrt(tamaño_fijo)),
    Método=="Variables antitéticas" ~ Error/(sqrt(estimacion_varianza_antitetica)/sqrt(tamaño_fijo)),
    Método=="Muestreo por importancia" ~ Error/(sqrt(estimacion_varianza_IS)/sqrt(tamaño_fijo)),
    TRUE ~ NA_real_
  ))


#A continuación, creamos dos gráficos guardados en dos variables para luego combinarlos:

#Histogramas frecuencias errores

ggplot(error_asintotico_por_metodo, aes(x = Error_estandarizado)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30,
    fill = "green",
    color = "white",
    alpha = 0.8
  ) +
  facet_wrap(~Método, ncol = 3, scales = "free") +
  labs(
    title = "Histogramas",
    x = "Error",
    y = ""
  ) +
  theme_gray() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

#QQ-plots respecto a la N(0,1)

ggplot(error_asintotico_por_metodo, aes(sample = Error_estandarizado)) +
  
  # Bandas de confianza
  qqplotr::stat_qq_band(
    distribution = "norm",
    dparams = list(mean = 0, sd = 1),
    conf = 0.95,
    alpha = 0.5,
    fill = "blue"
  ) +
  
  # Puntos del QQ-plot
  ggplot2::stat_qq(
    distribution = stats::qnorm,
    dparams = list(mean = 0, sd = 1),
    color = "black",
    size = 1
  ) +
  
  # Línea teórica
  ggplot2::stat_qq_line(
    distribution = stats::qnorm,
    dparams = list(mean = 0, sd = 1),
    color = "red",
    linewidth = 1
  ) +
  
  facet_wrap(~Método, ncol = 3, scales = "free") +
  
  labs(
    title = "QQ\u2013plots",
    x = "Cuantiles teóricos",
    y = "Cuantiles muestrales"
  ) +
  
  theme_gray() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )






#TABLA CONTRASTES DE HIPÓTESIS

#Por método, contrastamos la normalidad con una batería de contrastes de hipótesis:

resultados_CH_normalidad=error_asintotico_por_metodo %>% group_by(Método) %>% 
  summarise(P_valor_SW=shapiro.test(Error_estandarizado)$p.value,
            P_valor_KSL=nortest::lillie.test(Error_estandarizado)$p.value,
            P_valor_AD=nortest::ad.test(Error_estandarizado)$p.value,
            P_valor_Pearson=nortest::pearson.test(Error_estandarizado)$p.value) %>% 
  ungroup()

#Mostramos los resultados por consola:

resultados_CH_normalidad











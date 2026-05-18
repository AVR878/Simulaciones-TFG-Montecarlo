#En primer lugar, instalamos, si hiciera falta, y cargamos las librerías necesarias:

#INSTALACIÓN:

paquetes <- c("magrittr","ggplot2")

faltan <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(faltan) > 0) install.packages(faltan)

#CARGA:

library(magrittr)
library(ggplot2)

options(digits = 10)#Controlamos la precisión decimal


#DETERMINACIÓN DE N PARA UNA COTA SUPERIOR DEL ERROR ABSOLUTO


#Declaramos la función y calculamos su cuarta derivada:

f_expr <- expression(cos(x^2))
f4_expr <- f_expr %>% D("x") %>% D("x") %>% D("x") %>% D("x")

#Ahora, las convertimos en funciones evaluables

f=function(x){
  eval(f_expr,list(x=x))
}

f4 <- function(x) {
  eval(f4_expr, list(x = x))
}

#A continuación, representamos el valor absoluto de la cuarta derivada en el intervalo [0,1] para hallar su valor máximo

datos <- data.frame(
  x = seq(0, 1, length.out = 2000)
)
datos$y <- abs(f4(datos$x))

# Gráfico
ggplot(datos, aes(x = x, y = y)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    title = "Gráfica |f''''(x)|",
    x = "x",
    y = "y"
  ) +
  theme_gray()


#Como podemos apreciar en el gráfico, el máximo se alcanza en x=1.

#Almacenamos ese valor y, para un error máximo de 10^-5, calculamos el n par siguiendo las expresión de la memoria:

error=10^-5
maximo=abs(f4(1))

n=ceiling(sqrt(sqrt(maximo/(180*error))))

#Y nos quedamos con el primer número par:

while (n %% 2 !=0) {
  n=n+1
}

#REGLA SIMPSON COMPUESTA 1/3

#Una vez tenemos el valor de n, pasamos a implementar la fórmula de Simpson 1/3 compuesta 

#Primero, construimos la malla de puntos entre 0 y 1: x_0=0, x_1, ..., x_n=1

h <- 1 / n
i <- 0:n
malla <- h * i

#Al empezar en R todas las indexaciones por 1, tenemos que hacer un pequeño ajuste para distinguir los índices pares e impares de los sumatorios:

malla_auxiliar=malla[2:length(malla)]

#Y ahora sí, tomamos los índices pares e impares de los dos sumatorios:

indices_impares=seq(1,n-1,by=2)
indices_pares=seq(2,n-2,by=2)

#Por último, calculamos la aproximación:

aproximacion=(h/3)*(f(0)+4*sum(f(malla_auxiliar[indices_impares]))+2*sum(f(malla_auxiliar[indices_pares]))+f(1))

#A continuación, procedemos a calcular el valor real de la integral y el error absoluto de la aproximación:

valor_real=integrate(f,0,1)$value

error_absoluto=abs(valor_real-aproximacion)



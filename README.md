# Simulaciones TFG Montecarlo Grado en Matemáticas UIB

En el presente repositorio, se hallan configurados los scripts en el lenguaje de programación *R* mediante los cuales, en esencia, se han llevado a cabo las siguientes simulaciones:

- **Simulación I**: consistente en generar una muestra pseudoaleatoria de tamaño 5000 de la distribución probabilística $N(0,1)$.
- **Simulación II**: basada en la ejecución de tres técnicas de integración unidimensional de Montecarlo, para estimar una integral exponencial-trigonométrica..

pertenecientes, respectivamente, a los capítulos 2 y 3 de la memoria TFG titulada *"El Método de Montecarlo: Una introducción a los números pseudoaleatorios aplicados a la integración unidimensional"* realizada por *A.J Vázquez Ramírez*.

## Preliminares: instalación del entorno de programación

Con la finalidad de que el lector pueda reproducir los resultados de las simulaciones en su entorno local, vamos a describir brevemente los elementos que conforman el entorno de programación *R* utilizado:

- **R**: lenguaje de programación orientado al cálculo estadístico, análisis de datos y simulación numérica. Es el motor principal donde se ejecutan los scripts y donde se llevan a cabo las simulaciones de Montecarlo.
- **Rtools**: conjunto de herramientas de desarrollo necesarias para compilar paquetes de R en sistemas Windows. Incluye utilidades como `gcc`, `make` y otros compiladores que permiten instalar paquetes que contienen código en C, C++ o Fortran.
- **RStudio**: entorno de desarrollo integrado (IDE) para R que facilita la edición de scripts, la gestión de proyectos, la visualización de gráficos y la ejecución interactiva del código. Proporciona una interfaz más cómoda y estructurada para trabajar con R.

los cuales, respectivamente y siguiendo el orden expuesto, se descargan a través de los enlaces web que siguen:

- R: https://cran.r-project.org  
- Rtools: https://cran.r-project.org/bin/windows/Rtools  
- RStudio: https://posit.co/download/rstudio-desktop/

## Ejecución de las simulaciones: pasos y recomendaciones

Una vez tenemos instalado y configurado el entorno en nuestro equipo, podemos proceder con la descarga de todos los scripts **.R** de este repositorio para reproducir las simulaciones.

En particular, los cálculos de las simulaciones están representados por los siguientes conjuntos de scripts:

- **Simulación I**: *Muestra_pseudo_uniforme.R* y *Muestra_pseudo_normal.R*.
- **Simulación II**: *Simulacion_integral_exponencial_trigonometrica.R*.

Todos ellos, están exhaustivamente comentados para que el lector pueda saber, en todo momento, qué esta haciendo cada parte del código; en consecuencia, se recomienda encarecidamente la **lectura previa de los comentarios** para hacer un correcto seguimiento de las ejecuciones por consola.

Por último, detallamos los pasos a seguir para reproducir los resultados de las simulaciones reflejados en la memoria final:

- **Primera simulación**: la ejecución **debe hacerse de forma secuencial** ejecutando, en primer lugar, el script *Muestra_pseudo_uniforme.R* para, acto seguido, ejecutar el script *Muestra_pseudo_normal.R* que nos devuelve la muestra objetivo. La razón principal de esta separación, radica en que para obtener la muestra pseudonormal necesitamos, previamente, simular una muestra pseudouniforme; por tanto, como cada muestra requiere de su propio análisis de calidad y bondad, se ha decidido, por comodidad, separar la ejecución en estos dos scripts.

- **Segunda simulación**: a diferencia de la primera, la segunda simulación se hace solo a través de un único script; concretamente, el script *Simulacion_integral_exponencial_trigonometrica.R* posee, integramente, todo el análisis de convergencia y residuos de los tres métodos seleccionados para la simulación de la integral objetivo.

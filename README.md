# Fundamentos de Estadística Bayesiana

_Abril 6, 2017_

------
<center>
[Mini] Taller de Métodos Numéricos y Estadísticos en Cosmología 2017

Cinvestav, CDMX
</center>

------

# Resumen

En esta plática revisaremos algunos fundamentos metodológicos asociados con el paradigma bayesiano de aprendizaje estadístico (inferencial y predictivo). Prestaremos particular atención a la especificación de estructuras de dependencia estocástica en modelación, así como a la revisión de herramientas computacionales contemporáneas para su implementación práctica.

La plática intentará ser interactiva, por lo que varios ejemplos serán presentados en el lenguaje de programación `R` <a href="https://www.r-project.org/">[Liga]</a> junto con la revisión metodológica del paradigma. 

------

# Agenda

1. Incertidumbre y aleatoriedad

2. Subjetividad y modelación 

3. Aprendizaje y predicción

3. Herramientas computacionales

5. Aplicaciones

6. Temas abiertos

------

Presentacion: <a href="https://github.com/jcmartinezovando/17-04.Cinvestav_Taller/blob/master/17-04.Cinvestav_TallerBayes.pdf">[.pdf]</a>

------

# Herramientas computacionales

* `R` <a href="https://www.r-project.org/">[Liga]</a> (lenguaje de programación)

* `RStudio` <a href="https://www.rstudio.com/">[Liga]</a> (editor/entorno de trabajo para reproducibilidad)

* `JAGS` <a href="http://mcmc-jags.sourceforge.net/">[Liga]</a> (implementación bayesiana)

`R` es un lenguaje de programación libre que funciona con librerías/paquetes concentrados en un repositorio `CRAN` <a href="https://cran.r-project.org/">[Liga]</a>. En esta sesión emplearemos un grupo de esos paquetes. Les pido, puedan instalarlos con las siguientes instrucciones que se copian y pegan en la `linea de comando` de `R`:

```
install.packages("dplyr")
install.packages("ggplot2")
install.packages("LearnBayes")
install.packages("mcmc")
install.packages("MCMCpack")
install.packages("coda")
install.packages("boa")
```
------

**En esta <a href="https://cran.r-project.org/doc/contrib/Paradis-rdebuts_en.pdf">[Liga]</a> podrán descargar un tutorial básico para `R`.** Vean también esta <a href="https://cran.r-project.org/doc/contrib/Short-refcard.pdf">[Referencia]</a>.

Dentro de `RStudio` estaremos trabajando en el ambiente `Markdown` <a href="http://rmarkdown.rstudio.com/">[Liga]</a>. Vean también esta <a href="https://guides.github.com/pdfs/markdown-cheatsheet-online.pdf">[Referencia]</a>.

# Tlamatini
Funciones utiles para el analisis de datos Biologicos/Ecologicos

Paquete ***Tlamatini***. Este paquete contiene funciones útiles para el análisis de datos biológicos/ecológicos. 
Es una compilación de funciones útiles creadas por mi o por alguien más, pero que facilitan el análisis de datos y ajuste de modelos LM, LMM, GLM, GLMM, GAM, etc. 
Tlamatini fue creada con la finalidad de proporcionar herramientas estadísticas a hablantes del Español, principalmente de Ciencias Biológicas. 
El nombre de la paquetería proviene del Náhuatl, Tlamatini (los que saben algo o los que saben cosas), traducido como hombres sabios, era el equivalente a los filósofos 
en la época de los Mexicas. Los Biólogos y Ecólogos, al igual que un Tlamatini, "sabemos cosas", cosas del mundo natural. 

El paquete está todavía en su **etapa de desarrollo**, puede haber errores en algunas funciones y pueden cambiar en el futuro.

Para instalar una versión de desarrollo del paquete desde GitHub:


<!-- ## Install package -->

<!-- To install a released version of the package from *CRAN*: -->

<!-- ```{r, eval=FALSE} -->

<!-- install.packages("tlamatini") -->

<!-- ``` -->



``` r
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("mariosandovalmx/tlamatini")
```

<!-- *** -->

<!-- CONTACTO -->
## Contacto

Si tienes alguna duda, quieres sugerir alguna función o quisieras reportar algún problema con el paquete puedes contactarme a:
Mario Sandoval - [@mariosandovalmo](https://twitter.com/mariosandovalmo) - sandoval.m@hotmail.com

Más información sobre mi: [https://mariosandovalmx.github.io/ecology/](https://mariosandovalmx.github.io/ecology/)

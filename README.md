
# Tlamatini


<br />
<p align="center">
  <a href="https://github.com/mariosandovalmx/tlamatini">
    <img src="https://github.com/mariosandovalmx/ecology/blob/3f7a8cf7100d008de02c081a63d65e7bc3aadd13/images/tlamatini-logo.jpg" alt="Logo" width="1500" height="400">
  </a>
</p>


Paquete ***Tlamatini***. Este paquete contiene funciones útiles para el análisis de datos, principalmente para modelos lineales (LM, GLM, GLMM, GAM). Aunque está pensado para usarse en Ciencias Biológicas, su aplicación se extiende a cualquier área del conocimiento que requiera análisis estadísticos. Es una compilación de funciones útiles creadas por mi o creadas por alguien más, pero que facilitan el análisis de datos: transformaciones de datos, ajuste y revisión de modelos lineales (LM, LMM, GLM, GLMM, GAM, etc.), exportación de tablas, entre otros. 

Tlamatini fue creada con la finalidad de proporcionar herramientas estadísticas a hablantes del Español, principalmente de Ciencias Biológicas. 
El nombre de la paquetería proviene del Náhuatl, Tlamatini (los que saben algo o los que saben cosas), traducido como hombres sabios, era el equivalente a los filósofos 
en la época de los Mexicas. Quienes nos dedicamos a las Ciencias Naturales, al igual que un Tlamatini, "sabemos cosas", cosas del mundo natural. 

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

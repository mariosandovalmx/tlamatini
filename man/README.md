
# Tlamatini
<a href="https://doi.org/10.5281/zenodo.7765347"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.7765347.svg" alt="DOI"></a>

<br />
<p align="center">
  <a href="https://github.com/mariosandovalmx/tlamatini">
    <img src="https://raw.githubusercontent.com/mariosandovalmx/ecology/main/images/tlamatini-logo.jpg" alt="Logo" width="1500" height="400">
  </a>
</p>
https://raw.githubusercontent.com/mariosandovalmx/ecology/main/images/tlamatini-logo.jpg

Paquete ***Tlamatini***. Este paquete contiene funciones útiles para el análisis de datos, principalmente para modelos lineales (LM, GLM, GLMM, GAM). Aunque está pensado para usarse en Ciencias Biológicas, su aplicación se extiende a cualquier área del conocimiento que requiera análisis estadísticos. Es una compilación de funciones útiles creadas por mi o creadas por alguien más, pero que facilitan el análisis de datos: transformaciones de datos, ajuste y revisión de modelos lineales (LM, LMM, GLM, GLMM, GAM, etc.), exportación de tablas, entre otras funciones. Este paquete surge de la necesidad de disponer de herramientas estadísticas en idioma Español, y pretende proveer de una herramienta estadistica a  los hablantes de lengua hispana.   

Tlamatini fue creada con la finalidad de proporcionar herramientas estadísticas a hablantes del Español. 
El nombre de la paquetería proviene del Náhuatl, Tlamatini (los que saben algo o los que saben cosas), traducido como hombres sabios, era el equivalente a los filósofos 
en la época de los Mexicas. Quienes nos dedicamos a las Ciencias Naturales, al igual que un Tlamatini, "sabemos cosas", cosas del mundo natural. 

El paquete está todavía en su **etapa de desarrollo**, puede haber errores en algunas funciones y pueden cambiar en el futuro. Poco a poco iré corrigiendo posibles errores en las funciones.

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
Para consultar un tutorial rápido de como usar esta paqueteria con un GLM: 
[https://mariosandovalmx.github.io/tlamatini-website/GLM_ejemplo.html](https://mariosandovalmx.github.io/tlamatini-website/GLM_ejemplo.html)


<!-- CONTACTO -->
## Contacto

Si tienes alguna duda, quieres sugerir alguna función o quisieras reportar algún problema con el paquete puedes contactarme a:
Mario Sandoval - [@mariosandovalmo](https://twitter.com/mariosandovalmo) - sandoval.m@hotmai.com ; mas413bio@gmail.com

Más información sobre mi: [https://mariosandovalmx.github.io/ecology/](https://mariosandovalmx.github.io/ecology/)

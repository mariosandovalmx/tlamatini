#' Grafico de residuales de un modelo LM, LMM, GLM, GLMM, GAM, etc
#'
#' Muestra el ajuste del modelo LM, GLM, GLMM, GAM, etc. Esta función crea residuos escalados
#' mediante la simulación del modelo ajustado, 250 simulaciones por default. Los residuos pueden ser extraídos con residuals.DHARMa. Ver
#' testResiduals para una visión general de las pruebas de residuos, plot.DHARMa para una visión general de
#' los gráficos disponibles. Esta función fue creada para explorar el ajuste, normalidad y homogeneidad de
#' varianzas entre los grupos.
#' @param Modelo Un modelo LM, GLM, GLMM, GAM.
#' @param nsim Número de simulaciones. Cuanto menor sea el número, mayor será el error estocástico en los residuales.
#' Además, para n muy pequeño, los artefactos de discretización pueden influir en las pruebas. Por defecto
#' es 250, que es un valor relativamente seguro. Puede considerar aumentar a 1000 para estabilizar
#' los valores simulados.
#'
#' @return Grafica los residuales de un modelo.
#' @export
#'
#' @references
#'   \itemize{
#'   \item Ali MM, Sharma SC (1996) Robustness to nonnormality of regression F-tests. J Econom71, 175–205.
#'
#'   \item Lumley T, Diehr P, Emerson S, Chen L (2002) The importance of the normality assumption in
#'   large public health data sets. Annu Rev Public Health23, 151–169.
#'   }
#'
#' @examples
#' #datos <- datasets::ChickWeight
#' #library(glmmTMB)
#' #modelo <- glmmTMB(weight ~ Diet +(1|Chick), family=gaussian("log"), data = datos)
#' #resid_DHARMa(modelo)
#' @encoding UTF-8
#' @importFrom DHARMa simulateResiduals
resid_DHARMa <- function(Modelo, nsim=NULL){
if(is.null(nsim) ){
    options(encoding = "UTF-8")
    insight::print_color("Si Kolmogorov-Smirnov test (KS test) p < 0.05, entonces no hay normalidad en los residuales. Sin embargo, esta prueba es muy sensible al tama\u00f1o de muestra, con tama\u00f1o de muestra grande el valor de p tiene a < 0.05, pero la distribuci\u00f3n se aproxima mucho a la normal. Se recomienda ser un poco flexible con esta prueba al momento de examinar los residuales (examinar visualmente). Con tama\u00f1os de muestra grandes mayores a 500 el Teorema del Limite Central garantiza que los coeficientes de regresi\u00f3n se distribuyen normalmente en promedio (Ali & Sharma, 1996, Lumley et al., 2002).", "green")

    insight::print_color("Outlier test: p < 0.05, no hay outliers. En caso de haber outliers, usar la funci\u00f3n outliers.DHARMa para saber cu\u00e1les son los datos influyentes.", "green")

    insight::print_color("Dispersion test: p < 0.05, Indica problemas de sub/sobredispersi\u00f3n. En caso de haber problemas se recomienda ajustar el par\u00e1metro dispformula, solo para la paqueter\u00eda glmmTMB.", "green")


    insight::print_color("Gr\u00e1fico de la derecha muestra la distribuci\u00f3n esperada de los residuales, mediante simulaciones. Esto es \u00fatil cuando no sabemos cu\u00e1l es la distribuci\u00f3n nula de los residuales. El grafico de la derecha muestra los residuales contra los valores esperados (ajustados). Estas l\u00edneas pueden no ajustarse debido a un tama\u00f1o de muestra reducido.", "green")

    insight::print_color("Recuerda citar el paquete DHARMa. Usa citation('DHARMa') para ver como citar este paquete.", "red")


    res.mod <-DHARMa::simulateResiduals(Modelo,n = 250)
    return(plot(res.mod, rank = T))
  } else if(is.numeric(nsim)){
    options(encoding = "UTF-8")
    n.sim<- nsim

    insight::print_color("Si Kolmogorov-Smirnov test (KS test) p < 0.05, entonces no hay normalidad en los residuales. Sin embargo, esta prueba es muy sensible al tama\u00f1o de muestra, con tama\u00f1o de muestra grande el valor de p tiene a < 0.05, pero la distribuci\u00f3n se aproxima mucho a la normal. Se recomienda ser un poco flexible con esta prueba al momento de examinar los residuales (examinar visualmente). Con tama\u00f1os de muestra grandes mayores a 500 el Teorema del Limite Central garantiza que los coeficientes de regresi\u00f3n se distribuyen normalmente en promedio (Ali & Sharma, 1996, Lumley et al., 2002).", "green")

    insight::print_color("Outlier test: p < 0.05, no hay outliers. En caso de haber outliers, usar la funci\u00f3n outliers.DHARMa para saber cu\u00e1les son los datos influyentes.", "green")

    insight::print_color("Dispersion test: p < 0.05, Indica problemas de sub/sobredispersi\u00f3n. En caso de haber problemas se recomienda ajustar el par\u00e1metro dispformula, solo para la paqueter\u00eda glmmTMB.", "green")

    insight::print_color("Gr\u00e1fico de la derecha muestra la distribuci\u00f3n esperada de los residuales, mediante simulaciones. Esto es \u00fatil cuando no sabemos cu\u00e1l es la distribuci\u00f3n nula de los residuales. El grafico de la derecha muestra los residuales contra los valores esperados (ajustados). Estas l\u00edneas pueden no ajustarse debido a un tama\u00f1o de muestra reducido.", "green")
    insight::print_color("Recuerda citar el paquete DHARMa. Usa citation('DHARMa') para ver como citar este paquete.", "red")

    base::message(c(paste("Se usaron ", nsim, "simulaciones.")))
    res.mod <-DHARMa::simulateResiduals(Modelo,n = n.sim)
    return(plot(res.mod, rank = T))

  }


}



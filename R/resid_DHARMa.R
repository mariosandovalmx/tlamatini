#' Grafico de residuales de un modelo LM, LMM, GLM, GLMM, GAM, etc
#'
#' Muestra el ajuste del modelo LM, GLM, GLMM, GAM, etc. Esta función crea residuos escalados
#' mediante la simulación del modelo ajustado, 250 simulaciones por default. Los residuos pueden ser extraídos con residuals.DHARMa. Ver
#' testResiduals para una visión general de las pruebas de residuos, plot.DHARMa para una visión general de
#' los gráficos disponibles. Esta función fue creada para explorar el ajuste, normalidad y homogeneidad de
#' varianzas entre los grupos.
#' @param Modelo Un modelo LM, GLM, GLMM, GAM.
#' @param nsim Número de simulaciones. Cuanto menor sea el número, mayor será el error estocástico en los residuales.
#' Además, para n muy pequeño, los artefactos de discretización pueden influir en las pruebas. Por defecto es 250, que
#' es un valor relativamente seguro. Puede considerar aumentar a 1000 para estabilizar los valores simulados.
#'
#' @return Grafica los residuales de un modelo.
#' @export
#'
#' @examples
#' #datos <- datasets::ChickWeight
#' #library(lme4)
#' #modelo <- glmer(weight ~ Diet +(1|Chick), family=gaussian("log"), data = datos)
#' #resid_DHARMa(modelo)
#' @encoding UTF-8
#' @importFrom DHARMa simulateResiduals
#'

resid_DHARMa <- function(Modelo, nsim=NULL){
  if(is.null(nsim) ){
    options(encoding = "UTF-8")
    base::message(c("Si Kolmogorov-Smirnov test (KS test) p < 0.05, entonces no hay normalidad en los residuales. Sin embargo esta prueba es muy sensible al tamano de muestra, con tamano de muestra grande el valor de p tiene a < 0.05, pero la distribucion se apoxima mucho a la normal. Se recomienda ser un poco flexible con esta prueba al momento de examinar los residuales (examinar visualmente). Con tamanos de muestra grandes mayores a 500 el Teorema del Limite Central garantiza que los coeficientes de regresion se distribuyen normalmente en promedio (Ali & Sharma, 1996, Lumley et al., 2002)."))

    base::message(c("Outlier test: p < 0.05, no hay outliers. En caso de haber outliers, usar la funcion outliers.DHARMa para saber cuales son los datos influyentes."))
    base::message(c("Dispersion test: p < 0.05, Indica problemas de sub/sobredispersion. En caso de haber problemas se recomienda ajustar el parametro dispformula, solo para la paqueteria glmmTMB."))
    base::message(c("Grafico de la derecha muestra la distribucion esperada de los resiudales, mediante simulaciones. Esto es util cuando no sabemos cual es la distribucion nula de los residuales. El grafico de la derecha muestra los residuales contra los valores esperados (ajustados). Estas lineas pueden no ajustarse debido a un tamano de muestra reducido."))
    base::message(c(" Nota: los acentos fueron removidos intencionalmente."))
    res.mod <-DHARMa::simulateResiduals(Modelo,n = 250)
    return(plot(res.mod, rank = T))
  } else if(is.numeric(nsim)){
    options(encoding = "UTF-8")
    n.sim<- nsim

    base::message(c("Si Kolmogorov-Smirnov test (KS test) p < 0.05, entonces no hay normalidad en los residuales. Sin embargo esta prueba es muy sensible al tamano de muestra, con tamano de muestra grande el valor de p tiene a < 0.05, pero la distribucion se apoxima mucho a la normal. Se recomienda ser un poco flexible con esta prueba al momento de examinar los residuales (examinar visualmente). Con tamanos de muestra grandes = 500 el Teorema del Limite Central garantiza que los coeficientes de regresion se distribuyen normalmente en promedio (Ali & Sharma, 1996, Lumley et al., 2002)."))

    base::message(c("Outlier test: p < 0.05, no hay outliers. En caso de haber outliers, usar la funcion outliers.DHARMa para saber cuales son los datos influyentes."))
    base::message(c("Dispersion test: p < 0.05, Indica problemas de sub/sobredispersion. En caso de haber problemas se recomienda ajustar el parametro dispformula, solo para la paqueteria glmmTMB."))
    base::message(c("Grafico de la derecha muestra la distribucion esperada de los resiudales, mediante simulaciones. Esto es util cuando no sabemos cual es la distribucion nula de los residuales. El grafico de la derecha muestra los residuales contra los valores esperados (ajustados). Estas lineas pueden no ajustarse debido a un tamano de muestra reducido."))
    base::message(c(" Nota: los acentos fueron removidos intencionalmente."))
    base::message(c(paste("Se usaron ", nsim, "simulaciones.")))
    res.mod <-DHARMa::simulateResiduals(Modelo,n = n.sim)
    return(plot(res.mod, rank = T))

  }


}



#' Grafico complementario de residuales de un modelo LM, LMM, GLM, GLMM, GAM, etc
#'
#' Esta es una función complementaria de la función resid.DHARMa. Muestra el ajuste del modelo LM, GLM, GLMM
#' , GAM, etc. Esta función crea residuos escalados
#' mediante la simulación del modelo ajustado. Los residuos pueden ser extraídos con residuals.DHARMa. Ver
#' testResiduals para una visión general de las pruebas de residuos, plot.DHARMa para una visión general de
#' los gráficos disponibles. Esta función fue creada para explorar el ajuste, normalidad y homogeneidad de
#' varianzas entre los grupos.
#' @param Modelo Un modelo LM, GLM, GLMM, GAM,
#'
#' @return Grafica los residuales de un modelo.
#' @export
#'
#' @examples
#' datos <- datasets::ChickWeight
#' library(lme4)
#' modelo <- glmer(weight ~ Diet +(1|Chick), family=gaussian("log"), data = datos)
#' resid_DHARMa2(modelo)
#' @encoding UTF-8
#' @importFrom DHARMa simulateResiduals
#' @importFrom DHARMa testOutliers
resid_DHARMa2 <- function(Modelo){
  dev.new()
  res <- DHARMa::simulateResiduals(fittedModel = Modelo, n = 1000)
  par(mfrow=c(1,2))

  message(c("Para saber si hay outliers, Outlier test: p > 0.05, no hay outliers:"))
  #outlier test
  x<-DHARMa::testOutliers(res,plot = T)  #
  print(x)
  # zero inflado
  message(c("Para saber si hay evidencia de posible effecto inflado por exceso de ceros. p > 0.05 no hay efecto:"))

  print(DHARMa::testZeroInflation(res))



}



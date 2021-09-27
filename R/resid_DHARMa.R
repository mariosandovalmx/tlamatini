#' Grafico de residuales de un modelo LM, LMM, GLM, GLMM, GAM, etc
#'
#' Muestra el ajuste del modelo LM, GLM, GLMM, GAM, etc. Esta función crea residuos escalados
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
#' #datos <- datasets::ChickWeight
#' #library(lme4)
#' #modelo <- glmer(weight ~ Diet +(1|Chick), family=gaussian("log"), data = datos)
#' #resid_DHARMa(modelo)
#' @encoding UTF-8
#' @importFrom DHARMa simulateResiduals
resid_DHARMa <- function(Modelo){

  res.mod <-DHARMa::simulateResiduals(Modelo,n = 20000)

  base::message(c("Kolmogorov-Smirnov test (KS test) p < 0.05, entonces no hay normalidad en los residuales. Outlier test: p < 0.05, no hay outliers. Grafico de la derecha muestra la distribucin esperada de los resiudales, mediante simulaciones. Esto es util cuando no sabemos cual es la distribucion nula de los residuales. El grafico de la derecha muestra los residuales contra los valores esperados (ajustados). Estas lineas pueden no ajustarse debido a un tamano de muestra reducido."))

  dev.new()
  plot(res.mod, rank = T)
}


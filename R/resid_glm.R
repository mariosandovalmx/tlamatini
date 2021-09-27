#' Grafico de residuales de un modelo GLM
#'
#' Muestra el ajuste del modelo LM, GLM. Esta función permite la exploración visual de los
#' residuales de un modelo.
#' @param Modelo Un modelo LM, GLM.
#'
#' @return Esta función fue creada para explorar el ajuste, normalidad y homogeneidad de varianzas de un modelo GLM.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length, family = gaussian("log"), data=iris)
#' resid_glm(modelo)
#' @encoding UTF-8
#' @importFrom grDevices dev.new
resid_glm <- function(Modelo){
  dev.new()
  par(mfrow=c(2,2))
  plot(Modelo)

}


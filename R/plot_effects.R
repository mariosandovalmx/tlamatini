#' Graficar effectos de un LM, LMM, GLM, GLMM
#'
#' Graficar effectos de un LM, LMM, GLM, GLMM, etc. Permite hacer una observación rápida de las variables explicativas en el modelo y su influencia en la variable de respuesta. Gráficos generados con paqueteria "effects".
#' @param modelo Modelo LM, GLM, GLMM, etc.
#'
#' @return Grafica todas las variables explicativas del modelo.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length*Species+ Sepal.Length, family = gaussian("log"), data=iris)
#' plot_effects(modelo)
#' @encoding UTF-8
#' @importFrom effects allEffects
#' @importFrom graphics plot
plot_effects<- function(modelo){

  alleff<- effects::allEffects(modelo)
  ###
  #indx <- base::grepl(':', names(alleff))
  #graphics::plot(alleff[indx])
  graphics::plot(alleff)


}



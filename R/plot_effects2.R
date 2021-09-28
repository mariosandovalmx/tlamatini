#' Graficar effectos de un LM, LMM, GLM, GLMM
#'
#' Graficar effectos de un LM, LMM, GLM, GLMM, etc. Permite hacer una observación rápida de las variables. Gráficos generados con paqueteria "ggeffects".
#' explicativas en el modelo y su influencia en la variable de respuesta.
#' @param modelo Modelo LM, GLM, GLMM, etc.
#'
#' @return Grafica todas las variables explicativas del modelo.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length*Species+ Sepal.Length, family = gaussian("log"), data=iris)
#' plot_effects2(modelo)
#' @encoding UTF-8
#' @importFrom ggeffects ggeffect
#' @importFrom ggpubr ggarrange
#' @importFrom graphics plot
plot_effects2<- function(modelo) {



  #graficar efectos paqueteria ggeffects
  dat<- ggeffects::ggeffect(modelo)

  # graficar los efectos y agruparlos en una sola grafica ggpubr

  graphs <- lapply(names(dat),function(x){
    graphics::plot(dat[[x]],connect.lines = TRUE)

  })
  do.call(ggpubr::ggarrange,graphs)
}






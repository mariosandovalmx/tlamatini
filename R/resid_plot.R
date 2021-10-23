#' Grafica de residuales de un modelo. Version 2
#'
#' Muestra el ajuste del modelo LM, GLM o GLMM. Esta función permite la exploración visual de los
#' residuales de un modelo.
#' @param Modelo Un modelo LM, GLM o GLMM.
#'
#' @return Esta función fue creada para explorar el ajuste, normalidad y homogeneidad de varianzas entre
#' los grupos.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length, family = gaussian("log"), data=iris)
#' resid_plot(modelo)
#' @encoding UTF-8
#' @importFrom sjPlot plot_model
#' @importFrom ggpubr ggarrange
resid_plot <- function(Modelo){
  dev.new()
  plot_list<- sjPlot::plot_model(Modelo, type = "diag")

  x= length(plot_list)
  cols=round(sqrt(x),0)
  rows=ceiling(x/cols)

 return(ggpubr::ggarrange(plotlist = plot_list, ncol = cols, nrow = rows))


}


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

  plot_list<- sjPlot::plot_model(Modelo, type = "diag")
  x= length(plot_list)
  nm<- as.character(seq(1:x))

  names(plot_list) <- nm

  graphs <- lapply(names(plot_list),function(x){
    return(plot_list[[x]])
  })
  do.call(ggpubr::ggarrange,graphs)

}


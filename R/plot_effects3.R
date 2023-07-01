#' Graficar effectos de un LM, LMM, GLM, GLMM
#'
#' Graficar effectos de un LM, LMM, GLM, GLMM, etc. Permite hacer una observación rápida de las variables. Gráficos generados con paqueteria "Sjplot".
#' explicativas en el modelo y su influencia en la variable de respuesta.
#' @param modelo Modelo LM, GLM, GLMM, etc.
#' @param show.data Argumento logico TRUE o FALSE, que indica si se agregan los puntos de las observaciones.
#'
#' @return Grafica todas las variables explicativas del modelo.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length+Species, family = gaussian("log"), data=iris)
#' plot_effects3(modelo)
#' #mostrando los puntos de datos
#' plot_effects3(modelo, show.data = TRUE)
#' #no mostrando los puntos de datos
#' plot_effects3(modelo, show.data = FALSE)

#' @encoding UTF-8
#' @importFrom sjPlot set_theme
#' @importFrom sjPlot plot_model
#' @importFrom sjPlot plot_grid
#' @importFrom ggplot2 theme_classic

plot_effects3<- function(modelo, show.data=NULL) {

  if(is.null(show.data)){

    sjPlot::set_theme(base = ggplot2::theme_classic(),axis.textsize.x = 1,axis.textsize.y = 1,axis.title.color="black",title.color = "white")
    p.eff<- sjPlot::plot_model(modelo, type="eff",colors = "bw")
    return(sjPlot::plot_grid(p.eff,tags=TRUE))
  } else if(isTRUE(show.data)){

    sjPlot::set_theme(base = ggplot2::theme_classic(),axis.textsize.x = 1,axis.textsize.y = 1,axis.title.color="black",title.color = "white")
    p.eff<- sjPlot::plot_model(modelo, type="eff",colors = "bw",show.data = TRUE, jitter = 0.05)
    return(sjPlot::plot_grid(p.eff,tags=TRUE))
  } else{
    sjPlot::set_theme(base = ggplot2::theme_classic(),axis.textsize.x = 1,axis.textsize.y = 1,axis.title.color="black",title.color = "white")
    p.eff<- sjPlot::plot_model(modelo, type="eff",colors = "bw")
    return(sjPlot::plot_grid(p.eff,tags=TRUE))


  }

}

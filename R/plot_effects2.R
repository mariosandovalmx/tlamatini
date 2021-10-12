#' Graficar effectos de un LM, LMM, GLM, GLMM
#'
#' Graficar effectos de un LM, LMM, GLM, GLMM, etc. Permite hacer una observación rápida de las variables. Gráficos generados con paqueteria "ggeffects".
#' explicativas en el modelo y su influencia en la variable de respuesta.
#' @param modelo Modelo LM, GLM, GLMM, etc.
#' @param lineas Argumento logico TRUE o FALSE, que indica si se agregan las lineas o no.
#' @param puntos Argumento logico TRUE o FALSE, que indica si se agregan los puntos o no.
#'
#' @return Grafica todas las variables explicativas del modelo.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length*Species+ Sepal.Length, family = gaussian("log"), data=iris)
#' plot_effects2(modelo, lineas = TRUE, puntos=TRUE)
#' #plot_effects2(modelo, lineas = FALSE, puntos=FALSE)
#' #plot_effects2(modelo, lineas = TRUE, puntos=FALSE)
#' #plot_effects2(modelo, lineas = FALSE, puntos=TRUE)

#' @encoding UTF-8
#' @importFrom ggeffects ggeffect
#' @importFrom ggpubr ggarrange
#' @importFrom graphics plot
plot_effects2<- function(modelo, lineas, puntos ) {

  if( lineas == T & puntos ==  T ){

    #graficar efectos paqueteria ggeffects
    dat<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs <- lapply(names(dat),function(x){
      graphics::plot(dat[[x]], connect.lines = TRUE, add.data=TRUE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1)


    })
    do.call(ggpubr::ggarrange,graphs)
  } else if( lineas ==  F & puntos == F) {

    #graficar efectos paqueteria ggeffects
    dat2<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs2 <- lapply(names(dat2),function(x){
      graphics::plot(dat2[[x]],connect.lines = FALSE, add.data=FALSE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1)


    })
    do.call(ggpubr::ggarrange,graphs2)

  } else if( lineas == T & puntos == F) {

    #graficar efectos paqueteria ggeffects
    dat2<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs2 <- lapply(names(dat2),function(x){
      graphics::plot(dat2[[x]],connect.lines = TRUE, add.data=FALSE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1)


    })
    do.call(ggpubr::ggarrange,graphs2)

  } else if( lineas == F & puntos == T) {

    #graficar efectos paqueteria ggeffects
    dat2<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs2 <- lapply(names(dat2),function(x){
      graphics::plot(dat2[[x]],connect.lines = FALSE, add.data=TRUE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1)


    })
    do.call(ggpubr::ggarrange,graphs2)

  }

}

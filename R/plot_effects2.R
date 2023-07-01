#' Graficar effectos de un LM, LMM, GLM, GLMM
#'
#' Graficar effectos de un LM, LMM, GLM, GLMM, etc. Permite hacer una observación rápida de las variables. Gráficos generados con paqueteria "ggeffects".
#' explicativas en el modelo y su influencia en la variable de respuesta.
#' @param modelo Modelo LM, GLM, GLMM, etc.
#' @param lineas Argumento logico TRUE o FALSE, que indica si se agregan las lineas o no.
#' @param puntos Argumento logico TRUE o FALSE, que indica si se agregan los puntos o no.
#' @param grids  Si se especifica F o FALSE, se eliminan las rejillas del grafico.
#'
#' @return Grafica todas las variables explicativas del modelo.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length+Species, family = gaussian("log"), data=iris)
#' plot_effects2(modelo)
#' #grafico con lineas y puntos
#' plot_effects2(modelo, lineas = TRUE, puntos=TRUE)
#' #grafico con sin rejillas
#' plot_effects2(modelo, grids = FALSE)
#' plot_effects2(modelo, lineas = TRUE, puntos = TRUE, grids = FALSE)
#' @encoding UTF-8
#' @importFrom ggeffects ggeffect
#' @importFrom ggpubr ggarrange
#' @importFrom graphics plot
#' @importFrom ggpubr grids
plot_effects2<- function(modelo, lineas=NULL, puntos=NULL, grids=NULL) {
  #### grafico default sin puntos ni lineas con grids
  if(is.null(lineas) && is.null(puntos) && is.null(grids)){
    th<- ggplot2::theme(axis.text=ggplot2::element_text(size=15,color="black"),axis.title=ggplot2::element_text(size=15,face="bold",color="black"))+ ggplot2::theme_light(base_size = 17)+ ggpubr::grids(linetype = "dashed", color = "grey85")
    #graficar efectos paqueteria ggeffects
    dat<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs <- lapply(names(dat),function(x){
      graphics::plot(dat[[x]], connect.lines = FALSE, add.data=FALSE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1, show.title = FALSE)+th


    })
    do.call(ggpubr::ggarrange,graphs)
    ### grafico sin grids ni puntos y lineas
  }  else if(is.null(lineas) && is.null(puntos) && grids==F){
    th<- ggplot2::theme(axis.text=ggplot2::element_text(size=15,color="black"),axis.title=ggplot2::element_text(size=15,face="bold",color="black"))+ ggplot2::theme_light(base_size = 17)+ ggpubr::grids(linetype = "dashed", color = "white")
    #graficar efectos paqueteria ggeffects
    dat<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs <- lapply(names(dat),function(x){
      graphics::plot(dat[[x]], connect.lines = FALSE, add.data=FALSE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1, show.title = FALSE)+th


    })
    do.call(ggpubr::ggarrange,graphs)
    # grafico con lineas y puntos con grids
  } else if( lineas == T && puntos ==  T && is.null(grids)){
    th<- ggplot2::theme(axis.text=ggplot2::element_text(size=15,color="black"),axis.title=ggplot2::element_text(size=15,face="bold",color="black"))+ ggplot2::theme_light(base_size = 17)+ ggpubr::grids(linetype = "dashed", color = "grey85")

    #graficar efectos paqueteria ggeffects
    dat<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs <- lapply(names(dat),function(x){
      graphics::plot(dat[[x]], connect.lines = TRUE, add.data=TRUE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1, show.title = FALSE)+th


    })
    do.call(ggpubr::ggarrange,graphs)
    #grafico con lineas y puntos sin grids
  } else if(lineas == T && puntos ==  T   && grids==F){
    th<- ggplot2::theme(axis.text=ggplot2::element_text(size=15,color="black"),axis.title=ggplot2::element_text(size=15,face="bold",color="black"))+ ggplot2::theme_light(base_size = 17) + ggpubr::grids(linetype = "dashed", color = "white")
    #graficar efectos paqueteria ggeffects
    dat<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs <- lapply(names(dat),function(x){
      graphics::plot(dat[[x]], connect.lines = TRUE, add.data=TRUE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1, show.title = FALSE)+th


    })
    do.call(ggpubr::ggarrange,graphs)
    #grafico sin lineas ni puntos con grid
  } else if( lineas ==  F && puntos == F && is.null(grids)) {
    th<- ggplot2::theme(axis.text=ggplot2::element_text(size=15,color="black"),axis.title=ggplot2::element_text(size=15,face="bold",color="black"))+ ggplot2::theme_light(base_size = 17)+ ggpubr::grids(linetype = "dashed", color = "grey85")
    #graficar efectos paqueteria ggeffects
    dat2<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs2 <- lapply(names(dat2),function(x){
      graphics::plot(dat2[[x]],connect.lines = FALSE, add.data=FALSE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1, show.title = FALSE)+th


    })
    do.call(ggpubr::ggarrange,graphs2)
    #grafico solo con lineas sin puntos y con grid
  } else if( lineas ==  F && puntos == F && grids==F) {
    th<- ggplot2::theme(axis.text=ggplot2::element_text(size=15,color="black"),axis.title=ggplot2::element_text(size=15,face="bold",color="black"))+ ggplot2::theme_light(base_size = 17)+ ggpubr::grids(linetype = "dashed", color = "white")
    #graficar efectos paqueteria ggeffects
    dat2<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs2 <- lapply(names(dat2),function(x){
      graphics::plot(dat2[[x]],connect.lines = FALSE, add.data=FALSE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1, show.title = FALSE)+th


    })
    do.call(ggpubr::ggarrange,graphs2)
    #grafico solo con lineas sin puntos y con grid
  }  else if( lineas == T && puntos == F && is.null(grids)) {
    th<- ggplot2::theme(axis.text=ggplot2::element_text(size=15,color="black"),axis.title=ggplot2::element_text(size=15,face="bold",color="black"))+ ggplot2::theme_light(base_size = 17)+ ggpubr::grids(linetype = "dashed", color = "grey85")
    #graficar efectos paqueteria ggeffects
    dat2<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs2 <- lapply(names(dat2),function(x){
      graphics::plot(dat2[[x]],connect.lines = TRUE, add.data=FALSE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1, show.title = FALSE) + th


    })
    do.call(ggpubr::ggarrange,graphs2)
    #grafico con lineas sin puntos y sin grids
  }  else if( lineas == T && puntos == F && grids==F) {
    th<- ggplot2::theme(axis.text=ggplot2::element_text(size=15,color="black"),axis.title=ggplot2::element_text(size=15,face="bold",color="black"))+ ggplot2::theme_light(base_size = 17)+ ggpubr::grids(linetype = "dashed", color = "white")
    #graficar efectos paqueteria ggeffects
    dat2<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs2 <- lapply(names(dat2),function(x){
      graphics::plot(dat2[[x]],connect.lines = TRUE, add.data=FALSE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1, show.title = FALSE) + th


    })
    do.call(ggpubr::ggarrange,graphs2)
    #grafico sin lineas con puntos y con grids
  } else if( lineas == F && puntos == T && is.null(grids)) {
    th<- ggplot2::theme(axis.text=ggplot2::element_text(size=15,color="black"),axis.title=ggplot2::element_text(size=15,face="bold",color="black"))+ ggplot2::theme_light(base_size = 17)+ ggpubr::grids(linetype = "dashed", color = "grey85")
    #graficar efectos paqueteria ggeffects
    dat2<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs2 <- lapply(names(dat2),function(x){
      graphics::plot(dat2[[x]],connect.lines = FALSE, add.data=TRUE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1, show.title = FALSE) + th


    })
    do.call(ggpubr::ggarrange,graphs2)
    #grafico sin lineas con puntos y sin grids
  } else if( lineas == F && puntos == T && grids==F) {
    th<- ggplot2::theme(axis.text=ggplot2::element_text(size=15,color="black"),axis.title=ggplot2::element_text(size=15,face="bold",color="black"))+ ggplot2::theme_light(base_size = 17)+ ggpubr::grids(linetype = "dashed", color = "white")
    #graficar efectos paqueteria ggeffects
    dat2<- ggeffects::ggeffect(modelo)

    # graficar los efectos y agruparlos en una sola grafica ggpubr

    graphs2 <- lapply(names(dat2),function(x){
      graphics::plot(dat2[[x]],connect.lines = FALSE, add.data=TRUE, colors = "bw", alpha = 0.15, dot.alpha = 0.15,jitter = 0.2,line.size = 1, show.title = FALSE) + th


    })
    do.call(ggpubr::ggarrange,graphs2)

  }

}

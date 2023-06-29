#' Tema con valores predeterminados minimalistas (en mi opinion) adecuados para la publicacion
#'
#' Esta función permite usarse como tema predefinido de ggplot para una publiacion científica o para una presentación. Anteriormente esta función estaba con el nombre "my_ggplot_themes".
#'
#' @inheritParams ggplot2::theme_bw
#' @importFrom ggplot2 theme_bw element_line element_rect element_text
#' @importFrom grid unit
#' @export
#' @examples
#' library(ggplot2)
#'
#' d = data.frame(
#'   x = 1:90,
#'   y = rnorm(90),
#'   red = rep(letters[1:3], 30),
#'   blue = c(rep(1, 30), rep(2, 30), rep(3, 30)))
#'
#' p <- ggplot(d) +
#'   geom_point(aes(x = x, y = y)) +
#'   facet_grid(red ~ blue)
#' p + tema_articulo()
#' p + tema_articulo(axis.size=14, axis.title.size = 14)
#' # axis.size y axis.title.size solo funcionan dentro de "tema_articulo()"
#' p + tema_articulo2()
#' p + tema_presentacion()
#'
tema_articulo2 <- function(base_size = 12, base_family = "") {
  gray <- "#464646"
  fg <- "#000000"
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(line = element_line(colour = gray),
          rect = element_rect(fill = NA, colour = NA),
          text = element_text(colour = fg),
          axis.ticks = element_line(colour = gray),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.key.height = unit(3,"mm"),
          panel.border = element_rect(colour = gray, fill = NA),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          panel.background=element_blank(),
          strip.background = element_blank())
}

#' Tema con valores predeterminados minimalistas (en mi opinion) adecuados para una presentacion
#'
#' .
#'
#' @inheritParams ggplot2::theme_bw
#' @importFrom ggplot2 theme_bw element_line element_rect element_text
#' @importFrom grid unit
#' @export
tema_presentacion <- function(base_size = 24, base_family = "") {
  gray <- "#464646"
  bg = 'grey98'
  fg <- "#000000"
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(line = element_line(colour = gray),
          rect = element_rect(fill = NA, colour = NA),
          text = element_text(colour = fg),
          axis.ticks = element_line(colour = gray),
          legend.key = element_rect(colour = NA, fill = bg),
          legend.key.height = unit(3,"mm"),
          panel.border = element_rect(colour = gray, fill = NA),
          panel.grid = element_blank(),
          panel.background=element_rect(fill=bg),
          strip.background = element_blank())
}

#' #' Tema 1 de ggplot predefinidos por mi para publicaciones
#'
#' Usa parámetros de ggplot predefinidos para una publicación científica, se basa en mis criterios y gustos
#' personales. Se puede especificar tamaño de los ejes X y Y, negritas y sin fondo.
#'
#' @param axis.size Tamaño de los ejes.
#' @param axis.title.size Tamaño de la leyenda del eje.
#'
#' @return Un grafico de ggplot con el tema predefinido.
#'
#' @export
#' @examples
#' #Para usar el tema predefinido
#' #require(ggplot2)
#' #data(iris)
#' #ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) +
#' #geom_point() +
#' #geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
#' #tema_articulo(axis.size= 10, axis.title.size= 12)
#' @encoding UTF-8
#' @import ggplot2

tema_articulo <-  function( axis.size=NULL, axis.title.size = NULL){

  if(is.null(axis.size) & is.null(axis.title.size)){

    tema_articulo  <- theme_classic() + theme(legend.position = "top", legend.text=element_text(size=12),title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=axis.size, color="black"),axis.title=element_text(size=axis.title.size,face="bold",color="black"))

  } else {

    tema_articulo  <- theme_classic() + theme(legend.position = "top", legend.text=element_text(size=12),title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=axis.size, color="black"),axis.title=element_text(size=axis.title.size,face="bold",color="black"))


  }
}

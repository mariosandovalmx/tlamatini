#' Tema 1 de ggplot predefinidos por mi para publicaciones
#'
#' Usa parámetros de ggplot predefinidos para una publicación científica, se basa en mis criterios y gustos
#' personales. Se puede especificar tamaño de los ejes X y Y, negritas y sin fondo.
#'
#' @param axis.size Tamaño de los ejes, obligatorio
#' @param legend.size Tamaño de la leyenda, obligatorio
#'
#' @return Un grafico de ggplot con el tema predefinido.
#'
#' @export
#'
#' @encoding UTF-8
#' @import ggplot2
#' @importFrom ggpubr grids
my_theme_ggplot3<-  function( axis.size=NULL,  legend.size=NULL){
  my_theme_ggplot  <- ggplot2::theme(axis.text=ggplot2::element_text(size=15,color="black"),axis.title=ggplot2::element_text(size=15,face="bold",color="black"))+ ggplot2::theme_light(base_size = 17)+ ggpubr::grids(linetype = "dashed", color = "grey85")
}

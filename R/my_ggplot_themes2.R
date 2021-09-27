#' Tema2  de ggplot predefinidos por mi para publicaciones
#'
#' Usa parámetros de ggplot predefinidos para una publicación científica, se basa en mis criterios y gustos
#' personales. solo ejes y letras grandes.
#'
#' @param axis.size Tamaño de los ejes
#' @param legend.size Tamaño de la leyenda
#'
#' @return Un grafico de ggplot con el tema predefinido.
#'
#' @export
#'
#' @encoding UTF-8
#' @import ggplot2
my_theme_ggplot2<-  function( axis.size=NULL,  legend.size=NULL){
  my_theme_ggplot_2 <- ggplot2::theme_classic() + ggplot2::theme(legend.position = "top",legend.text=ggplot2::element_text(size=13),title = ggplot2::element_blank(),panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), axis.text=ggplot2::element_text(size=15,color="black"),axis.title=element_text(size=14,face="bold",color="black"))

}

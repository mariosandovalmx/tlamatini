#' Tema 1 de ggplot predefinidos por mi para publicaciones
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
#' #my_theme_ggplot(axis.size= 10, axis.title.size= 12)
#' @encoding UTF-8
#' @import ggplot2

my_theme_ggplot<-  function( axis.size=NULL, axis.title.size = NULL){

  if(is.null(axis.size) & is.null(axis.title.size)){

  my_theme_ggplot  <- ggplot2::theme_classic() + ggplot2::theme(legend.position = "top", legend.text=ggplot2::element_text(size=12),title = ggplot2::element_blank(),panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), axis.text=ggplot2::element_text(size=axis.size, color="black"),axis.title=element_text(size=axis.title.size,face="bold",color="black"))

  } else {

    my_theme_ggplot  <- ggplot2::theme_classic() + ggplot2::theme(legend.position = "top", legend.text=ggplot2::element_text(size=12),title = ggplot2::element_blank(),panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), axis.text=ggplot2::element_text(size=axis.size, color="black"),axis.title=element_text(size=axis.title.size,face="bold",color="black"))


  }
}

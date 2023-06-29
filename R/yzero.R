#' Hacer cero absoulto en eje Y de ggplot
#'
#' Cambia el cero en formato "0.00" mostrado en el eje Y por un "0" visualmente mas estetico.Se usa como un argumento adicional de ggplot.
#'
#'
#' @return Un grafico de ggplot con el tema predefinido.
#'
#' @export
#' @examples
#' #Para usar el tema predefinido
#' #library(ggplot)
#' #data(iris)
#' #ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) +
#' #geom_point() +
#' #geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
#' #yzero()
#'
#' @encoding UTF-8
#' @import ggplot2
#' @importFrom stringr str_extract
yzero<-  function(){
  prettyZero <- function(l){
    max.decimals = max(nchar(stringr::str_extract(l, "\\.[0-9]+")), na.rm = T)-1
    lnew = formatC(l, replace.zero = T, zero.print = "0",
                   digits = max.decimals, format = "f", preserve.width=T)
    return(lnew) }

  yzero<- ggplot2::scale_y_continuous(labels = prettyZero)


}


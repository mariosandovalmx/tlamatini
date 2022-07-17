#' Sustituir todos los NA por ceros.
#'
#' Sustituir todos los NA por ceros.
#' @param x Vector o columna
#'
#' @return Na por cualquier valor en un vector o columna.
#' @export
#'
#' @examples
#' iris$Sepal.Length[c(1,3,5,7,9)] <- NA
#' iris.sinNA<- as.data.frame(lapply(iris,NA.cero))
#' iris.sinNA$Sepal.Length
#' @encoding UTF-8
NA.cero=function(x){
  x2<- ifelse(is.na(x),0,as.numeric(x))
  return(x2)
  insight::print_color("Na's convertidos a ceros.", "green")
  }

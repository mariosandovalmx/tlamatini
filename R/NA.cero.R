#' Sustituir todos los NA por ceros.
#'
#' Sustituir todos los NA por ceros.
#' @param x Vector o columna
#'
#' @return Na por cualquier valor en un vector o columna.
#' @export
#'
#' @examples
#' #Introducimos NAs aleatorios en el dataframe
#' iris$Sepal.Length[c(1,3,5,7,9)] <- NA
#' head(iris)
#' #ahora sustituimos por ceros
#' iris.sinNA<- as.data.frame(lapply(iris,NA.cero))
#' head(iris)
#' @encoding UTF-8
NA.cero=function(x){
  x2<- ifelse(is.na(x),0,as.numeric(x))
  return(x2)
  insight::print_color("Na's convertidos a ceros.", "green")
  }

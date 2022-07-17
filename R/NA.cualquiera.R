#' Sustituir los NA de un vector por cualquier valor deseado
#'
#' Sustituir los NA de un vector por cualquier valor deseado, puede ser numérico o caracter.
#' @param x Columna o vector numérico o tipo caracter.
#' @param reemplazo Cualquier valor deseado que ser? usado para sustituir los NA.
#'
#' @return Na por cualquier valor en un vector o columna.
#' @export
#'
#' @examples
#' data(iris)
#' iris$Sepal.Width[c(1,3,5,7)] <- NA
#' iris<-NA.cualquiera(iris,c(22.22))
#' head(iris)
#' @encoding UTF-8
NA.cualquiera <- function(x, reemplazo) {
  is_miss <- is.na(x)
  x[is_miss] <- reemplazo
  return(x)
  message(sum(is_miss), " NA sustituidos por el valor ", reemplazo)
}



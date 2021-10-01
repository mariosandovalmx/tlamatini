#' Eliminar los NA de una columna o varias columnas a la vez
#'
#' Quitar NA de una sola columna o de varias columnas a la vez.
#' @param data Dataframe.
#' @param columnas Columnas de las cuales se van a remover los NA.
#'
#' @return Un vector sin NA.
#' @export
#'
#' @examples
#' data("iris")
#' iris$Sepal.Width[c(1,3,5,7,9)] <- NA
#' iris$Petal.Width[c(1,3,5,7,9)] <- NA
#' iris2<- NA.quitar(iris, c("Petal.Width","Sepal.Width"))
#' iris3<- NA.quitar(iris, "Sepal.Width")
#' @encoding UTF-8
#' @importFrom stats complete.cases
NA.quitar <- function(data, columnas) {
  completeVec <- complete.cases(data[, columnas])
  return(data[completeVec, ])
}

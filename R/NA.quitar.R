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
#' #Introducimos NAs aleatorios en el dataframe
#' iris$Sepal.Width[c(1,3,5,7,9)] <- NA
#' iris$Petal.Width[c(2,5,6,7,9)] <- NA
#' head(iris)
#' #quitamos la columna Sepal.Width con NA
#' iris.sinNA2 <- NA.quitar(iris, "Sepal.Width")
#' head(iris.sinNA2)
#' #quitamos las columnas con NA
#' iris.sinNA3 <- NA.quitar(iris, c("Petal.Width","Sepal.Width"))
#' head(iris.sinNA3)
#' @encoding UTF-8
#' @importFrom stats complete.cases
NA.quitar <- function(data, columnas) {
  completeVec <- complete.cases(data[, columnas])
  insight::print_color("Na removidos con \u00e9xito", "green")
  return(data[completeVec, ])
}

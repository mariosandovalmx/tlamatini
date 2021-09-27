#' ggpairs_dfnum
#'
#' Matriz de correlación basada en ggpairs con lineas de regresion lineal y no lineal. Matriz de correlación #' de variables numéricas. Esta función selecciona solo las variables numéricas y aplica la función ggpairs
#' de la paqueteria GGally.
#' @param dataframe Dataframe
#'
#' @return Un gráfico de correlación con valores de correlación, valores de p y lineas de
#' regresión lineal y no lineal de cada relación.
#' @export
#'
#' @examples
#' ggpairs_dfnum(iris)
#' @encoding UTF-8
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom GGally ggpairs
#' @importFrom stats lm
ggpairs_dfnum <- function(dataframe){

  num       <- vector(mode = "character")
  char      <- vector(mode = "character")
  for (var in 1:ncol(dataframe)) {
    if (class(dataframe[[var]])=="numeric" || class(dataframe[[var]])=="integer") {
      num   <- c(num,names(dataframe[var]))
    }else if (class(dataframe[[var]])=="factor" || class(dataframe[[var]])=="character") {
      char  <- c(char,names(dataframe[var]))
    }
  }
  dfnum     <- dataframe[, c(num)]
  D         <- sapply(dfnum, function(x) as.numeric(x,na.rm=TRUE))
  DD        <- as.data.frame(D)

  ### Si calculamos lo mismo pero a?adimos una linea de regresi?n lineal y una no lineal
  ln.function <- function(data, mapping, ...){
    p <- ggplot2::ggplot(data = data, mapping = mapping) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method=loess, fill="red", color="red", ...) +
      ggplot2::geom_smooth(method=lm, fill="blue", color="blue", ...)
  }

  #obtenemos lo siguiente:
  g =  GGally::ggpairs(DD, lower = list(continuous = ln.function)) + ggplot2::theme_minimal()
  g

}

ggplot2::theme_minimal()

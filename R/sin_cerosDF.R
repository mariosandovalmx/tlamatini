#' Quitar todos los ceros de un dataframe, para quedarnos con observaciones completas
#'
#'  Quitar todos los ceros del dataframe y quedarse solo con las filas con observaciones completas.
#' @param dataframe Un dataframe variables.
#'
#' @return  quitar ceros del vector.
#' @export
#'
#' @examples
#' df<-sin_cerosDF(iris)
#' @encoding UTF-8
#' @importFrom stats complete.cases
sin_cerosDF <- function(dataframe){
  data.sc<- dataframe
  data.sc[data.sc==0] <- NA
  data2.sc<-data.sc[complete.cases(data.sc),]
  print(data2.sc)
}



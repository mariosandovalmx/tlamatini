#' Convertir en 0 y 1 un vector numerico
#'
#' Permite convertir cualquier variable numerica en variable binomial de 0 y 1, presencia-ausencia.
#' @param dataframe dataframe
#' @param variable variable numerica a transformar en binomial.
#'
#' @return vector transformado en 0 y 1.
#' @export
#'
#' @examples
#' #df <- trans.binomial(dataframe = data, variable = concentration)
#' @encoding UTF-8
#' @import dplyr
trans.binomial <-function(dataframe,variable){
   df <- dataframe %>%
     dplyr::mutate(variable = ifelse(variable == 0,0,1))
  print(df)
}


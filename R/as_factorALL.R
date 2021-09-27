#' Convertir a factor todas las variables tipo caracter de un dataframe
#'
#' Convierte a factor todas las columnas tipo caracter.
#' @param dataframe Dataframe
#'
#' @return Un dataframe con todas las variables tipo caracter convertidas a factor.
#' @export
#'
#' @examples
#' #iris2<- as_factorALL(iris)
#' @encoding UTF-8
as_factorALL<- function(dataframe){

  df<- dataframe
  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)],
                                         as.factor)
  return(df)
}



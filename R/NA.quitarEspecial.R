#' Reemplazar caracteres especiales por NA
#'
#' Reemplaza los caracteres especiales en el dataframe por NA.
#' @param df dataframe
#' @param vec vector que contiene los caracteres especiales que desea sustituir por NA
#'
#' @return Vector sin caracteres especiales.
#' @export
#'
#' @examples
#'data(iris)
#'iris[2,2]<-"@@"
#'iris[3,2]<-"???"
#'head(iris)
#'iris2<-NA.quitarEspecial(iris,c("@@","???"))
#'head(iris2)
#' @encoding UTF-8
NA.quitarEspecial<-function(df,vec){
  df[ df == "NaN" ] = NA
  df[ df == "<NA>" ] = NA
  df[ df == "?" ] = NA
  df[ df == "@" ] = NA
  df[ df== "" ] = NA
  df[ df == " " ] = NA
  df[ df == "N/A" ] = NA
  for (i in 1:length(vec)){df[ df == vec[i] ] =
    NA}
  return(df)
}





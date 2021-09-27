#' Estandarizar las variables de un dataframe, excluyendo algunas
#'
#' Estandariza las todas las variables de un dataframe mediante el centrado y el escalado. Ver stdize en MuMIn package para detalles.
#' @param df Un dataframe
#' @param ex_variables Variables a excluir del dataframe. Esto si no queremos aplicar la escalada a dichas variables.
#' @param center centrado
#' @param scale escalado
#'
#' @return vector escalado
#' @export
#'
#' @examples
#' data(iris)
#' df.sc <- scale_data(iris, ex_variables = c("Petal.Width","Petal.Length"))
#' @encoding UTF-8

scale_data <- function(df, ex_variables, center = TRUE, scale=TRUE){
  subset_colclasses <- function(df, colclasses="numeric") {
    df[,sapply(df, function(vec, test) class(vec) %in% test, test=colclasses)]
  }


  dfnum<- subset_colclasses(df, c("numeric","integer"))

  dfnum2<- as.data.frame(dfnum)
  dfnum2 <- select(dfnum, -ex_variables) # sin ciertas columnas

  dfnum3 <- select(dfnum, ex_variables)
  dfchar<- subset_colclasses(df, c("factor","character"))
  dfchar<- as.data.frame(dfchar)

  p <- MuMIn::stdize(dfnum2,omit.cols=ex_variables,prefix="",center = TRUE, scale=TRUE)
  df2 <- cbind(p,dfchar,dfnum3)
  return(df2)
}






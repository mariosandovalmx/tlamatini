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

  num       <- vector(mode = "character")
  char      <- vector(mode = "character")
  for (var in 1:ncol(df)) {
    if (class(df[[var]])=="numeric" || class(df[[var]])=="integer") {
      num   <- c(num,names(df[var]))
    }else if (class(df[[var]])=="factor" || class(df[[var]])=="character") {
      char  <- c(char,names(df[var]))
    }
  }

  dfnum     <- subset(df,select=num)
  dfchar     <- subset(df,select=char)

  p <- MuMIn::stdize(dfnum, omit.cols=ex_variables,prefix="",center = TRUE, scale=TRUE)
  df2 <- cbind(p, dfchar)
  return(df2)
}






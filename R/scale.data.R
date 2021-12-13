#' Estandarizar las variables de un dataframe
#'
#' Estandariza las todas las variables de un dataframe mediante el centrado y el escalado. Para cada valor de una
#' variable, simplemente restamos el valor medio de la variable, luego dividimos por la desviaci?n est?ndar de la
#' variable. Esto significa que vamos a escalar los valores de manera que la variable tenga una media de 0 y una
#' desviación est?ndar de 1.
#' Esto es ?til cuando las variables explicativas tienen diferentes escalas, y a menudo no contribuyen por igual al
#' modelo. Por ejemplo, si los valores de una variable oscilan entre 0 y 10000 y los valores de otra variable oscilan
#' entre 0 y 10, la variable con el intervalo m?s grande recibir? un mayor peso en el an?lisis. Al estandarizar las
#' variables, podemos estar seguros de que cada variable contribuye por igual al an?lisis. Este procedimiento no
#' representa una transformaci?n de los datos, y no cambia la magnitud del efecto ni la significancia de la variable en
#' el modelo. Ver la funci?n "stdize" de MuMIn package para m?s detalles.
#' @param df Un dataframe
#' @param ex_variables Variables a excluir del dataframe, si no queremos aplicar la escalada a dichas variables.
#' @param center centrado
#' @param scale escalado
#'
#' @return Dataframe escalado y centrado
#' @export
#'
#' @examples
#' data(iris)
#' df.sc <- scale_data(iris, ex_variables = c("Petal.Width","Petal.Length"))
#' # Si no se especifica que variables excluir, se aplicará a todas las columnas numericas del dataframe.
#' df.sc2 <- scale_data(iris)
#' @encoding UTF-8

scale_data <- function(df, ex_variables= NULL, center = TRUE, scale=TRUE){
  if(is.null(ex_variables)){
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

    p <- MuMIn::stdize(dfnum, omit.cols=NULL,prefix="",center = TRUE, scale=TRUE)
    df2 <- cbind(dfchar,p)
    return(df2)
  }else if(isTRUE(is.character(ex_variables))){
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
    df2 <- cbind(dfchar,p)
    return(df2)
  }

}

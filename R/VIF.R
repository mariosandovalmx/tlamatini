#' Obtener el VIF de un dataframe solo tomando en cuenta variables numéricas
#'
#' Calcule Factores de inflación de la variación (VIF) para estimar la multi-colinearidad
#' de las variables de un dataframe. Cuando al ajustar un modelo, arroja un error: Error in solve.default(as.matrix(fit$hessian)) : system is computationally singular: reciprocal condition number = 4.31555e-18 stepAIC). Se recomienda quitar las variables si el VIF es mayor a 10, o siendo más estrictos mayor a 3.
#' @param df Un dataframe.
#'
#' @return Lista de las variables y su correspondiente VIF.
#' @export
#'
#' @examples
#' VIF(iris)
#' VIF(mtcars)
#' @encoding UTF-8
#' @importFrom usdm vif

VIF <- function(df){
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

  dfnum2<- as.data.frame(dfnum)
  D         <- sapply(dfnum2, function(x) as.numeric(x,na.rm=TRUE))
  DD        <- as.data.frame(D)



  vif.table <- usdm::vif(DD)
  resultados <- print(vif.table)
}


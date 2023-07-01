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
VIF <- function (df) {
  num_cols <- unlist(lapply(df, is.numeric))         # Identify numeric columns
  num_cols
  dfnum <- df[ , num_cols]                        # Subset numeric columns of data

  VIF <- rep(NA, ncol(dfnum))
  names(VIF) <- colnames(dfnum)
  for (i in 1:ncol(dfnum)) {
    VIF[i] <- 1/(1 - summary(lm(dfnum[, i] ~ ., data = dfnum[-i]))$r.squared)
  }
  insight::print_color("# Comprobaci\u00f3n de la multicolinealidad\n", "blue")
  dfvif<- as.data.frame(VIF)
  return(dfvif)


}

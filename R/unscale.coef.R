#' Desescalar coeficientes previamente escalados y centrados.
#'
#' Permite desescalar y descentrar los coeficientes de un modelo ajustado con las variables
#' escaladas.
#' @param coef Coeficientes del modelo.
#' @param scaled_covariate variable escalada que contiene la media y la desviación estandar como atributos.
#'
#' @return coeficientes o medias reconvertidas en su escala original
#' @export
#'
#' @examples
#' #unscale.coef(-0.3440, using_scale)
#' #unscale.coef(coefs, df_scaled)
#' @encoding UTF-8

unscale.coef <- function(coef, scaled_covariate){

  # collect mean and standard deviation from scaled covariate
  mean_sd <- unlist(attributes(scaled_covariate)[-1])

  # reverse the z-transformation
  answer <- (coef * mean_sd[2]) + mean_sd[1]

  # this value will have a name, remove it
  names(answer) <- NULL

  # return unscaled coef
  return(answer)
}

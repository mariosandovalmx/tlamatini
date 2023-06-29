#' Desescalar coeficientes previamente escalados y centrados.
#'
#' Permite desescalar y descentrar los coeficientes de un modelo ajustado con las variables
#' escaladas. Esta funci?n tambien permite graficar las variables del eje X en su escala original.
#' @param coef Coeficientes del modelo.
#' @param scaled_covariate variable escalada que contiene la media y la desviaci?n estandar como atributos.
#'
#' @return coeficientes o medias reconvertidas en su escala original
#' @export
#'
#' @examples
#' #unscale.coef(-0.3440, using_scale)
#' #unscale.coef(coefs, df_scaled)
#' #
#' #
#' #Si lo que se quiere es graficar los efectos de un modelo en el que se
#' #usaron variables escaladas y se desea graficar en su escala original:
#' #Primero se escala la variable y se guarda en un objeto con
#' #los atributos de la media y SD
#' #alt.sc<- scale(DataR$Altitud, center = TRUE, scale = TRUE)
#' #library(ggeffects)
#' #dat<- ggeffect(m, c("Altitud", "Monta?a"))
#' #luego se retransforman los valores predichos por el modelo a su escala
#' #original usando la variable escalada.
#' #dat$x <- unscale.coef(dat$x, alt.sc)
#' #plot(dat)
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

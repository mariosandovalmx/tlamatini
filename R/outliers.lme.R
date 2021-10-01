#' Encontrar outliers de un modelo LMM
#'
#' Encontrar outliers de un LMM Linear mixed model de la paqueteria nlme, usa las distancias de
#' Cook.
#' @param modelo Modelo ajustado con la función lme de paqueteria nlme.
#' @param random Componente aleatorio del modelo a graficar.
#'
#' @return encontrar los outliers de un modelo lme.
#' @export
#'
#' @examples
#' #datos <- datasets::ChickWeight
#' #library(nlme)
#' #modelo <- lme(weight ~ Diet, random= ~1|Chick, data = datos)
#' #outliers.lme(modelo)
#' #outliers.lme(modelo, random = "Chick")
#' @encoding UTF-8
#' @importFrom predictmeans CookD
#' @importFrom nlme lme.formula

outliers.lme <- function(modelo, random = NULL){

  cook<- predictmeans::CookD(modelo,group = random)
  message("Outliers identificados:")
  which(cook > 0.25)

}


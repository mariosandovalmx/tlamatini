#' Transformar variable con logit
#'
#' Permite la transformación de logit aplicada a proporciones. Aplicar la transformación logit seguida de una regresión por mínimos cuadrados no lineales sobre  las variables transformadas (Warton & Hui, 2011). Este metodo se ha sugerido por mucho tiempo como un  método para tratar con proporciones/porcentajes. Ver: Warton, D. I., & Hui, F. K. C. (2011). The arcsine is asinine: The analysis of proportions in ecology. Ecology, 92(1), 3–10. https://doi.org/10.1890/10‐0340.1
#' @param p vector que contiene las proporciones en escalas de porcentaje de 0-100.
#'
#' @return vector transformado.
#' @export
#'
#' @examples
#' #variable <- seq(1,99, 3)
#' #trans.logit(variable)
#' @encoding UTF-8
#' @importFrom car logit
trans.logit <- function(p) {
  x <- car::logit(p, percents=TRUE)
  return(x)  }


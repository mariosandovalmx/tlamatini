#' Transformar variable con arcsine, proporciones
#'
#' Permite la transformación de Arcsine aplicada a proporciones. Aplicar la transformación arcsine se ha
#' sugerido por mucho tiempo como un método estandar al tratar con proporciones/porcentajes (Sokahl & Rohlf, #' 1995). Ver: Sokahl, R., & Rohlf, F. (1995). Biometry (3rd ed.). New York: W. H. Freeman.
#' @param x vector que contiene las proporciones en escalas de porcentaje de 0-100.
#'
#' @return vector transformado.
#' @export
#'
#' @examples
#' #variable <- seq(1,99, 3)
#' #trans.arcsine(variable)
#' @encoding UTF-8
#'
trans.arcsine = function(x){asin(sqrt(0.01*x))}



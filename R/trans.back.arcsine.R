#' Re-transformar variable con arcsine
#'
#' Permite la re-transformación de un vector previamente transformado con Arcsine aplicada a
#' proporciones. Aplicar la transformación arcsine se ha
#' sugerido por mucho tiempo como un método estandar al tratar con proporciones/porcentajes (Sokahl & Rohlf, #' 1995). Ver: Sokahl, R., & Rohlf, F. (1995). Biometry (3rd ed.). New York: W. H. Freeman.
#' @param x vector que contiene las proporciones transformadas con arcsine.
#'
#' @return vector transformado a su escala original.
#' @export
#'
#' @examples
#' #variable <- seq(1,99, 3)
#' #var.arcssine <- trans.arcsine(variable)
#' #trans.back.arcsine(var.arcssine)
#' @encoding UTF-8

trans.back.arcsine = function(x){(sin(x)^2)*100} # para variables en porcentajes 0 y 100



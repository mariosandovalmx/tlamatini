#' #' Re-transformar variable con transformación logit
#'
#' Permite la re-transformación de una variable previamente transformada con logit . Aplicar la transformación logit seguida de una regresión por mínimos cuadrados no lineales sobre  las variables transformadas (Warton & Hui, 2011). Este metodo se ha sugerido por mucho tiempo como un  método para tratar con proporciones/porcentajes. Ver: Warton, D. I., & Hui, F. K. C. (2011). The arcsine is asinine: The analysis of proportions in ecology. Ecology, 92(1), 3–10. https://doi.org/10.1890/10‐0340.1
#' @param x vector que contiene las proporciones transformadas con logit.
#' @param a Parémetro fijo de la transformación.
#'
#' @return vector transformado a escala original.
#' @export
#'
#' @examples
#' #variable <- seq(1,99, 3)
#' #logit.var<- trans.logit(variable)
#' #trans.back.logit(logit.var)
#' @encoding UTF-8

trans.back.logit = function(x,a=0.025){
  inv.logit <- function(f,a) {
    a <- (1-2*a)
    (a*(1+exp(f))+(exp(f)-1))/(2*a*(1+exp(f)))
  }

  vector<- zapsmall(inv.logit(x,a=0.025)*100)
  return(vector)
}


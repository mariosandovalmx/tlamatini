#' Transformar variable para ajustar regresión beta, util para proporciones
#'
#' Cuando se tienen 0 y 1 absolutos en las proporciones, no se puede ajustar un modelo de regresion Beta. Arroja una advertencia: invalid dependent variable, all observations must be in (0, 1). Se recomienda aplicar la transformacion de las proporciones. Con base en el articulo: Analysing continuous proportions in ecology and evolution: A practical introduction to beta and Dirichlet regression. para ajustar modelos con familia beta, propia para proporciones o porcentajes entre 0 y 1. Esta transformacion permite lidiar con los 0 y 1 absolutos.
#' @param x vector que contiene las proporciones en escalas de porcentaje entre 0 y 1.
#'
#' @return vector transformado.
#' @export
#'
#' @examples
#' x<- seq(0.1, 0.9, 0.01)
#' trans.01.beta(x)
#' #trans.01.beta(df$proporciones)
#' @encoding UTF-8

trans.01.beta <- function(x) {
  (x * (length(x) - 1) + 0.5) / (length(x))
}


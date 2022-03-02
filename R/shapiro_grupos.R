#' Prueba de normalidad de Shapiro-Wilk por grupos.
#'
#' Realiza la prueba de normalidad de Shapiro-Wilk por grupos de un dataframe. Se pueden seleccionar una o más variables a la vez.
#' @param formula Formula iniciando por la variable numerica de nuestro interes. Puede tomar forma de y ~ x+z. Se pueden usar 2 o más grupos.
#' @param datos Base de datos o dataframe.
#'
#' @return Una prueba Shapiro-Wilk por grupos
#' @export
#'
#' @examples
#' #data(iris)
#' #shapiro_grupos(Petal.Length~ Species, iris)
#' #iris$site <- c(rep("A", 75), rep("B", 75))
#' #shapiro_grupos(Petal.Length~ Species+ site, iris)
#' @encoding UTF-8


shapiro_grupos <- function(formula, datos){

  form <- as.formula(formula)

  #  calcular el valor de W
  res<- stats::aggregate(form, data = datos, FUN =
                           function(x) shapiro.test(x)$statistic)

  names(res)[names(res) == lhsOfFormula] <- "W.statistic"

  #calcular el valor de p
  resb<- stats::aggregate(form, data = datos, FUN =
                            function(x) shapiro.test(x)$p.value)
  names(resb)[names(resb) == lhsOfFormula] <- "p.value"

  #unir los dos resultados en una tabla
  results<- base::merge(x = res, y = resb, all.x = TRUE)
  message(c("Prueba de normalidad de Shapiro-Wilk por grupos"))
  return(results)
}

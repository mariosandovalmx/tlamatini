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
#' #norm.shapiro.grupos(Petal.Length~ Species, iris)
#' #iris$site <- c(rep("A", 75), rep("B", 75))
#' #norm.shapiro.grupos(Petal.Length~ Species+ site, iris)
#' @encoding UTF-8
#' @importFrom stats aggregate
#' @importFrom stats as.formula
#' @importFrom car qqPlot

norm.shapiro.grupos <- function(formula, datos){

  form <- as.formula(formula)
  resp.var<- as.character(form)[2]

  #  calcular el valor de W
  res<- aggregate(form, data = datos, FUN =
                    function(x) shapiro.test(x)$statistic)
  res
  names(res)[names(res) == resp.var] <- "W.statistic"
  res
  #calcular el valor de p
  resb<- aggregate(form, data = datos, FUN =
                     function(x) shapiro.test(x)$p.value)
  names(resb)[names(resb) == resp.var] <- "p.value"
  resb
  #unir los dos resultados en una tabla
  results<- merge(x = res, y = resb, all.x = TRUE)
  message(c("Prueba de normalidad de Shapiro-Wilk por grupos"))
  auto_mfrow(nrow(res), setup = TRUE)
  # graficar qqplot
  aggregate(form, data = datos, FUN =
              function(x) car::qqPlot(x,glab=deparse(substitute(groups)), ylab=deparse(substitute(x))))
  message(c("Las graficas se muestran en orden de aparicion como se muestran las filas."))


  return(results)




}

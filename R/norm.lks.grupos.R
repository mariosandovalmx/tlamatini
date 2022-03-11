#' Prueba de normalidad de Lilliefors (Kolmogorov-Smirnov) por grupos.
#'
#' Realiza la prueba de Lilliefors (Kolmogorov-Smirnov) para probar la normalidad de una
#' variable por grupos.
#'
#' @param formula Formula iniciando por la variable numerica de nuestro interes. Puede tomar
#' forma de y ~ x+z. Se pueden usar 2 o más grupos.
#' @param datos Base de datos o dataframe.
#'
#' @return Una prueba Lilliefors-Kolmogorov-Smirnov por grupos, un valor de p y estadistico
#' asociado a la prueba.
#' @export
#'
#' @examples
#' #data(iris)
#' #norm.lks.grupos(Petal.Length~ Species, iris)
#' #iris$site <- c(rep("A", 75), rep("B", 75))
#' #norm.lks.grupos(Petal.Length~ Species+ site, iris)
#' @encoding UTF-8
#' @importFrom stats aggregate
#' @importFrom stats as.formula
#' @importFrom nortest lillie.test
#' @importFrom car qqPlot

norm.lks.grupos <- function(formula, datos){

  form <- as.formula(formula)
  resp.var<- as.character(form)[2]

  #  calcular el valor de W
  res<- aggregate(form, data = datos, FUN =
                    function(x) lillie.test(x)$statistic)
  res
  names(res)[names(res) == resp.var] <- "statistic"
  res
    #calcular el valor de p
  resb<- aggregate(form, data = datos, FUN =
                     function(x) lillie.test(x)$p.value)
  names(resb)[names(resb) == resp.var] <- "p.value"
  resb
  #unir los dos resultados en una tabla
  results<- merge(x = res, y = resb, all.x = TRUE)
  message(c("Prueba de normalidad de Lilliefors (Kolmogorov-Smirnov) por grupos"))

  auto_mfrow(nrow(res), setup = TRUE)
  # graficar qqplot
  aggregate(form, data = datos, FUN =
              function(x) car::qqPlot(x))
  message(c("Las graficas se muestran en orden de aparicion como se muestran las filas."))
  return(results)

}

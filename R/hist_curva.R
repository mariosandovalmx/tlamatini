#' Grafica un histograma de frecuencias y qqplot de una variable
#'
#' Crea un histograma de frecuencias con una linea de densidad de la distribución y una prueba de normalidad
#'. Además de mostrar un qqplot de la variable. Si la longitud del vector es < 30 aplica una prueba Shapiro
#'-Wilk, si es mayor usa ks.test.
#' @param variable Columna o vector numérico.
#'
#' @return Un gráfico de correlación con valores de correlación, valores de p y lineas de
#' regresión lineal y no lineal de cada relación.
#' @export
#'
#' @examples
#' data(iris)
#' hist_curva(iris$Sepal.Width)
#' @encoding UTF-8
#' @importFrom stats dnorm
#' @importFrom stats sd
#' @importFrom stats na.omit
#' @importFrom stats shapiro.test
#' @importFrom stats ks.test
#' @importFrom car qqPlot
#' @importFrom graphics lines
#' @importFrom graphics hist
hist_curva <- function(variable){

  variable<- stats::na.omit(variable)
  par(mfrow=c(1,2))
  h <- hist(variable, breaks = 12, main = "Histograma")
  xfit <- seq(min(variable), max(variable), length = 20)
  yfit <- stats::dnorm(xfit, mean = mean(variable), sd = stats::sd(variable))
  yfit <- yfit * diff(h$mids[1:2]) * length(variable)
  graphics::lines(xfit, yfit, col = c("#0C3D7D9F"), lwd = 2)

  #
  insight::print_color("Prueba de normalidad", "green")


  if(length(variable)<= 30){
    print(stats::shapiro.test(variable))} else{

      print(stats::ks.test(variable,"pnorm",mean=mean(variable), sd=sd(variable)))
    }


  car::qqPlot(variable)


}




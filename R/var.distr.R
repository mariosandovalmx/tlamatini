#' Graficar la distribucion de una variable numerica.
#'
#' Permite conocer la distribución de una variable numerica. Usando la paqueteria fitdistrplus, ajusta las distribuciones más probables de un vector. Para tener en cuenta la incertidumbre de los valores estimados de la curtosis y la asimetría a partir de los datos, el conjunto de datos puede someterse a un proceso de bootstrap fijando el argumento boot en un número entero superior a 10. A continuación, se calculan los valores de asimetría y curtosis correspondientes a las muestras de bootstrap y se presentan en color azul en el gráfico de asimetría y curtosis.
#' @param variable Variable de interes.
#'
#' @return gráfico con la distribución más probable de la variable.
#' @export
#'
#' @examples
#' #x<- rnorm(1000,313, 5.7)
#' #var.distr(x)
#' #var.distr(datos$variable)
#' @encoding UTF-8
#' @importFrom fitdistrplus descdist

var.distr <- function(variable){
  variable<- variable
  fitdistrplus::descdist(variable,boot = 10000)

}

#

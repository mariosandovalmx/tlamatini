% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yzero.R
\encoding{UTF-8}
\name{yzero}
\alias{yzero}
\title{Hacer cero absoulto en eje Y de ggplot}
\usage{
yzero()
}
\value{
Un grafico de ggplot con el tema predefinido.
}
\description{
Cambia el cero en formato "0.00" mostrado en el eje Y por un "0" visualmente mas estetico.Se usa como un argumento adicional de ggplot.
}
\examples{
#Para usar el tema predefinido
#library(ggplot)
#data(iris)
#ggplot(iris, aes(x=Petal.Length, y=Petal.Width)) +
#geom_point() +
#geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
#yzero()

}

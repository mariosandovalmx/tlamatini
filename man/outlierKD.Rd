% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlierKD.R
\encoding{UTF-8}
\name{outlierKD}
\alias{outlierKD}
\title{Encuentra y remueve los outliers de una variable}
\usage{
outlierKD(dt, var)
}
\arguments{
\item{dt}{dataframe}

\item{var}{variable de interes, un vector o columna.}
}
\value{
Una variable sin outliers.
}
\description{
Encuentra y quita outliers de un dataframe. Permite explorar la media y distribución con y sin ouliers,
además de permitir decidir si remover los outliers del dataframe o no. Función tomada y modificada parcialmente de:
https://datascienceplus.com/identify-describe-plot-and-removing-the-outliers-from-the-dataset/
}
\examples{
outlierKD(iris, Sepal.Length)
}

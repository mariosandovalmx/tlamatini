% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numSummary.R
\encoding{UTF-8}
\name{numSummary}
\alias{numSummary}
\title{Resumen de las variables numericas de un dataframe}
\usage{
numSummary(df)
}
\arguments{
\item{df}{Una base de datos, dataframe.}
}
\value{
Un resumen del dataframe.
}
\description{
Esta es una función que permite obtener el resumen de todas las variables numéricas de un
dataframe. Resumen de todas la variables numéricas de un dataframe. Detectar automaticamente las
variables numéricas y excluye las variables caracter o factor. Muestra el resumen: media, SD, min, max,
NA, etc.
Función tomada de la paquetería: xda: R package for exploratory data analysis.
}
\examples{
#resumen de todas las variables numericas del dataframe
numSummary(iris)
}

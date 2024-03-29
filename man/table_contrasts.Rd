% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_contrasts.R
\encoding{UTF-8}
\name{table_contrasts}
\alias{table_contrasts}
\title{Exportar tabla de contrastes post hoc}
\usage{
table_contrasts(cont.emmeans, digs = NULL)
}
\arguments{
\item{cont.emmeans}{Contrastes hechos con emmeans de un modelo LM,GLM, GLMM, GAM, LMM, etc.}

\item{digs}{Numero de digitos a mostrar en la tabla.}
}
\value{
Tabla de contrastes.
}
\description{
Extraer los contrastes de un modelo, para generar una tabla en HTML copiable en Word. Para usar
esta función es necesario crear un objeto con los contrastes previamente hechos con la paqueteria
emmeans.
}
\examples{
#data(iris)
#modelo <- glm(Petal.Width ~ Petal.Length+Species, family = gaussian("log"), data=iris)
#instalar y cargar paquete "emmeans"
#library(emmeans)
#cont <- emmeans(modelo,pairwise ~ Species,adjust="tukey",type="response")$contrasts
#table_contrasts(cont)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_contrasts_letters.R
\encoding{UTF-8}
\name{table_contrasts_letters}
\alias{table_contrasts_letters}
\title{Extraer los contrastes post hoc y representar las diferencias con letras}
\usage{
table_contrasts_letters(df.emmeans)
}
\arguments{
\item{df.emmeans}{Medias obtenidas con emmeans de un modelo LM,GLM, GLMM, GAM, LMM, etc.}
}
\value{
Tabla de contrastes con letras.
}
\description{
Función en versión de prueba, puede haber errores en script Esta funcion permite obtener los contrastes por letras para luego ponerlas en las figuras.
Requiere un objeto que contenga las medias de los grupos obtenidos de la paqueteria emmeans. Para usar
esta función es necesario crear un objeto con los contrastes previamente hechos con la #' paqueteria
emmeans.
}
\examples{
#ejemplo, no correr:
#data(iris)
#modelo <- glm(Petal.Width ~ Petal.Length+Species, family = gaussian("log"), data=iris)
#library(emmeans)
#cont2 <- emmeans(modelo, pairwise ~ Species, type = "response")
#table_contrasts_letters(cont2)
}

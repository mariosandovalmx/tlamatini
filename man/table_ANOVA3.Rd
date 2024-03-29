% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_ANOVA3.R
\encoding{UTF-8}
\name{table_ANOVA3}
\alias{table_ANOVA3}
\title{Extraer tabla de ANOVA Tipo III}
\usage{
table_ANOVA3(modelo)
}
\arguments{
\item{modelo}{Un modelo GLM, GLMM, LMM, etc.}
}
\value{
Tabla ANOVA similar a la que se obtiene con la paqueteria car::Anova.
}
\description{
Extraer tabla de ANOVA Tipo III de un modelo. Extraer la tabla como HTML con formato publicable. Puede ser que algunos modelos no sean soportados y deberían llamarse directamente con la función Anova de la paquetería car.
}
\examples{
data(iris)
modelo <- glm(Petal.Width ~ Petal.Length, family = gaussian("log"), data=iris)
table_ANOVA3(modelo)
}

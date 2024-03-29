% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/violinplot_emmeans.R
\encoding{UTF-8}
\name{violinplot_emmeans}
\alias{violinplot_emmeans}
\title{Violinplot con pvalues obtenidos con emmeans}
\usage{
violinplot_emmeans(formula, modelo, data, p.adjust.method = NULL)
}
\arguments{
\item{formula}{Formula de los contrastes que nos interesan obtener.}

\item{modelo}{Modelo LM, GLM, GLMM, etc.}

\item{data}{Base de datos con que se ajusto el modelo.}

\item{p.adjust.method}{método para ajustar los valores p para comparaciones múltiples. Se utiliza cuando se realizan comparaciones por pares. Los valores permitidos incluyen "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". Si no desea ajustar el valor p (no recomendado), utilice p.adjust.method = "none".}
}
\value{
Violinplot de los contrastes posthoc en el modelo todas las variables explicativas del modelo.
}
\description{
Violinplot con los valores de p obtenidos con emmeans de un LM, LMM, GLM, GLMM, etc. Gráficos generados con paqueteria "ggplot", por ahora solo permite una variable explicativa sin interacciones. Proporciona informacion sobre la distribución de cada grupo, así como la media y los quantiles.
}
\examples{
library(tlamatini)
data(iris)
iris$Species <- as.factor(iris$Species)
mod <- glm(Petal.Width ~ Species, family = gaussian("log"), data=iris)
violinplot_emmeans(formula = Petal.Width ~ Species, modelo=mod, data= iris)
}

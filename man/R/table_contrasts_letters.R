#' Extraer los contrastes post hoc y representar las diferencias con letras
#'
#' Función en versión de prueba, puede haber errores en script Esta funcion permite obtener los contrastes por letras para luego ponerlas en las figuras.
#' Requiere un objeto que contenga las medias de los grupos obtenidos de la paqueteria emmeans. Para usar
#' esta función es necesario crear un objeto con los contrastes previamente hechos con la #' paqueteria
#' emmeans.
#' @param df.emmeans Medias obtenidas con emmeans de un modelo LM,GLM, GLMM, GAM, LMM, etc.
#'
#' @return Tabla de contrastes con letras.
#' @export
#'
#' @examples
#' #ejemplo, no correr:
#' #data(iris)
#' #modelo <- glm(Petal.Width ~ Petal.Length+Species, family = gaussian("log"), data=iris)
#' #library(emmeans)
#' #cont2 <- emmeans(modelo, pairwise ~ Species, type = "response")
#' #table_contrasts_letters(cont2)
#' @encoding UTF-8
#' @importFrom sjPlot tab_df
#' @importFrom multcomp cld

table_contrasts_letters <- function(df.emmeans){

  contrastes<- multcomp::cld(df.emmeans[[1]], Letters = letters,  alpha = 0.05)
  matchme <- c('emmean', 'response')
  names(contrastes)[names(contrastes) %in% matchme] <- "Medias"
  contrastes2<-contrastes[ , -which(names(contrastes) %in% c("df"))]
  sjPlot::tab_df(contrastes2,digits = 3)



}

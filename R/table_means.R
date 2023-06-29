#' Exportar tabla de medias, SE e intervalos de confianza de un modelo
#'
#' Extraer las medias entre grupos de un modelo, para generar una tabla en HTML copiable en Word.
#' @param cont.emmeans Medias obtenidas con emmeans de un modelo LM,GLM, GLMM, GAM, LMM, etc.
#'
#' @return Tabla de contrastes con medias, se e IC.
#' @export
#'
#' @examples
#' #mn2 <- emmeans(m4,pairwise ~ Sexo|tratamiento|tipo, adjust="tukey", type="response")$emmeans
#' #table_means(mn2)
#' @encoding UTF-8
#' @import dplyr
#' @importFrom sjPlot tab_df
table_means <- function(cont.emmeans){

  contrastes<- as.data.frame(cont.emmeans)
  contrastes<- contrastes#[,c(1:4,5,6)]
  names(contrastes)[names(contrastes) == "response"] <- "Mean"


  contrastes <- contrastes %>% mutate_if(is.numeric, round, digits=3)

  contrastes<- na.omit(contrastes)
  tab_df(contrastes, digits = 3)
}

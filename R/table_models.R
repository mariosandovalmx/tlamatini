#' Exportar tabla de un modelo
#'
#' Extraer el summary de un modelo LM, GLM, GLMM, GAM, etc. Generar una tabla en HTML copiable en Word.
#' @param ... Uno o más modelos LM, GLM, GLMM, GAM, LMM, etc.
#' @return Tabla del summary del modelo, con formato agradable lista para publicación.
#' @export
#'
#' @examples
#' #table.models(model)
#' #table.models(m,m2,m3)
#' @encoding UTF-8
#' @importFrom sjPlot tab_model

table.models<- function(...){

  tab<-sjPlot::tab_model(..., show.se = TRUE,show.ci=F,digits=3,show.stat = TRUE,show.r2 = TRUE,show.aic = TRUE,p.style = c("numeric"),show.p = TRUE,transform = NULL)
  return(tab)

}

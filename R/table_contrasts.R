#' Exportar tabla de contrastes post hoc
#'
#' Extraer los contrastes de un modelo, para generar una tabla en HTML copiable en Word. Para usar
#' esta función es necesario crear un objeto con los contrastes previamente hechos con la paqueteria
#' emmeans.
#' @param cont.emmeans Contrastes hechos con emmeans de un modelo LM,GLM, GLMM, GAM, LMM, etc.
#'
#' @return Tabla de contrastes.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length+Species, family = gaussian("log"), data=iris)
#' library(emmeans)
#' cont <- emmeans(modelo,pairwise ~ Species,adjust="tukey",type="response")$contrasts
#' table_contrasts(cont)
#' @encoding UTF-8
#' @importFrom sjPlot tab_df


table_contrasts <- function(cont.emmeans){

  contrastes<- as.data.frame(cont.emmeans)
  contrastes<- contrastes#[,c(1:4,5,6)]

  if("ratio" %in% colnames(contrastes))
  { contrastes$ratio <-as.numeric(substr(contrastes$ratio, start = 1, stop = 5));
  } else{
    contrastes$estimate <-as.numeric(substr(contrastes$estimate, start = 1, stop = 5))
  }
  contrastes$SE<- as.numeric(substr(contrastes$SE, start = 1, stop = 5))
  if("t.ratio" %in% colnames(contrastes))
  { contrastes$t.ratio <- as.numeric(substr(contrastes$t.ratio, start = 1, stop = 5));
  } else{
    contrastes$z.ratio <-as.numeric(substr(contrastes$z.ratio, start = 1, stop = 5))
  }
  contrastes$p.value<-format(contrastes$p.value, scientific = FALSE)
  contrastes$p.value <- as.numeric(substr(contrastes$p.value, start = 1, stop = 5))
  contrastes$p.value[contrastes$p.value <= 0.000] <- "<0.001"
  contrastes<- na.omit(contrastes)



  #contrastes2<- contrastes[ , -which(names(contrastes) %in% c("null"))]

  sjPlot::tab_df(contrastes,digits = 3)

}



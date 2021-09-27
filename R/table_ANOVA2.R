#' Extraer tabla de ANOVA Tipo II
#'
#' Extraer tabla de ANOVA Tipo II de un modelo. Extraer la tabla como HTML con formato publicable.
#' @param modelo Un modelo LM,GLM, GLMM, GAM, LMM, etc.
#'
#' @return Tabla ANOVA tipo II similar a la que se obtiene con la paqueteria car::Anova.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length, family = gaussian("log"), data=iris)
#' table_ANOVA2(modelo)
#' @encoding UTF-8
#' @importFrom car Anova
#' @importFrom data.table setDT
#' @importFrom dplyr rename
#' @importFrom sjPlot tab_df

table_ANOVA2 <- function(modelo) {

  options(scipen = 999)
  anovas<- car::Anova(modelo,type="II")
  anovas<- data.table::setDT(anovas, keep.rownames = TRUE)[]
  names(anovas)[names(anovas) == "rn"] <- "Variables"
  anovas<- as.data.frame(anovas)
  # redondear valor de p
  anovas$`Pr(>Chisq)`= ifelse(anovas$`Pr(>Chisq)`> 0.001,  format(round(anovas$`Pr(>Chisq)`,3),nsmall=3),  "<0.001" )

  sjPlot::tab_df(anovas, title = "Analysis of Deviance Table (Type II tests)")


}
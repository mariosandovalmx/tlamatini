#' Extraer tabla de ANOVA Tipo II
#'
#' Extraer tabla de ANOVA Tipo II de un modelo. Extraer la tabla como HTML con formato publicable. Puede ser que algunos modelos no sean soportados y deberían llamarse directamente con la función Anova de la paquetería car.
#' @param modelo Un modelo GLM, GLMM, LMM, etc.
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
  anovas[,ncol(anovas)]  <- format(anovas[,ncol(anovas)], scientific = FALSE)
  anovas[,ncol(anovas)]  <- as.numeric(substr(anovas[,ncol(anovas)]  , start = 1, stop = 5))
  anovas[,ncol(anovas)][anovas[,ncol(anovas)]   < 0.001] <- "<0.001"
  colnames(anovas)[ncol(anovas)] <- "P.value"
  tab<- sjPlot::tab_df(anovas, title = "Analysis of Deviance Table (Type II tests)")
  return(tab)
  options(scipen=0)




}

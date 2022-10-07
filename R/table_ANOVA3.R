#' Extraer tabla de ANOVA Tipo III
#'
#' Extraer tabla de ANOVA Tipo III de un modelo. Extraer la tabla como HTML con formato publicable. Puede ser que algunos modelos no sean soportados y deberían llamarse directamente con la función Anova de la paquetería car.
#' @param modelo Un modelo GLM, GLMM, LMM, etc.
#'
#' @return Tabla ANOVA similar a la que se obtiene con la paqueteria car::Anova.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length, family = gaussian("log"), data=iris)
#' table_ANOVA3(modelo)
#' @encoding UTF-8
#' @importFrom car Anova
#' @importFrom data.table setDT
#' @importFrom dplyr rename
#' @importFrom sjPlot tab_df

##### ANOVA TYPE III  table

table_ANOVA3 <- function(modelo) {


  options(scipen = 999)
  anovas<- car::Anova(modelo,type="III")
  anovas<- data.table::setDT(anovas, keep.rownames = TRUE)[]
  names(anovas) <- c("Variables","LR Chisq","DF","P.value")
  #names(anovas)[names(anovas) == "rn"] <- "Variables"
  anovas$P.value<- format_p_values(anovas$P.value)

  anovas<- as.data.frame(anovas)
  strs<- get_signif_stars(0.001)
  strs2<- as.vector(unlist(attributes(strs)))
  strs3<- c("<0.001 \u2018***\u2019 0.001 \u2018**\u2019 0.01 \u2018*\u2019 0.05 \u2018.\u2019")
  #anovas[,ncol(anovas)]  <- format(anovas[,ncol(anovas)], scientific = FALSE)
  #anovas[,ncol(anovas)]  <- as.numeric(substr(anovas[,ncol(anovas)]  , start = 1, stop = 5))
  #anovas[,ncol(anovas)][anovas[,ncol(anovas)]   < 0.001] <- "<0.001"
  tab<- sjPlot::tab_df(anovas, title = "Analysis of Deviance Table (Type III tests)", footnote =strs3, show.footnote = T)


  return(tab)
  options(scipen=0)
}


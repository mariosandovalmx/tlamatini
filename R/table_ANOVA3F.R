#' Extraer tabla de ANOVA Tipo III usando el estadistico F
#'
#' Extraer tabla de ANOVA Tipo III usando el estadistico F. Puede ser que algunos modelos no sean soportados.
#' @param modelo Un modelo LM,GLM,y LMM.
#'
#' @return Tabla ANOVA similar a la que se obtiene con la paqueteria car::Anova.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length, family = gaussian("log"), data=iris)
#' table_ANOVA3F(modelo)
#' @encoding UTF-8
#' @importFrom car Anova
#' @importFrom data.table setDT
#' @importFrom dplyr rename
#' @importFrom sjPlot tab_df
table_ANOVA3F <- function(modelo) {


  if( modelo[["call"]][[1]]== "glm"){
    #### GLM y LM
    options(scipen = 999)
    anovas<- car::Anova(modelo,type="III", test=c("F"))
    anovas<- data.table::setDT(anovas, keep.rownames = TRUE)[]
    names(anovas)[names(anovas) == "rn"] <- "Variables"
    anovas<- as.data.frame(anovas)
    #quitar fila con NA
    anovas2 <- anovas[is.na(anovas$`F values`),]
    anovas<- na.omit(anovas)
    # redondear valor de p
    anovas$`Pr(>F)` = ifelse(anovas$`Pr(>F)`> 0.001,  format(round(anovas$`Pr(>F)` ,3),nsmall=3),  "<0.001" )
    anovas3<- rbind(anovas, anovas2)
    sjPlot::tab_df(anovas3, title = "Analysis of Deviance Table (Type III tests)")
  } else if(modelo[["call"]][[1]]== "lme.formula"){


    #### LMM
    options(scipen = 999)
    anovas<- car::Anova(modelo,type="III", test=c("F"))
    anovas<- data.table::setDT(anovas, keep.rownames = TRUE)[]
    names(anovas)[names(anovas) == "rn"] <- "Variables"
    anovas<- as.data.frame(anovas)
    # redondear valor de p
    anovas$`Pr(>Chisq)`= ifelse(anovas$`Pr(>Chisq)`> 0.001,  format(round(anovas$`Pr(>Chisq)`,3),nsmall=3),  "<0.001" )

    sjPlot::tab_df(anovas, title = "Analysis of Deviance Table (Type III tests)")


  } else {
    message("Modelo no soportado")
  }


}



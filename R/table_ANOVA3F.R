#' Extraer tabla de ANOVA Tipo III usando el estadistico F
#'
#' Extraer tabla de ANOVA Tipo III usando el estadistico F. Extraer la tabla como HTML con formato publicable. Puede ser que algunos modelos no sean soportados y deberían llamarse directamente con la función Anova de la paquetería car.
#' @param modelo Un modelo GLM,y LMM.
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
    anovas[,4]  = ifelse(anovas[,4] > 0.001,  format(round(anovas[,4] ,3),nsmall=3),  "<0.001" )
    anovas3<- rbind(anovas, anovas2)
    sjPlot::tab_df(anovas3, title = "Analysis of Deviance Table (Type III tests)")
    options(scipen = -999)
  } else if(modelo[["call"]][[1]]== "lme.formula"){


    #### LMM
    options(scipen = 999)
    anovas<- car::Anova(modelo,type="III", test=c("F"))
    anovas<- data.table::setDT(anovas, keep.rownames = TRUE)[]
    names(anovas)[names(anovas) == "rn"] <- "Variables"
    anovas<- as.data.frame(anovas)
    # redondear valor de p
    anovas[,4] = ifelse(anovas[,4] > 0.001,  format(round(anovas[,4] ,3),nsmall=3),  "<0.001" )

    sjPlot::tab_df(anovas, title = "Analysis of Deviance Table (Type III tests)")
    options(scipen=0)


  } else {
    message("Modelo no soportado")
  }


}



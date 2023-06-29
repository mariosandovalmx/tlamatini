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
    names(anovas) <- c("Variables","Sum Sq","DF","F values","P.value")
    #names(anovas)[names(anovas) == "rn"] <- "Variables"
    anovas$P.value <- format_p_values(anovas$P.value)
    anovas[,c(4,5)] <- tlamatini::NA.cualquiera(anovas[,c(4,5)], "-")
    anovas<- as.data.frame(anovas)
    strs<- get_signif_stars(0.001)
    strs2<- as.vector(unlist(attributes(strs)))
    strs3<- c("<0.001 \u2018***\u2019 0.001 \u2018**\u2019 0.01 \u2018*\u2019 0.05 \u2018.\u2019")
    tab<- sjPlot::tab_df(anovas, title = "Analysis of Deviance Table (Type III tests)", footnote =strs3, show.footnote = T)

    return(tab)
    options(scipen = -999)
  } else if(modelo[["call"]][[1]]== "lme.formula"){


    #### LMM
    options(scipen = 999)
    anovas<- car::Anova(modelo,type="III", test=c("F"))
    anovas<- data.table::setDT(anovas, keep.rownames = TRUE)[]
    names(anovas) <- c("Variables","Sum Sq","DF","F values","P.value")
    anovas<- as.data.frame(anovas)
    strs<- get_signif_stars(0.001)
    strs2<- as.vector(unlist(attributes(strs)))
    strs3<- c("<0.001 \u2018***\u2019 0.001 \u2018**\u2019 0.01 \u2018*\u2019 0.05 \u2018.\u2019")


    tab<- sjPlot::tab_df(anovas, title = "Analysis of Deviance Table (Type III tests)", footnote =strs3, show.footnote = T)
    return(tab)
    options(scipen=0)


  } else {
    message("Modelo no soportado")
  }


}



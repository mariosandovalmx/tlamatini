#' Exportar tabla de contrastes post hoc
#'
#' Extraer los contrastes de un modelo, para generar una tabla en HTML copiable en Word. Para usar
#' esta función es necesario crear un objeto con los contrastes previamente hechos con la paqueteria
#' emmeans.
#' @param cont.emmeans Contrastes hechos con emmeans de un modelo LM,GLM, GLMM, GAM, LMM, etc.
#' @param digs Numero de digitos a mostrar en la tabla.
#'
#' @return Tabla de contrastes.
#' @export
#'
#' @examples
#' #data(iris)
#' #modelo <- glm(Petal.Width ~ Petal.Length+Species, family = gaussian("log"), data=iris)
#' #instalar y cargar paquete "emmeans"
#' #library(emmeans)
#' #cont <- emmeans(modelo,pairwise ~ Species,adjust="tukey",type="response")$contrasts
#' #table_contrasts(cont)
#' @encoding UTF-8
#' @importFrom sjPlot tab_df

table_contrasts <- function(cont.emmeans, digs= NULL){
if(is.null(digs)){
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                  SI NO ESPECIFICA CUANTOS DIGITOS MOSTRAR                ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if( length(cont.emmeans@levels) == 1){
    conts<- cont.emmeans
    conts<- as.data.frame(conts)
    contrastes<- as.data.frame(conts)

    if("ratio" %in% colnames(contrastes))
    { contrastes$ratio <-as.numeric(substr(contrastes$ratio, start = 1, stop = 5));
    } else if("odds.ratio" %in% colnames(contrastes)){
      contrastes$odds.ratio <-as.numeric(substr(contrastes$odds.ratio, start = 1, stop = 5))
    } else if("estimate" %in% colnames(contrastes)){
      contrastes$estimate <-as.numeric(substr(contrastes$estimate, start = 1, stop = 5))
    }

    contrastes$SE<- as.numeric(substr(contrastes$SE, start = 1, stop = 5))

    if("t.ratio" %in% colnames(contrastes))
    { contrastes$t.ratio <- as.numeric(substr(contrastes$t.ratio, start = 1, stop = 5));
    } else  if("z.ratio" %in% colnames(contrastes)){
      contrastes$z.ratio <-as.numeric(substr(contrastes$z.ratio, start = 1, stop = 5))
    }

    contrastes$p.value<-format(contrastes$p.value, scientific = FALSE)
    contrastes$p.value <- as.numeric(substr(contrastes$p.value, start = 1, stop = 5))
    #contrastes$p.value[contrastes$p.value <= 0.000] <- "<0.001"
    contrastes$p.value <- format_p_values(contrastes$p.value)
    contrastes<- na.omit(contrastes)
    if("null" %in% colnames(contrastes))
    { contrastes<- contrastes[,-grep("null",colnames(contrastes))]
    }else {
      contrastes<- contrastes
    }

    strs<- c("<0.001 \u2018***\u2019 0.001 \u2018**\u2019 0.01 \u2018*\u2019 0.05 \u2018.\u2019")


    sjPlot::tab_df(contrastes,digits = 3, footnote =strs, show.footnote = T)


  } else {

    contrastes<- as.data.frame(cont.emmeans)
    contrastes<- contrastes#[,c(1:4,5,6)]

    if("ratio" %in% colnames(contrastes))
    { contrastes$ratio <-as.numeric(substr(contrastes$ratio, start = 1, stop = 5));
    } else if("odds.ratio" %in% colnames(contrastes)){
      contrastes$odds.ratio <-as.numeric(substr(contrastes$odds.ratio, start = 1, stop = 5))
    } else if("estimate" %in% colnames(contrastes)){
      contrastes$estimate <-as.numeric(substr(contrastes$estimate, start = 1, stop = 5))
    }

    contrastes$SE<- as.numeric(substr(contrastes$SE, start = 1, stop = 5))
    if("t.ratio" %in% colnames(contrastes))
    { contrastes$t.ratio <- as.numeric(substr(contrastes$t.ratio, start = 1, stop = 5));
    } else if("z.ratio" %in% colnames(contrastes)){
      contrastes$z.ratio <-as.numeric(substr(contrastes$z.ratio, start = 1, stop = 5))
    }
    contrastes$p.value<-format(contrastes$p.value, scientific = FALSE)
    contrastes$p.value <- as.numeric(substr(contrastes$p.value, start = 1, stop = 5))
    #contrastes$p.value[contrastes$p.value <= 0.000] <- "<0.001"
    contrastes$p.value <- format_p_values(contrastes$p.value)
    contrastes<- na.omit(contrastes)
    if("null" %in% colnames(contrastes))
    { contrastes<- contrastes[,-grep("null",colnames(contrastes))]
    }else {
      contrastes<- contrastes
    }


    #contrastes2<- contrastes[ , -which(names(contrastes) %in% c("null"))]
    strs<- c("<0.001 \u2018***\u2019 0.001 \u2018**\u2019 0.01 \u2018*\u2019 0.05 \u2018.\u2019")
    sjPlot::tab_df(contrastes,digits = 3, footnote =strs, show.footnote = T)}






} else if(isTRUE(is.numeric(digs))){

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                                                            ~~
  ##                  SI SE ESPECIFICA CUANTOS DIGITOS MOSTRAR                ----
  ##                                                                            ~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if( length(cont.emmeans@levels) == 1){
    conts<- cont.emmeans
    conts<- as.data.frame(conts)
    contrastes<- as.data.frame(conts)

    if("ratio" %in% colnames(contrastes))
    { contrastes$ratio <-as.numeric(substr(contrastes$ratio, start = 1, stop = 5));
    } else if("odds.ratio" %in% colnames(contrastes)){
      contrastes$odds.ratio <-as.numeric(substr(contrastes$odds.ratio, start = 1, stop = 5))
    } else if("estimate" %in% colnames(contrastes)){
      contrastes$estimate <-as.numeric(substr(contrastes$estimate, start = 1, stop = 5))
    }

    contrastes$SE<- as.numeric(substr(contrastes$SE, start = 1, stop = 5))

    if("t.ratio" %in% colnames(contrastes))
    { contrastes$t.ratio <- as.numeric(substr(contrastes$t.ratio, start = 1, stop = 5));
    } else  if("z.ratio" %in% colnames(contrastes)){
      contrastes$z.ratio <-as.numeric(substr(contrastes$z.ratio, start = 1, stop = 5))
    }

    contrastes$p.value<-format(contrastes$p.value, scientific = FALSE)
    contrastes$p.value <- as.numeric(substr(contrastes$p.value, start = 1, stop = 5))
    #contrastes$p.value[contrastes$p.value <= 0.000] <- "<0.001"
    contrastes$p.value <- format_p_values(contrastes$p.value)
    contrastes<- na.omit(contrastes)
    if("null" %in% colnames(contrastes))
    { contrastes<- contrastes[,-grep("null",colnames(contrastes))]
    }else {
      contrastes<- contrastes
    }

    strs<- c("<0.001 \u2018***\u2019 0.001 \u2018**\u2019 0.01 \u2018*\u2019 0.05 \u2018.\u2019")
    sjPlot::tab_df(contrastes,digits = digs, footnote =strs, show.footnote = T)


  } else {

    contrastes<- as.data.frame(cont.emmeans)
    contrastes<- contrastes#[,c(1:4,5,6)]

    if("ratio" %in% colnames(contrastes))
    { contrastes$ratio <-as.numeric(substr(contrastes$ratio, start = 1, stop = 5));
    } else if("odds.ratio" %in% colnames(contrastes)){
      contrastes$odds.ratio <-as.numeric(substr(contrastes$odds.ratio, start = 1, stop = 5))
    } else if("estimate" %in% colnames(contrastes)){
      contrastes$estimate <-as.numeric(substr(contrastes$estimate, start = 1, stop = 5))
    }

    contrastes$SE<- as.numeric(substr(contrastes$SE, start = 1, stop = 5))
    if("t.ratio" %in% colnames(contrastes))
    { contrastes$t.ratio <- as.numeric(substr(contrastes$t.ratio, start = 1, stop = 5));
    } else if("z.ratio" %in% colnames(contrastes)){
      contrastes$z.ratio <-as.numeric(substr(contrastes$z.ratio, start = 1, stop = 5))
    }
    contrastes$p.value<-format(contrastes$p.value, scientific = FALSE)
    contrastes$p.value <- as.numeric(substr(contrastes$p.value, start = 1, stop = 5))
    #contrastes$p.value[contrastes$p.value <= 0.000] <- "<0.001"
    contrastes$p.value <- format_p_values(contrastes$p.value)
    contrastes<- na.omit(contrastes)
    if("null" %in% colnames(contrastes))
    { contrastes<- contrastes[,-grep("null",colnames(contrastes))]
    }else {
      contrastes<- contrastes
    }


    #contrastes2<- contrastes[ , -which(names(contrastes) %in% c("null"))]
    strs<- c("<0.001 \u2018***\u2019 0.001 \u2018**\u2019 0.01 \u2018*\u2019 0.05 \u2018.\u2019")
    sjPlot::tab_df(contrastes,digits = digs, footnote =strs, show.footnote = T)}


}
}



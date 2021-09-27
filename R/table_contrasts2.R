#' Exportar tabla de contrastes post hoc. Tres variables en interacción
#'
#' Extraer los contrastes de un modelo, para generar una tabla en HTML copiable en Word. Esta funcion se usa
#' cuando tenemos contrastes entre tres grupos, por ejemplo la interacción entre tratamiento x tipo x  sexo.
#' Para usar esta función es necesario crear un objeto con los contrastes previamente hechos con la
#' paqueteria emmeans.
#' @param cont.emmeans Contrastes hechos con emmeans de un modelo LM,GLM, GLMM, GAM, LMM, etc.
#' @param title Titulo opcional.
#' @param sort Argumento TRUE o FALSE, permite decidir si se ordenan los valores de P.
#' @param digs número de digitos a mostrar.
#'
#' @return Tabla de contrastes.
#' @export
#'
#' @examples
#' #ejemplo, no correr:
#' #cont2 <- emmeans(modelo, pairwise ~ Sexo|tratamiento|tipo, adjust="tukey")$contrasts
#' #table_contrasts2(cont2)
#' @encoding UTF-8
#' @importFrom sjPlot tab_df

table_contrasts2 <- function(cont.emmeans,title,digs, sort=FALSE){
  if(sort == FALSE){

    contrastes<- as.data.frame(cont.emmeans)
    contrastes<- contrastes#[,c(1:8)]
    digs <- digs


    if("ratio" %in% colnames(contrastes))
    { contrastes$ratio <-as.numeric(substr(contrastes$ratio, start = 1, stop = 5));}
    else{
      contrastes$estimate <-as.numeric(substr(contrastes$estimate, start = 1, stop = 5))
    }
    contrastes$SE<- as.numeric(substr(contrastes$SE, start = 1, stop = 5))

    if("t.ratio" %in% colnames(contrastes))
    { contrastes$t.ratio <- as.numeric(substr(contrastes$t.ratio, start = 1, stop = 5));}
    else{
      contrastes$z.ratio <-as.numeric(substr(contrastes$z.ratio, start = 1, stop = 5))
    }

    contrastes$p.value<-format(contrastes$p.value, scientific = FALSE)
    contrastes$p.value <- as.numeric(substr(contrastes$p.value, start = 1, stop = 5))
    contrastes$p.value[contrastes$p.value <= 0.000] <- "<0.001"

    sjPlot::tab_df(contrastes, title = as.character(title),digits = 3)} else{

      contrastes<- as.data.frame(cont.emmeans)
      contrastes<- contrastes#[,c(1:8)]
      digs <- digs

      if("ratio" %in% colnames(contrastes))
      { contrastes$ratio <-as.numeric(substr(contrastes$ratio, start = 1, stop = 5));}
      else{
        contrastes$estimate <-as.numeric(substr(contrastes$estimate, start = 1, stop = 5))
      }
      contrastes$SE<- as.numeric(substr(contrastes$SE, start = 1, stop = 5))

      if("t.ratio" %in% colnames(contrastes))
      { contrastes$t.ratio <- as.numeric(substr(contrastes$t.ratio, start = 1, stop = 5));}
      else{
        contrastes$z.ratio <-as.numeric(substr(contrastes$z.ratio, start = 1, stop = 5))
      }

      contrastes$p.value<-format(contrastes$p.value, scientific = FALSE)
      contrastes$p.value <- as.numeric(substr(contrastes$p.value, start = 1, stop = 5))
      contrastes$p.value[contrastes$p.value <= 0.000] <- "<0.001"

      sjPlot::tab_df(contrastes, title = as.character(title),digits = 3,sort.column = 6)
    }
}


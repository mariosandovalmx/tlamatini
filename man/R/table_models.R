#' Exportar tabla de un modelo
#'
#' Extraer el summary de un modelo LM, GLM, GLMM, GAM, etc. Generar una tabla en HTML copiable en Word.
#' @param ... Uno o más modelos LM, GLM, GLMM, GAM, LMM, etc.
#' @param transformar Si es TRUE, se aplicara la transformacion inversa de la funcion de liga del modelo. Si no se proporciona un argumento, se mostraran los estimados y el std.error en la escala de la función de liga. La transformación solo se aplica al primer modelo, no se puede aplicar a multiples modelos a la vez.
#' @param digs Numero de digitos a mostrar en la tabla.
#' @return Tabla del summary del modelo, con formato agradable lista para publicacion.
#' @export
#'
#' @examples
#' # muestra valores sin transformar en la escala de la funcion de liga
#' #table.models(model)
#' #muestra valores sin transformar de todos los modelos
#' #table.models(m,m2,m3)
#' # muestra valores re-transformados a la escala original
#' #table.models(model, transformar=TRUE)
#' # muestra valores sin transformar
#' #table.models(model, transformar=FALSE)
#
#' @encoding UTF-8
#' @importFrom sjPlot tab_model

table.models<- function(..., transformar=NULL, digs=NULL){
  if(is.null(digs)){
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                                                            ~~
    ##                  SI NO ESPECIFICA CUANTOS DIGITOS MOSTRAR                ----
    ##                                                                            ~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    if(is.null(transformar)){

      tab<-sjPlot::tab_model(..., show.se = TRUE,show.ci=F,digits=3,show.stat = TRUE,show.r2 = TRUE,show.aic = TRUE,p.style = c("numeric"),show.p = TRUE,transform = NULL)
      return(tab)
    } else if(transformar == TRUE){
      tab<-sjPlot::tab_model(..., show.se = TRUE,show.ci=F,digits=3,show.stat = TRUE,show.r2 = TRUE,show.aic = TRUE,p.style = c("numeric"),show.p = TRUE)
      return(tab)
    } else {
      tab<-sjPlot::tab_model(..., show.se = TRUE,show.ci=F,digits=3,show.stat = TRUE,show.r2 = TRUE,show.aic = TRUE,p.style = c("numeric"),show.p = TRUE, transform = NULL)
      return(tab)
    } } else if(isTRUE(is.numeric(digs))){

      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##                                                                            ~~
      ##                  SI SE ESPECIFICA CUANTOS DIGITOS MOSTRAR                ----
      ##                                                                            ~~
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


      if(is.null(transformar)){

        tab<-sjPlot::tab_model(..., show.se = TRUE,show.ci=F,digits=digs,show.stat = TRUE,show.r2 = TRUE,show.aic = TRUE,p.style = c("numeric"),show.p = TRUE,transform = NULL)
        return(tab)
      } else if(transformar == TRUE){
        tab<-sjPlot::tab_model(..., show.se = TRUE,show.ci=F,digits=digs,show.stat = TRUE,show.r2 = TRUE,show.aic = TRUE,p.style = c("numeric"),show.p = TRUE)
        return(tab)
      } else {
        tab<-sjPlot::tab_model(..., show.se = TRUE,show.ci=F,digits=digs,show.stat = TRUE,show.r2 = TRUE,show.aic = TRUE,p.style = c("numeric"),show.p = TRUE, transform = NULL)
        return(tab)
      }
    }
}

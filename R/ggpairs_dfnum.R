#' ggpairs_dfnum Matriz de correlación
#'
#' Matriz de correlación basada en ggpairs con lineas de regresion lineal y no lineal. Matriz de correlación de variables numéricas. Esta función selecciona solo las variables numéricas y aplica la función ggpairs
#' de la paqueteria GGally.
#' @param dataframe Dataframe
#' @param var.response Si se proporciona el nombre de la variable de respuesta, esta se coloca al final, para mostrarla en el eje Y. Para facilitar la observación de la relación entre las variables explicativas y la variable de respuesta.
#'
#' @return Un gráfico de correlación con valores de correlación, valores de p y lineas de
#' regresión lineal y no lineal de cada relación.
#' @export
#'
#' @examples
#' #ggpairs_dfnum(iris)
#' #ggpairs_dfnum(iris, var.response = "Petal.Length")
#' @encoding UTF-8
#' @import ggplot2
#' @import GGally
#' @importFrom stats lm
#' @importFrom stats loess



ggpairs_dfnum <- function(dataframe, var.response=NULL){
  if(is.null(var.response)){
    #seleccionar solo variables numericas
    num_cols <- unlist(lapply(dataframe, is.numeric))         # Identify numeric columns
    num_cols
    dfnum <- dataframe[ , num_cols]                        # Subset numeric columns of data
    DD        <- as.data.frame(dfnum)


    ### Si calculamos lo mismo pero a?adimos una linea de regresi?n lineal y una no lineal
    ln.function <- function(data, mapping, ...){
      p <- ggplot2::ggplot(data = data, mapping = mapping) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method=loess, fill="red", color="red", ...) +
        ggplot2::geom_smooth(method=lm, fill="blue", color="blue", ...)
    }

    #obtenemos lo siguiente:
    g =  GGally::ggpairs(DD, lower = list(continuous = ln.function)) + ggplot2::theme_light()
    g


  }else if(isTRUE(is.character(var.response))){
    var.resp <-var.response

    num_cols <- unlist(lapply(dataframe, is.numeric))         # Identify numeric columns
    num_cols
    dfnum <- dataframe[ , num_cols]                        # Subset numeric columns of data
    DD        <- as.data.frame(dfnum)
    DD <- DD %>% select(-all_of(var.resp), everything())

    ### Si calculamos lo mismo pero a?adimos una linea de regresi?n lineal y una no lineal
    ln.function <- function(data, mapping, ...){
      p <- ggplot2::ggplot(data = data, mapping = mapping) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method=loess, fill="red", color="red", ...) +
        ggplot2::geom_smooth(method=lm, fill="blue", color="blue", ...)
    }

    #obtenemos lo siguiente:
    g =  GGally::ggpairs(DD, lower = list(continuous = ln.function)) + ggplot2::theme_light()
    g}
}

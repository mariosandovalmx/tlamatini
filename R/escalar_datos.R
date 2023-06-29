#' Estandarizar las variables de un dataframe
#'
#' Estandariza las todas las variables de un dataframe mediante el centrado
#' y el escalado. Para cada valor de una variable, simplemente restamos el
#' valor medio de la variable, luego dividimos por la desviacion estandar de
#' la variable. Esto significa que vamos a escalar los valores de manera que
#' la variable tenga una media de 0 y una desviaion estandar de 1.
#' Esto es util cuando las variables explicativas tienen diferentes escalas,
#' y a menudo no contribuyen por igual al modelo. Por ejemplo, si los valores
#' de una variable oscilan entre 0 y 10000 y los valores de otra variable oscilan
#' entre 0 y 10, la variable con el intervalo mas grande recibira un mayor peso
#' en el analisis. Al estandarizar las variables, podemos estar seguros de que
#' cada variable contribuye por igual al an?lisis. Este procedimiento no
#' representa una transformacion de los datos, y no cambia la magnitud del efecto ni
#' la significancia de la variable en el modelo.
#' Ver la funcion "stdize" de MuMIn package para mas detalles.
#' @param df Un dataframe
#' @param ex_variables Variables a excluir del dataframe, si no queremos aplicar
#' la escalada a dichas variables.
#' @param center centrado
#' @param scale escalado
#'
#' @return Dataframe escalado y centrado
#' @export
#'
#' @examples
#' data(iris)
#' #' #numericas del dataframe.
#' # Si no se especifica que variables excluir, se aplicara a todas las columnas
#' df.sc2 <- escalar_datos(iris)
#' # si se especifica que variables excluir
#' df.sc <- escalar_datos(iris, ex_variables = c("Petal.Width","Petal.Length"))
#' @encoding UTF-8
#' @importFrom dplyr mutate_at


escalar_datos <- function(df, ex_variables= NULL, center = NULL, scale=NULL){
  if(is.null(ex_variables)){

    df[] <- lapply(df, function(x) if(is.numeric(x)){
      scale(x, center=TRUE, scale=TRUE)
    } else x)


    return(df)
  } else if(isTRUE(is.character(ex_variables))){

    drops <- ex_variables
    dat2 <- df %>% mutate_at(c(drops), ~(scale(.) %>% as.vector))


    return(dat2)
  }

}

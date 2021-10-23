#' Convierte celdas vacias en NA
#'
#' Datos faltantes en blanco por NA.
#' @param x Dataframe con los datos faltantes no codificados como NA.
#'
#' @return Un vector con NA.
#' @export
#'
#' @examples
#' library(dplyr)
#' A  <- data.frame(x=c(1,2,3,4),y=c(1,2,3,4), z=c(1,"N/A",3,4))
#' A %>% mutate_each(funs(vacio.as.NA))
#' @encoding UTF-8
#' @import dplyr

vacio.as.NA <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}


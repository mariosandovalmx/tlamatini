#' Resumen de las variables caracter o factor de un dataframe
#'
#' Esta es una función que permite obtener el resumen de todas las variables no numéricas de un
#' dataframe. Permite obtener el número de observaciones (n), número de observaciones faltantes (miss),
#' porcentaje de observaciones faltantes (miss%), número de factores únicos (unique) y el top 5 de niveles.
#' Función tomada de la paquetería: xda: R package for exploratory data analysis.
#'
#' @param df Una base de datos, dataframe.
#'
#' @return Un resumen del dataframe.
#' @export
#'
#' @examples
#' charSummary(iris)
#' @encoding UTF-8
charSummary <- function(df){

  dfchar <- df[, sapply(df, class) %in% c('character', 'factor')]   # Solo variables caracter o factor
  E <- dfchar
  EE       <- as.data.frame(E)
  n        <- as.data.frame(sapply(EE, function(x) sum(!is.na(x))))
  n        <- data.frame(n)
  colnames(n) <- "n"

  n1       <- nrow(df)

  #valores perdidos
  faltantes     <- sapply(EE, function(x) sum(is.na(x)))
  faltantes     <- as.data.frame(faltantes)
  g3       <- cbind(n, faltantes)
  perc     <- (faltantes/n1)*100
  m3       <- cbind(g3, perc)
  colnames(m3)[ncol(m3)] <- "%faltantes"

  #top-5 level count
  topfivelevel <- function(x){
    tbl_x             <- table(x)
    topfive           <- sort(tbl_x, decreasing = TRUE)[1:ifelse(length(tbl_x) >= 5, yes = 5, no = length(tbl_x))]
    topfivelevelcount <- paste0(names(topfive), ":", topfive)
  }

  unique     <- sapply(EE, function(x) length(unique(x)))
  unique_val <- sapply(EE, function(x) paste0(topfivelevel(x), collapse = ", "))
  m4         <- cbind.data.frame(m3, unique, "top5:conteos" = unique_val)

  return(m4)
}

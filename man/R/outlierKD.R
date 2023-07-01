#' Encuentra y remueve los outliers de una variable
#'
#' Encuentra y quita outliers de un dataframe. Permite explorar la media y distribución con y sin ouliers,
#' además de permitir decidir si remover los outliers del dataframe o no. Función tomada y modificada parcialmente de:
#' https://datascienceplus.com/identify-describe-plot-and-removing-the-outliers-from-the-dataset/
#' @param dt dataframe
#' @param var variable de interes, un vector o columna.
#'
#' @return Una variable sin outliers.
#' @export
#'
#' @examples
#' outlierKD(iris, Sepal.Length)
#' @encoding UTF-8
#' @importFrom graphics boxplot
#' @importFrom graphics title
#' @importFrom graphics hist

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  graphics::boxplot(var_name, main="Con outliers")
  hist(var_name, main="Sin outliers", xlab=NA, ylab=NA)
  outlier <- graphics::boxplot(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  graphics::boxplot(var_name, main="Sin outliers")
  hist(var_name, main="Sin outliers", xlab=NA, ylab=NA)
  graphics::title("Outliers", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identificados: ", na2 - na1, " de ", tot, " observaciones")
  message("Porcentaje de outliers: ", (na2 - na1) / tot*100)
  message("Media de los outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Media sin remover outliers: ", m1)
  message("Media si removemos outliers: ", m2)
  response <- readline(prompt="Quiere eliminar los outliers y sustituirlos por NA? [si/no]: ")
  if(response == "s" | response == "si"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    base::assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers removidos exitosamente", "\n")
    return(invisible(dt))
  } else{
    message("Nada cambia", "\n")
    return(invisible(var_name))
  }
}

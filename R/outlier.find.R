#' Encuentra los outliers de un dataframe
#'
#' Encuentra los outliers de un dataframe. Identifique las celdas con un valor superior al límite * sd (por columnas)
#' @param data dataframe
#' @param cutoff Parametro de tolerancia para oultiers.
#'
#' @return Una lista de los outliers.
#' @export
#'
#' @examples
#' outlier.find(iris)
#' outliers <- outlier.find(iris)
#' @encoding UTF-8
outlier.find <- function(data, cutoff = 3) {
  ## calcular sd
  sds <- apply(data, 2, sd, na.rm = TRUE)
  ## Identifique las celdas con un valor superior al límite * sd (por columnas)
  result <- mapply(function(d, s) {
    which(d > cutoff * s)
  }, data, sds)
  result
}



#' Ajustar los colores a cierto porcentaje de transparencia
#'
#' Ajustar los colores a cierto porcentaje de transparencia.
#' @param color Un color soportado por R. (p.ej. #66CD007F)
#' @param porcentaje Porcentaje de transparencia.
#' @param nombre Nombre opcional del color
#'
#' @return quitar ceros del vector.
#' @export
#'
#' @examples
#' color_transparente <- t_col("#66CD007F", porcentaje = 50, nombre = "lt.color")
#' color_transparente
#' @encoding UTF-8
#' @importFrom grDevices col2rgb
#' @importFrom grDevices rgb
t_col <- function(color, porcentaje = NULL, nombre = NULL) {
  #	  color = nombre del color
  #	porcentaje = % de transparencia
  #	   nombre = nombre opcional del color
  ##
  rgb.val <- grDevices::col2rgb(color)
  ## aplicar la transparencia
  t.col <- grDevices::rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-porcentaje)*255/100,
               names = nombre)
  ## guardar color
  invisible(t.col)
  print(t.col)
}




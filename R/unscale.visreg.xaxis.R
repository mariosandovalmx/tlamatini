#' Desescalar coeficientes previamente escalados y centrados, para graficar con visreg
#'
#' Permite dibujar las regresiones parciales de un modelo usando la paqueteria visreg. Permite desescalar y descentrar los coeficientes de un modelo ajustado con las variables escaladas. Para convertir la variable del eje x a la escala original. Solo hay que sustituir la "variable"  por la variable en escala original. Luego incluir en la funcion de visreg xtrans = unscale.visreg.xaxis. Se usa para graficar en visreg.
#' @param x var
#'
#' @return coeficientes o medias reconvertidas en su escala original
#' @export
#'
#' @examples
#' #Esta función se usa cuando se quiere graficar los efectos de una variable escalada.
#' #visreg(m2, xtrans = unscale.visreg.xaxis)
#' @encoding UTF-8


unscale.visreg.xaxis<- function(x){
  z<- x * sd("variable") + mean("variable")
  return(z)
}


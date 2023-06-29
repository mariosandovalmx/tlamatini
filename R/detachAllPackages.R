#' Desmontar paquetes cargados en el espacio de trabajo
#'
#'Desmontar todos los paquetes en R. Quita los paquetes cargados en el espacio de trabajo. Útil cuando
#'tenemos las mismas funciones con mismo nombre de diferentes paquetes.
#'
#' @return Quita todos lo paquetes cargados.
#' @export
#'
#' @examples
#' #detach_paquetes()
#' @encoding UTF-8
detach_paquetes <- function() {

  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")

  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]

  package.list <- setdiff(package.list,basic.packages)

  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  insight::print_color("Todos los paquetes desmontados.", "green")


}



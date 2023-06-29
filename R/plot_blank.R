#' Funcion complementaria de auto mfrow tomada de la paqueteria mjcgraphics
#' @description automaticamente elige cuantas columnas y filas se usaran para
#' mostrar las graficas \code{par(mfrow)}. creada por Mark Cowley, 3 June 2006
#' @param main you can optionally write a main title, and a sub-title
#' @param sub you can optionally write a main title, and a sub-title
#' @param message message to write in the middle of the plotting device
#' @param box logical: draw a box around the plot?
#' @keywords internal
#' @export
plot_blank <- function(main=NULL, sub=NULL, message=NULL, box=FALSE) {
  plot(0, type="n", ann=FALSE, xaxt="n", yaxt="n", bty="n", xlim=c(0,1), ylim=c(0,1))

  title(main=main, sub=sub)

  if( !is.null(message) ) {
    text(0.5,0.5, message, adj=0.5)
  }

  if( box ) {
    box()
  }
}

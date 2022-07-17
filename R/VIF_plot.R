#' Grafica el Variance Inflation Factor (VIF) de un modelo
#'
#' Grafica VIF de todas las variables de un modelo.
#' @param modelo Modelo, puede ser LM, GLM, Y GLMM.
#'
#' @return VIF plot
#' @export
#'
#' @examples
#' #VIF_plot(modelo)
#' @encoding UTF-8
#' @import ggplot2

VIF_plot <- function(modelo) {


  if (!requireNamespace("see", quietly = TRUE)) { # nocov start
    stop(
      "Paquete \'see\' es necesario para que la funcion pueda ser usada. Por favor, inst\u00e1lelo y carguela usando: library(see).",
      call. = FALSE
    )
  } # nocov end


    vifdf <- VIF_model(modelo)
    plvif<- plot(vifdf)
    plotvif<- plvif +  labs(title = "Multicolinearidad", subtitle = "Un valor alto de VIF puede inflar la incertidumbre de los parametros", y = "Variance Inflation Factor", x = "") + theme_light() + scale_color_manual(labels = c("Baja (<5)", "Moderada (<10)","Alta(>10)"), values = c("chartreuse4","darkgoldenrod1", "firebrick"))
    plotvif



}


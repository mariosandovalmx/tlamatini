#' Encontrar los outliers de un LM, LMM, GLM
#'
#' Encontrar los outliers de un LM, LMM, GLM.
#' @param model Modelo GLM, GLMM o LM.
#'
#' @return Encontrar los outliers de un modelo y graficarlos.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length, family = gaussian("log"), data=iris)
#' outliers.plot3(modelo)
#' @encoding UTF-8
#' @importFrom stats cooks.distance
#' @importFrom graphics text
#' @importFrom graphics abline
outliers.plot3<- function(model){
  cooksd <- stats::cooks.distance(model)
  plot(cooksd, pch="*", cex=0.5, main="Datos influyentes seg?n distancias de Cook")  # plot cook's distance
  abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
  graphics::text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
  print(cooksd>4*mean(cooksd, na.rm=T))


  insight::print_color("Las distancias de Cook, es una medida de c\u00f3mo influye la observaci\u00f3n identificada como outlier sobre la estimaci\u00f3n de B (pendiente) al ser retirada del conjunto de datos. Una distancia de Cook grande significa que una observaci\u00f3n tiene un peso grande en la estimaci\u00f3n de la pendiente.", "green")

}

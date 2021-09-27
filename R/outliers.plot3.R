#' Encontrar los outliers de un GLM, LM
#'
#' Encontrar los outliers de un GLM, GLMM, etc.
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
outliers.plot3<- function(model){
  cooksd <- stats::cooks.distance(model)
  plot(cooksd, pch="*", cex=0.5, main="Datos influyentes seg?n distancias de Cook")  # plot cook's distance
  abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
  graphics::text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
  print(cooksd>4*mean(cooksd, na.rm=T))
}

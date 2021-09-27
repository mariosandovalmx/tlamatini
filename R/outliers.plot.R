#' Encontrar los outliers de un GLM, GLMM.
#'
#' Encontrar los outliers de un GLM, GLMM O cualquier otro modelo. Además aplica una prueba para saber si
#' son o no datos influyentes.
#' @param model Modelo GLM, GLMM, etc.
#' @param outliers Numero de outliers a mostrar.
#'
#' @return Encontrar los outliers de un modelo y graficarlos.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length, family = gaussian("log"), data=iris)
#' outliers.plot(modelo)
#' @encoding UTF-8
#' @importFrom broom augment
#' @importFrom car outlierTest
#' @import ggplot2
outliers.plot <- function(model,outliers= 3){

  # Extract model results
  model.data <- broom::augment(model) %>%
    mutate(index = 1:n())

  out <- outliers
  if (out < 3) {
    out<- outliers
  } else if (out > 3) {
    out<- outliers
  }


  names(model.data)[names(model.data) == '.cooksd'] <- "cooksd"
  names(model.data)[names(model.data) == '.std.resid'] <- "std.resid"
  model.outliers <- model.data %>%  top_n(out, model.data$cooksd)


  model.realoutliers<- model.data %>%
    filter(abs(model.data$std.resid) > 3)
  message(c(" ################################################################################################"))
  message(c(" ###################################   Outliers reales > 3  ################################"))
  message(c(" ################################################################################################"))
  print(model.realoutliers)


  message(c(" ################################################################################################"))


  message(c("Nota: no todos los outliers son observaciones influyentes. Para revisar que los datos contienen potenciales datos influyentes, se puede inspeccionar los residuales estandarizados. Datos con residuales estandarizados absolutos mayores a 3 representan posibles outliers y merecen atenci?n. Sugerencia: 1) remover, 2)transformar los datos, 3) usar m?todos no param?tricos"))
  message(c(" ################################################################################################"))
  ######

  test <- car::outlierTest(model)
  print(test)
  message(c(" ################################################################################################"))

  p1<- ggplot2::ggplot(model.data, aes(index, std.resid)) +
    ggplot2::geom_point(alpha = .5) +
    ggplot2::theme_bw()+ ggplot2::geom_point(data = model.realoutliers,
                           mapping = aes(x = index, y = std.resid, label=), col= "red", size=3, pch= 12)


  p1



}

#' Encontrar los outliers de un LM, GLM
#'
#' Encontrar los outliers de un LM, GLM, GLMM O cualquier otro modelo. Ademas aplica una prueba para saber si
#' son o no datos influyentes.
#' @param model Modelo LM, GLM, GLMM.
#' @param outliers Numero de outliers a mostrar.
#'
#' @return Encontrar los outliers de un modelo y graficarlos.
#' @details Ver funciones outlierTest
#' @export
#'
#' @examples
#' #data(iris)
#' #modelo <- glm(Petal.Width ~ Petal.Length, family = gaussian("log"), data=iris)
#' #outliers.plot(modelo)
#' @encoding UTF-8
#' @importFrom car outlierTest
#' @import ggplot2
#' @importFrom broom augment

outliers.plot <- function(model,outliers= 3){

  if (!requireNamespace("car", quietly = TRUE)) { # nocov start
    stop(
      "Se necesita el paquete \'car\' para que esta funcion funcione. Por favor, inst\u00e1lelo.",
      call. = FALSE
    )
  } # nocov end





  # Extract model results
  model.data <- broom::augment(model) %>%
    mutate(index = 1:n())

  outliers<- 3
  out <- outliers
  if (out < 3) {
    out<- outliers
  } else if (out > 3) {
    out<- outliers
  }


  names(model.data)[names(model.data) == '.cooksd'] <- "cooksd"
  names(model.data)[names(model.data) == '.std.resid'] <- "StdResid"
  names(model.data)[names(model.data) == 'index'] <- "outlierID"
  names(model.data)


  model.outliers <- model.data %>%  top_n(out, model.data$cooksd)


  model.realoutliers<- model.data %>%
    filter(abs(model.data$StdResid) > 3)
  message(c(" ################################################################################################"))
  insight::print_color("Observaciones con std.resid (residuales estandarizados) > 3, son consideradas valores at\u00edpicos. Index es n\u00famero de fila donde se encuentra la observaci\u00f3n.", "green")
  message(c(" ################################################################################################"))
  print(model.realoutliers)


  message(c(" ################################################################################################"))
  insight::print_color("En el gr\u00e1fico de la derecha se marcan en rojo las observaciones influyentes.", "green")
  insight::print_color("No todos los outliers son observaciones influyentes. Para revisar que los datos contienen potenciales datos influyentes, se puede inspeccionar los residuales estandarizados. Datos con residuales estandarizados absolutos mayores a 3 representan posibles outliers y merecen atenci\u00f3n. Sugerencia: 1) remover, 2)transformar los datos, 3) usar m\u00e9todos no param\u00e9tricos. A continuaci\u00f3n se hace una prueba Bonferroni para cada dato identificado como outlier.", "green")

  message(c(" ################################################################################################"))
  ######

  test <- car::outlierTest(model)
  print(test)
  message(c(" ################################################################################################"))


  p1<- ggplot2::ggplot(model.data, aes(outlierID, StdResid)) +
    ggplot2::geom_point(alpha = .5) +
    ggplot2::theme_bw() +
    ggplot2::geom_point(data = model.realoutliers,aes(x = outlierID, y = StdResid), col= "red", size=3, pch= 12)

  p1



}


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
  message(c(" Observaciones con std.resid (residuales estandarizados) > 3, son consideradas valores atipicos. Index es numero de fila donde se encuentra la observacion."))
  message(c(" ################################################################################################"))
  print(model.realoutliers)


  message(c(" ################################################################################################"))
  message(c("No todos los outliers son observaciones influyentes. Para revisar que los datos contienen potenciales datos influyentes, se puede inspeccionar los residuales estandarizados. Datos con residuales estandarizados absolutos mayores a 3 representan posibles outliers y merecen atencion. Sugerencia: 1) remover, 2)transformar los datos, 3) usar metodos no parametricos. A continuacion se hace una prueba Bonferroni para cada dato identificado como outlier."))
  message(c(" ################################################################################################"))
  ######

  test <- car::outlierTest(model)
  print(test)
  message(c(" ################################################################################################"))
  message(c(" Nota: los acentos fueron removidos intencionalmente."))
  p1<- ggplot2::ggplot(model.data, aes(index, std.resid)) +
    ggplot2::geom_point(alpha = .5) +
    ggplot2::theme_bw()+
    ggplot2::geom_point(data = model.realoutliers,
    mapping = aes(x = index, y = std.resid, label=), col= "red", size=3, pch= 12)


  p1



}

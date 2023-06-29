#' Encontrar outliers de un GLM
#'
#' Encontrar outliers de un GLM. Usa las distancias de Cook e identifica los outliers. Ver la funcion cooks.distance, para mas detalles.
#' @param model Modelo con GLM y LM.
#' @param dataframe Dataframe con el cual se ajustó el modelo.
#'
#' @return Un grafico de outliers y una lista con los outliers detectados.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length, family = gaussian("log"), data=iris)
#' outliers.glm(modelo, iris)
#' @encoding UTF-8
#' @importFrom stats cooks.distance
outliers.glm <- function(model, dataframe){
  cooksd <- stats::cooks.distance(model)
  sample_size <- nrow(dataframe)
  influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
  insight::print_color("Las observaciones m\u00e1s influyentes, basandonos en las distancias de Cook son: ", "green")
  print(influential)
}



#' Encontrar outliers de un GLM
#'
#' Encontrar outliers de un GLM. Basandonos en las distancias de Cook identifica los outliers.
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
  print(influential)
}



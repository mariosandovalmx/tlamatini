#' Encontrar los outliers de un LM, GLM
#'
#' Encontrar los outliers de un LM, GLM.
#' @param model Modelo GLM, o LM.
#' @param fill Color predefinido
#' @param outline Outline predefinido
#' @param size tamaño predefinido
#'
#' @return Encontrar los outliers de un modelo y graficarlos.
#' @export
#'
#' @examples
#' data(iris)
#' modelo <- glm(Petal.Width ~ Petal.Length, family = gaussian("log"), data=iris)
#' outliers.plot2(modelo)
#' @encoding UTF-8
#' @importFrom stats rstudent
#' @importFrom stats hatvalues
#' @importFrom stats cooks.distance
outliers.plot2 <- function (model, fill="white",
                            outline="black", size=30) {

  if(!inherits(model, "lm"))
    stop("Es necesario suministrar un objeto lm.")
  df<-data.frame(Residual=stats::rstudent(model),
                 Leverage=stats::hatvalues(model),
                 Cooks=stats::cooks.distance(model),
                 Observation=names(hatvalues(model)),
                 stringsAsFactors=FALSE)
  myxint<-c(2*mean(df$Leverage), 3*mean(df$Leverage))
  inds<-intersect(which(abs(df$Residual) < 2),
                  which( df$Leverage < myxint[1]))
  if(length(inds) > 0) df$Observation[inds]<-""
  ggplot2::ggplot(df, aes_string(x='Leverage', y='Residual',
                        size='Cooks', label='Observation'),
         legend=FALSE) +
    ggplot2::geom_point(colour=outline, fill=fill, shape=21) +
    ggplot2:: scale_size_area(max_size=size) +
    ggplot2::theme_bw(base_size=16) + geom_text(size=4) +
    ggplot2::geom_hline(yintercept=c(2,-2), linetype="dashed") +
    ggplot2::geom_vline(xintercept=myxint, linetype="dashed") +
    ggplot2::ylab("Studentized Residuals") +
    ggplot2::xlab("Hat-Values") + ggplot2::labs(size="Cook's distance")
  message(c(" Las distancias de Cook, es una medida de como influye la observacin identificada como outlier sobre la estimacion de B (pendiente) al ser retirada del conjunto de datos. Una distancia de Cook grande significa que una observacion tiene un peso grande en la estimacion de la pendiente. En el grafico las observaciones con circulos grandes son consideradas outliers influyentes."))
  message(c(" Nota: los acentos fueron removidos intencionalmente."))
}



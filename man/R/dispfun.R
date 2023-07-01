#' Parametro de dispersion de un modelo poisson, o binomial negativo
#'
#' Calcular parametro de dispersión para poisson, binomial negativo y Quasipoison de la paqueteria lme4,
#' glmer, glmmTMB, etc. GLMM y GLM. Más detalles en:  https://github.com/glmmTMB/glmmTMB/issues/224
#' @param modelo Modelo con distribución Poisson, Quasipoisson, binomial negativo.
#'
#' @return Un valor de dispersión.
#' @export
#'
#' @examples
#' data<- warpbreaks
#' modelo <- glm(breaks ~ wool + tension, family= poisson("log"), data= data)
#' dispfun(modelo)
#' @encoding UTF-8
#' @importFrom stats residuals
#' @importFrom stats df.residual
#' @importFrom insight export_table

dispfun <- function(modelo) {
  insight::print_color("Si el valor es mayor a 1 indica que el modelo presenta sobredispersi\u00f3n. Si es menor a uno podr\u00eda indicar subdispersi\u00f3n", "green")
  r <- residuals(modelo,type="pearson")
  n <- df.residual(modelo)
  dsq <- sum(r^2)
  df<- data.frame(dsq=dsq,n=n,dispersion=dsq/n)

  cat(insight::export_table(df, title ="."))

}


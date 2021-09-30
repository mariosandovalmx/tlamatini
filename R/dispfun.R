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
#' modelo <- glm(breaks ~ wool+tension, poisson, data= data)
#' dispfun(modelo)
#' @encoding UTF-8
#' @importFrom stats residuals
#' @importFrom stats df.residual
dispfun <- function(modelo) {
  message("Si el valor es mayor a 1 indica que el modelo presenta sobredispersion.", "\n")

  r <- residuals(modelo,type="pearson")
  n <- df.residual(modelo)
  dsq <- sum(r^2)
  c(dsq=dsq,n=n,dispersion=dsq/n)

}
# dispfun(m4)

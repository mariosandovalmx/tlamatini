#' Parámetro de dispersión de un modelo poisson, o binomial negativo
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
#' dispfun2(modelo)
#' @encoding UTF-8
#' @importFrom stats pchisq
dispfun2 <- function(modelo) {
  message("Si el valor de ratio es mayor indica que los datos estan sobredispersos, es mejor usar un modelo quasipoisson o binomial negativo.",  "\n")
  rdf <- df.residual(modelo)
  rp <- residuals(modelo,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)

}

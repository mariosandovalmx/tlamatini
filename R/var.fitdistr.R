#' Ajustar una variable a cierta distribucion de nuestro interes.
#'
#' Permite conocer ajustar una variable a cierta distribución de nuestro interes.
#' @param variable Variable de interes.
#' @param distr Distribución a probar: "norm", "lnorm", "pois", "exp", "gamma", "nbinom", "geom", "beta",
#' "unif" and "logis"
#'
#' @return coeficientes o medias reconvertidas en su escala original
#' @export
#'
#' @examples
#' #x<- rnorm(1000,313, 5.7)
#' #var.fitdistr(x, distr = "norm")
#' @encoding UTF-8
#' @importFrom fitdistrplus fitdist
#' @importFrom fitdistrplus gofstat

var.fitdistr <- function(variable, distr){

  if(distr== c("norm", "lnorm", "pois", "exp", "gamma", "nbinom", "geom", "beta", "unif", "logis")){

    fitg<-fitdistrplus::fitdist(variable, distr=distr,method="mme",keepdata = TRUE)
    summary(fitg)
    plot(fitg, demp = TRUE)
  } else {

  fitg<-fitdistrplus::fitdist(variable, distr=distr,method="mle",keepdata = TRUE)
  summary(fitg)
  plot(fitg, demp = TRUE)

  return(gofstat(fitg))
  }
}

#

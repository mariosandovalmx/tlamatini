#' Graficos de dispersion de un modelo
#'
#' Graficar la sobredispersion de un modelo poisson, binomial negativo y Quasipoison de la paqueteria lme4,
#' glmer, glmmTMB, etc. GLMM y GLM. Más detalles en:  https://github.com/glmmTMB/glmmTMB/issues/224
#' @param modelo Modelo con distribución Poisson, Quasipoisson, binomial negativo.
#' @param overdispersion.term Parámetro de sobredispersión.
#' @param type Tipo "pearson" por default.
#'
#' @return Gráficos de dispersion de un modelo.
#' @export
#'
#' @examples
#' data<- warpbreaks
#' modelo <- glm(breaks ~ wool+tension, poisson, data= data)
#' dispersion.plot(modelo)
#' @encoding UTF-8
#' @importFrom stats resid
#' @importFrom stats fitted
#' @importFrom stats model.frame
#' @importFrom stats predict
#' @importFrom  stats loess
#' @importFrom graphics par
#' @importFrom graphics abline
#' @importFrom graphics points
#' @importFrom  graphics hist
#' @importFrom  AICcmodavg fam.link.mer
dispersion.plot<-function(modelo,type="pearson",overdispersion.term=NULL)
{
  if(is.null(overdispersion.term))
  {
    Fitted<-stats::fitted(modelo)
    Residuals= stats::resid(modelo,type)
  } else
  {
    response<-model.frame(modelo)[[1]]
    od.ranef<-lme4::ranef(modelo)[[overdispersion.term]][[1]]
    if(length(response)!=length(od.ranef) || AICcmodavg::fam.link.mer(modelo)$family!="poisson" || fam.link.mer(modelo)$link!="log")
      stop("Model is not lognormal-Poisson. Cannot use overdispersion term.")
    Fitted<-exp(log(fitted(modelo))-od.ranef)
    Residuals<-(response - Fitted)/sqrt(Fitted+(Fitted^2)*c(exp(lme4::VarCorr(modelo)[[overdispersion.term]])-1))
  }
  plot.data<-data.frame(Fitted=Fitted,Residuals=Residuals)
  plot.data$loess.line<-predict(loess(Residuals~Fitted,data=plot.data))
  plot.data<-plot.data[order(plot.data$Fitted),]
  par(mfrow=c(1,2))
  plot(plot.data[,c("Fitted","Residuals")])
  abline(h=0)
  points(plot.data[,c("Fitted","loess.line")],type="l",col="red")
  hist(plot.data$Residuals,xlab="Residuals",main="")
}

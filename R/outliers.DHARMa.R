#' Encontrar outliers de un modelo con paqueteria DHARMa
#'
#' Encontrar outliers con paqueteria DHARMa.
#' @param Modelo Modelo con lme4, glmer, glmmTMB, etc. GLMM y GLM.
#'
#' @return Un grafico de outliers y una lista con los outliers detectados.
#' @export
#'
#' @examples
#' #datos <- datasets::ChickWeight
#' #library(lme4)
#' #modelo <- glmer(weight ~ Diet +(1|Chick), family=gaussian("log"), data = datos)
#' #outliers.DHARMa(modelo)
#' @encoding UTF-8
#' @importFrom DHARMa simulateResiduals
#' @importFrom DHARMa testOutliers
#' @importFrom DHARMa testQuantiles
outliers.DHARMa <- function(Modelo){
  res.mod <-DHARMa::simulateResiduals(Modelo,n = 20000)

  message(c("Si el valor de p < 0.05, entonces hay observaciones influyentes en nuestros datos. Que hacer con los outlieres? algunos investigadores sugieren remover algunos outliers (no todos), de esta forma se puede lograr un mejor ajuste del modelo."))


  par(mfrow=c(1,2))
  pl1<- DHARMa::testOutliers(res.mod)
  print(pl1)
  pl2<- DHARMa::testQuantiles(res.mod)


  message(c("Esta funcion usa bootstrap para simular los outliers (posibles) basandose en la simulacion de los valores atipicos. Las observaciones mas influyentes son:
  "))
  #los outlieres son estos
  which(residuals(res.mod) == 1 | residuals(res.mod) == 0)
  #print(c("Siendo menos estrictos con la definici?n de outlier:"))
  #which(residuals(res.mod) >0.99 | residuals(res.mod) < 0.01)

}

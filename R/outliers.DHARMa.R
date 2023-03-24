#' Encontrar outliers de un modelo con paqueteria DHARMa
#'
#' Encontrar outliers con paqueteria DHARMa. Esta función comprueba si el número de observaciones fuera de la simulación es mayor o menor de lo esperado. Ver función "testOutliers" de la paquetería DHARMa para más detalles.
#' @param Modelo Modelo con lme4, glmer, glmmTMB, etc. GLMM, GLM y GAM.
#' @param nsim Número de simulaciones. Cuanto menor sea el número, mayor será el error estocástico en los residuales.
#' Además, para n muy pequeño, los artefactos de discretización pueden influir en las pruebas. Por defecto es 250, que
#' es un valor relativamente seguro. Puede considerar aumentar a 1000 para estabilizar los valores simulados.
#'
#' @return Un grafico de outliers y una lista con los outliers detectados.
#' @export
#'
#' @examples
#' #
#' #datos <- datasets::ChickWeight
#' #library(glmmTMB)
#' #modelo <- glmmTMB(weight ~ Diet +(1|Chick), family=gaussian("log"), data = datos)
#' #outliers.DHARMa(modelo)
#' @encoding UTF-8
#' @importFrom DHARMa simulateResiduals
#' @importFrom DHARMa testOutliers
#' @importFrom DHARMa testQuantiles
outliers.DHARMa <- function(Modelo, nsim=NULL){
  if(is.null(nsim) ){
  res.mod <-DHARMa::simulateResiduals(Modelo,n = 250)

  insight::print_color("Si el valor de p < 0.05, entonces hay observaciones influyentes en nuestros datos. \u00bfQue hacer con los outlieres? algunos investigadores sugieren remover algunos outliers (no todos), de esta forma se puede lograr un mejor ajuste del modelo.", "green")

  par(mfrow=c(1,2))
  pl1<- DHARMa::testOutliers(res.mod)
  print(pl1)
  pl2<- DHARMa::testQuantiles(res.mod)

  insight::print_color("Esta funci\u00f3n usa bootstrap para simular los outliers (posibles) bas\u00e1ndose en la simulaci\u00f3n de los valores at\u00edpicos. Las observaciones mas influyentes son:", "green")

  #los outlieres son estos
  which(residuals(res.mod) == 1 | residuals(res.mod) == 0)
  #print(c("Siendo menos estrictos con la definici?n de outlier:"))
  #which(residuals(res.mod) >0.99 | residuals(res.mod) < 0.01)
  } else if(is.numeric(nsim)){
    n.sim<- nsim
    res.mod <-DHARMa::simulateResiduals(Modelo,n = n.sim)

    insight::print_color("Si el valor de p < 0.05, entonces hay observaciones influyentes en nuestros datos. \u00bfQue hacer con los outlieres? algunos investigadores sugieren remover algunos outliers (no todos), de esta forma se puede lograr un mejor ajuste del modelo.", "green")


    par(mfrow=c(1,2))
    pl1<- DHARMa::testOutliers(res.mod)
    print(pl1)
    pl2<- DHARMa::testQuantiles(res.mod)


    insight::print_color("Esta funci\u00f3n usa bootstrap para simular los outliers (posibles) bas\u00e1ndose en la simulaci\u00f3n de los valores at\u00edpicos. Las observaciones mas influyentes son:", "green")


    #los outlieres son estos
    which(residuals(res.mod) == 1 | residuals(res.mod) == 0)
    #print(c("Siendo menos estrictos con la definici?n de outlier:"))
    #which(residuals(res.mod) >0.99 | residuals(res.mod) < 0.01)
  }
}

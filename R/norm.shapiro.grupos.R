#' Prueba de normalidad de Shapiro-Wilk por grupos.
#'
#' Realiza la prueba de normalidad de Shapiro-Wilk por grupos de un dataframe. Se pueden seleccionar una o más variables a la vez.
#' @param formula Formula iniciando por la variable numerica de nuestro interes. Puede tomar forma de y ~ x+z. Se pueden usar 2 o más grupos.
#' @param datos Base de datos o dataframe.
#'
#' @return Una prueba Shapiro-Wilk por grupos
#' @export
#'
#' @examples
#' #data(iris)
#' #norm.shapiro.grupos(Petal.Length~ Species, iris)
#' #iris$site <- c(rep("A", 75), rep("B", 75))
#' #norm.shapiro.grupos(Petal.Length~ Species+ site, iris)
#' @encoding UTF-8
#' @importFrom stats aggregate
#' @importFrom stats as.formula
#' @importFrom car qqPlot

norm.shapiro.grupos <- function(formula, datos){

  form <- as.formula(formula)
  resp.var<- as.character(form)[2]

  #  calcular el valor de W
  res<- aggregate(form, data = datos, FUN =
                    function(x) shapiro.test(x)$statistic)
  res
  names(res)[names(res) == resp.var] <- "W.statistic"
  res
  #calcular el valor de p
  resb<- aggregate(form, data = datos, FUN =
                     function(x) shapiro.test(x)$p.value)
  names(resb)[names(resb) == resp.var] <- "p.value"
  resb
  #unir los dos resultados en una tabla
  results<- merge(x = res, y = resb, all.x = TRUE)
  message(c("Prueba de normalidad de Shapiro-Wilk por grupos"))
  return(results)
  # funcion necesaria para correr auto.mfrow
  plot_blank <- function(main=NULL, sub=NULL, message=NULL, box=FALSE) {
    plot(0, type="n", ann=FALSE, xaxt="n", yaxt="n", bty="n", xlim=c(0,1), ylim=c(0,1))

    title(main=main, sub=sub)

    if( !is.null(message) ) {
      text(0.5,0.5, message, adj=0.5)
    }

    if( box ) {
      box()
    }
  }
  #AUTO numero de filas y columnas en graficas. Tomada de la paquetería mjcgraphics
  auto.mfrow <- function(nplots, setup=TRUE) {

    if(setup) {
      if(nplots <= 3) par(mfrow=c(1, nplots))
      else if(nplots <= 4)  par(mfrow=c(2,2))
      else if(nplots <= 6)  par(mfrow=c(2,3))
      else if(nplots <= 9)  par(mfrow=c(3,3))
      else if(nplots <= 12) par(mfrow=c(3,4))
      else if(nplots <= 16) par(mfrow=c(4,4))
      else if(nplots <= 20) par(mfrow=c(4,5))
      else if(nplots <= 25) par(mfrow=c(5,5))
      else if(nplots <= 30) par(mfrow=c(5,6))
      else if(nplots <= 36) par(mfrow=c(6,6))
      else if(nplots <= 42) par(mfrow=c(6,7))
      else if(nplots <= 49) par(mfrow=c(7,7))
      else if(nplots <= 56) par(mfrow=c(7,8))
      else if(nplots <= 64) par(mfrow=c(8,8))
      else {
        stop("Demasiadas graficas")
      }
    }
    else {
      nblankplots <- par("mfrow")[1] * par("mfrow")[2] - nplots
      if(nblankplots > 0)
        for(i in 1:nblankplots)
          plot_blank()
    }
  }

  auto.mfrow(nrow(res), setup = TRUE)
  # graficar qqplot
  plots<- aggregate(form, data = datos, FUN =
                      function(x) car::qqPlot(x,glab=deparse(substitute(groups)), ylab=deparse(substitute(x))))
  message(c("Las graficas se muestran en orden de aparicion como se muestran las filas."))


}

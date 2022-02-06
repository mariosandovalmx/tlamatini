#' Contrastes post hoc de una matriz de datos con chi-cuadrada
#'
#' Muestra las comparaciones post-hoc por pares de una prueba Chi cuadrada.
#' Realiza la prueba exacta de Fisher para comprobar independencia de filas en una tabla de contingencia, luego de obtener una prueba chi-cuadrada significativa.
#'
#' Función tomada y modificada de la paqueteria rstatix. https://cran.r-project.org/web/packages/rstatix/index.html
#' @param tabla Una tabla o matriz, la misma que se usó con la función chisq.test
#' @param p.adjust.method Método usado para ajustar el valor de P. Se puede usar: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", y "none".
#' @param  alternative Un argumento que indica la hipótesis alternativa que se probará.
#' @return Contrastes 'post-hoc' de chi cuadrada para dos o mas filas usando la prueba Fisher.test.
#' @export
#'
#' @examples
#' xtab <- as.table(rbind(c(180, 145), c(179, 106),c(510, 196), c(862, 23)))
#' dimnames(xtab) <- list(
#' Tratamiento = c("1st", "2nd", "3rd", "Control"),
#' Genero = c("hembra", "macho"))
#' chisq.post.hoc.filas(xtab)
#' chisq.post.hoc.filas(xtab, alternative = "less")
#' chisq.post.hoc.filas(xtab, alternative = "greater")
#' chisq.post.hoc.filas(xtab, alternative = "two.sided")
#' chisq.post.hoc.filas(xtab, p.adjust.method = "BY", alternative = "less")
#' chisq.post.hoc.filas(xtab, p.adjust.method = "BY", alternative = "two.sided")
#' chisq.post.hoc.filas(xtab, p.adjust.method = "holm", alternative = "less")
#' chisq.post.hoc.filas(xtab, p.adjust.method = "bonferroni")
#' chisq.post.hoc.filas(xtab, p.adjust.method = "BY")
#' @encoding UTF-8
#' @importFrom rstatix row_wise_fisher_test


chisq.post.hoc.filas <- function(tabla, p.adjust.method = NULL, alternative = NULL) {




  if(is.null(p.adjust.method) && is.null(alternative)){


    #### CAPTURAMOS EL ERROR Y DETENEMOS LA FUNCIÓN
    #x <- tryCatch(
    #  { # AQUI VA LA FUNCIÓN QUE VAMOS A PROBAR SI TIENE ERROR
    #    for (i in 1:tests) {
    #      pvals[i] <- test(tbl[prs[,i],])$p.value
    #      lbls[i] <- paste(popsNames[prs[,i]],collapse=" vs. ")
    #    }
    #  },
    #  error = function(e){
    #    stop("Error: algunas celdas tienen pocas observaciones. Considera ajustar agregando el argumento 'simulate.p.value = TRUE' ")
    #  }
    #)



    tabf <- rstatix::row_wise_fisher_test(tabla, p.adjust.method = "holm", alternative = "two.sided")
    names(tabf) <- c("grupo","n","p","p.ajust.","significancia")
    options(scipen = 999)
    tabf2<- as.data.frame(tabf)
    tabf2[,4]  <- format(tabf2[,4], scientific = FALSE)
    tabf2[,4]  <- as.numeric(substr(tabf2[,4]  , start = 1, stop = 5))
    tabf2[,4][tabf2[,4]   < 0.001] <- "<0.001"
    tabf2[,3]  <- format(tabf2[,3], scientific = FALSE)
    tabf2[,3]  <- as.numeric(substr(tabf2[,3]  , start = 1, stop = 5))
    tabf2[,3][tabf2[,3]   < 0.001] <- "<0.001"
    options(scipen=0)
    return(tabf2)



  } else if (is.null(p.adjust.method) ){

    tabf <- rstatix::row_wise_fisher_test(tabla, p.adjust.method = "holm", alternative = alternative)
    names(tabf) <- c("grupo","n","p","p.ajust.","significancia")
    options(scipen = 999)
    tabf2<- as.data.frame(tabf)
    tabf2[,4]  <- format(tabf2[,4], scientific = FALSE)
    tabf2[,4]  <- as.numeric(substr(tabf2[,4]  , start = 1, stop = 5))
    tabf2[,4][tabf2[,4]   < 0.001] <- "<0.001"
    tabf2[,3]  <- format(tabf2[,3], scientific = FALSE)
    tabf2[,3]  <- as.numeric(substr(tabf2[,3]  , start = 1, stop = 5))
    tabf2[,3][tabf2[,3]   < 0.001] <- "<0.001"
    options(scipen=0)
    return(tabf2) } else if (is.null(alternative) ){

      tabf <- rstatix::row_wise_fisher_test(tabla, p.adjust.method = p.adjust.method, alternative = "two.sided")
      names(tabf) <- c("grupo","n","p","p.ajust.","significancia")
      options(scipen = 999)
      tabf2<- as.data.frame(tabf)
      tabf2[,4]  <- format(tabf2[,4], scientific = FALSE)
      tabf2[,4]  <- as.numeric(substr(tabf2[,4]  , start = 1, stop = 5))
      tabf2[,4][tabf2[,4]   < 0.001] <- "<0.001"
      tabf2[,3]  <- format(tabf2[,3], scientific = FALSE)
      tabf2[,3]  <- as.numeric(substr(tabf2[,3]  , start = 1, stop = 5))
      tabf2[,3][tabf2[,3]   < 0.001] <- "<0.001"
      options(scipen=0)
      return(tabf2) }




  else {
    #### ajuste de valor de p
    p.adjust <- c(p.adjust.method)

    #### hipotesis alternativa
    alterhyp = c(alternative)

    tabf <- rstatix::row_wise_fisher_test(tabla, p.adjust.method = p.adjust, alternative = alterhyp)
    names(tabf) <- c("grupo","n","p","p.ajust.","significancia")
    options(scipen = 999)
    tabf2<- as.data.frame(tabf)
    tabf2[,4]  <- format(tabf2[,4], scientific = FALSE)
    tabf2[,4]  <- as.numeric(substr(tabf2[,4]  , start = 1, stop = 5))
    tabf2[,4][tabf2[,4]   < 0.001] <- "<0.001"
    tabf2[,3]  <- format(tabf2[,3], scientific = FALSE)
    tabf2[,3]  <- as.numeric(substr(tabf2[,3]  , start = 1, stop = 5))
    tabf2[,3][tabf2[,3]   < 0.001] <- "<0.001"
    options(scipen=0)
    return(tabf2)

  }
}




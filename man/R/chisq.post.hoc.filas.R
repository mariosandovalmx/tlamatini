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
#' #solo hipótesis alternativa
#' chisq.post.hoc.filas(xtab, alternative = "less")
#' chisq.post.hoc.filas(xtab, alternative = "greater")
#' chisq.post.hoc.filas(xtab, alternative = "two.sided")
#' #con los dos argumentos
#' chisq.post.hoc.filas(xtab, p.adjust.method = "BY", alternative = "less")
#' chisq.post.hoc.filas(xtab, p.adjust.method = "BY", alternative = "two.sided")
#' chisq.post.hoc.filas(xtab, p.adjust.method = "holm", alternative = "less")
#' @encoding UTF-8
#' @importFrom rstatix row_wise_fisher_test
chisq.post.hoc.filas <- function(tabla, p.adjust.method = NULL, alternative = NULL) {

  if(is.null(p.adjust.method) && is.null(alternative)){

    tabf <- rstatix::row_wise_fisher_test(tabla, p.adjust.method = "bonferroni", alternative = "two.sided")
    names(tabf) <- c("grupo","n","p","p.ajust.","significancia")
    options(scipen = 999)
    tabf2<- as.data.frame(tabf)
    tabf2[,4]  <- format(tabf2[,4], scientific = FALSE)
    tabf2[,4]  <- as.numeric(substr(tabf2[,4]  , start = 1, stop = 5))
    tabf2[,4][tabf2[,4]   < 0.001] <- "<0.001"
    tabf2[,4][tabf2[,4]   < 0.001] <- "<0.001"
    tabf2[,3]  <- format(tabf2[,3], scientific = FALSE)
    tabf2[,3]  <- as.numeric(substr(tabf2[,3]  , start = 1, stop = 5))
    tabf2[,3][tabf2[,3]   < 0.001] <- "<0.001"

    tabf2<- tabf2[,-5]
    options(scipen=0)
    if (length(tabf2)) {
      cat("\n")
      texto<- paste("Los valores de p fueron ajustados con el metodo:", "bonferroni,", "y la hip\u00f3tesis alternativa:", "two.sided")
      insight::print_color(texto, "green")
      insight::export_table(tabf2, align= "right", title = "--", format = "markdown")
      #print.data.frame(dfs, row.names = FALSE)
    } } else if(is.null(p.adjust.method) && isTRUE(is.character(alternative))) {
      #### ajuste de valor de p
      p.adjust <- p.adjust.method

      #### hipotesis alternativa
      alterhyp =  alternative

      tabf <- rstatix::row_wise_fisher_test(tabla, p.adjust.method = "bonferroni", alternative = alterhyp)
      names(tabf) <- c("grupo","n","p","p.ajust.","significancia")
      options(scipen = 999)
      tabf2<- as.data.frame(tabf)
      tabf2[,4]  <- format(tabf2[,4], scientific = FALSE)
      tabf2[,4]  <- as.numeric(substr(tabf2[,4]  , start = 1, stop = 5))
      tabf2[,4][tabf2[,4]   < 0.001] <- "<0.001"
      tabf2[,3]  <- format(tabf2[,3], scientific = FALSE)
      tabf2[,3]  <- as.numeric(substr(tabf2[,3]  , start = 1, stop = 5))
      tabf2[,3][tabf2[,3]   < 0.001] <- "<0.001"
      tabf2<- tabf2[,-5]
      options(scipen=0)
      if (length(tabf2)) {
        cat("\n")
        texto<- paste("Los valores de p fueron ajustados con el metodo:", "bonferroni", "y la hip\u00f3tesis alternativa:", alterhyp)
        insight::print_color(texto, "green")
        insight::export_table(tabf2, align= "right", title = "--", format = "markdown")
      }
    } else if(isTRUE(is.character(p.adjust.method)) && is.null(alternative)) {
      #### ajuste de valor de p
      p.adjust <- p.adjust.method

      #### hipotesis alternativa
      alterhyp =  alternative

      tabf <- rstatix::row_wise_fisher_test(tabla, p.adjust.method = p.adjust, alternative = "two.sided")
      names(tabf) <- c("grupo","n","p","p.ajust.","significancia")
      options(scipen = 999)
      tabf2<- as.data.frame(tabf)
      tabf2[,4]  <- format(tabf2[,4], scientific = FALSE)
      tabf2[,4]  <- as.numeric(substr(tabf2[,4]  , start = 1, stop = 5))
      tabf2[,4][tabf2[,4]   < 0.001] <- "<0.001"
      tabf2[,3]  <- format(tabf2[,3], scientific = FALSE)
      tabf2[,3]  <- as.numeric(substr(tabf2[,3]  , start = 1, stop = 5))
      tabf2[,3][tabf2[,3]   < 0.001] <- "<0.001"
      tabf2<- tabf2[,-5]
      options(scipen=0)
      if (length(tabf2)) {
        cat("\n")
        texto<- paste("Los valores de p fueron ajustados con el metodo:", p.adjust, "y la hip\u00f3tesis alternativa:", alterhyp)
        insight::print_color(texto, "green")
        insight::export_table(tabf2, align= "right", title = "--", format = "markdown")
      }
    } else {

      tabf <- rstatix::row_wise_fisher_test(tabla, p.adjust.method = p.adjust.method, alternative = alternative)
      names(tabf) <- c("grupo","n","p","p.ajust.","significancia")
      options(scipen = 999)
      tabf2<- as.data.frame(tabf)
      tabf2[,4]  <- format(tabf2[,4], scientific = FALSE)
      tabf2[,4]  <- as.numeric(substr(tabf2[,4]  , start = 1, stop = 5))
      tabf2[,4][tabf2[,4]   < 0.001] <- "<0.001"
      tabf2[,3]  <- format(tabf2[,3], scientific = FALSE)
      tabf2[,3]  <- as.numeric(substr(tabf2[,3]  , start = 1, stop = 5))
      tabf2[,3][tabf2[,3]   < 0.001] <- "<0.001"
      tabf2<- tabf2[,-5]
      options(scipen=0)
      if (length(tabf2)) {
        cat("\n")
        texto<- paste("Los valores de p fueron ajustados con el metodo:", p.adjust.method, "y la hip\u00f3tesis alternativa:", alternative)
        insight::print_color(texto, "green")
        insight::export_table(tabf2, align= "right", title = "--", format = "markdown")
      }
    }
}

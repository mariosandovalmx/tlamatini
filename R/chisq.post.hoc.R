#' Contrastes post hoc de una matriz de datos con chi-cuadrada
#'
#' Muestra las comparaciones post-hoc por pares de una prueba Chi cuadrada significativa. Calcula los contrastes si hay más de dos grupos. Se aplica una prueba de fisher individual para saber que grupos difieren. Los valores p ajustados pueden calcularse con una amplia variedad de métodos: fdr, BH, BY, bonferroni, holm, hochberg y hommel. Funciona básicamente como una función envolvente que envía los valores p "brutos" no ajustados de cada prueba de chi-cuadrado por pares a la función p.adjust en el programa R base. La función p.adjust debe ser consultada para una mayor descripción de los métodos utilizados. Función tomada y modificada de la paqueteria fifer. https://www.rdocumentation.org/packages/fifer/versions/1.1
#' @param tabla Una tabla o matriz usada para la función chisq.test
#' @param test Método usado para ajustar el valor de P.
#' @param  popsInRows Un argumento lógico que indica si las poblaciones forman las filas de la tabla (por defecto =TRUE) o no (=FALSE).
#' @param control Indica el método de control a utilizar. fdr, BH, BY, bonferroni, holm, hochberg y hommel.
#' @param digits Número de digitos a mostrar.
#' @param simulate.p.value Si argumento es TRUE, indica que se calculan los valores p mediante la simulación de Monte Carlo y 5000 replicas. Para más información consulta la funcion 'chisq.test'.
#'
#' @return Contrastes 'post-hoc' de chi cuadrada para dos niveles usando Fisher.test.
#' @export
#'
#' @examples
#' table <- as.table(rbind(c(14, 43), c(23, 47)))
#' rownames(table) <- c("Hembras", "Machos")
#' chisq.post.hoc(table,test = c("fisher.test"))
#'
#' xtab <- as.table(rbind(c(180, 145), c(179, 106),c(510, 196), c(862, 23)))
#' dimnames(xtab) <- list(
#' Tratamiento = c("1st", "2nd", "3rd", "Control"),
#' Genero = c("hembra", "macho"))
#' chisq.post.hoc(xtab)
#' chisq.post.hoc(xtab,control = c("fdr"))
#' chisq.post.hoc(xtab,control = c("BY"))
#' chisq.post.hoc(xtab,control = c("bonferroni"))
#' chisq.post.hoc(xtab,control = c("bonferroni"), simulate.p.value = TRUE)
#' @encoding UTF-8
#'
chisq.post.hoc <- function(tabla, test=c("fisher.test"), popsInRows=TRUE, control=c("fdr","BH","BY","bonferroni","holm","hochberg","hommel"),digits=4, simulate.p.value= NULL) {
  if(is.null(simulate.p.value)){

    #### extract correction method
    control <- match.arg(control)

    #### extract which test (fisher or chi square)
    test = match.fun(test)

    #### test rows or columns
    if (!popsInRows) tabla <- t(tabla)
    popsNames <- rownames(tabla)

    #### come up with all possible comparisons
    prs <- utils::combn(1:nrow(tabla),2)

    #### preallocate
    tests <- ncol(prs)
    pvals <- numeric(tests)
    lbls <- character(tests)

    #### CAPTURAMOS EL ERROR Y DETENEMOS LA FUNCIÓN
    x <- tryCatch(
      { # AQUI VA LA FUNCIÓN QUE VAMOS A PROBAR SI TIENE ERROR
        for (i in 1:tests) {
          pvals[i] <- test(tabla[prs[,i],])$p.value
          lbls[i] <- paste(popsNames[prs[,i]],collapse=" vs. ")
        }
      },
      error = function(e){
        stop("Error: algunas celdas tienen pocas observaciones. Considera ajustar agregando el argumento 'simulate.p.value = TRUE' ")
      }
    )
    adj.pvals <- stats::p.adjust(pvals,method=control)
    dfs<- data.frame(Comparar=lbls,p=round(pvals,digits),p.ajust=round(adj.pvals,digits))
    dfs[,3]  <- format(dfs[,3], scientific = FALSE)
    dfs[,3]  <- as.numeric(substr(dfs[,3]  , start = 1, stop = 5))
    dfs[,3][dfs[,3]   < 0.001] <- "<0.001"
    dfs[,2]  <- format(dfs[,2], scientific = FALSE)
    dfs[,2]  <- as.numeric(substr(dfs[,2]  , start = 1, stop = 5))
    dfs[,2][dfs[,2]   < 0.001] <- "<0.001"

    if (length(dfs)) {
      cat("\n")
      texto<- paste("Los valores de p fueron ajustados con el metodo:", control)
      insight::print_color(texto, "green")
      insight::export_table(dfs, align= "right", title = "--", format = "markdown")
      #print.data.frame(dfs, row.names = FALSE)
    }
  } else if (simulate.p.value == TRUE | T){

    #### extract correction method
    control <- match.arg(control)

    #### extract which test (fisher or chi square)
    test = match.fun(test)

    #### test rows or columns
    if (!popsInRows) tabla <- t(tabla)
    popsNames <- rownames(tabla)

    #### come up with all possible comparisons
    prs <- utils::combn(1:nrow(tabla),2)

    #### preallocate
    tests <- ncol(prs)
    pvals <- numeric(tests)
    lbls <- character(tests)
    for (i in 1:tests) {
      pvals[i] <- test(tabla[prs[,i],], simulate.p.value=TRUE, B=5000)$p.value
      lbls[i] <- paste(popsNames[prs[,i]],collapse=" vs. ")
    }
    adj.pvals <- stats::p.adjust(pvals,method=control)
    message("Los valores de p fueron ajustados con el metodo",control)
    dfs<- data.frame(Comparar=lbls,p=round(pvals,digits),p.ajust=round(adj.pvals,digits))
    dfs[,3]  <- format(dfs[,3], scientific = FALSE)
    dfs[,3]  <- as.numeric(substr(dfs[,3]  , start = 1, stop = 5))
    dfs[,3][dfs[,3]   < 0.001] <- "<0.001"
    dfs[,2]  <- format(dfs[,2], scientific = FALSE)
    dfs[,2]  <- as.numeric(substr(dfs[,2]  , start = 1, stop = 5))
    dfs[,2][dfs[,2]   < 0.001] <- "<0.001"
    dfs<- dplyr::as_tibble(dfs)

    if (length(dfs)) {
      cat("\n")
      texto<- paste("Los valores de p fueron ajustados con el metodo:", control, "usando 5000 simulaciones.")
      insight::print_color(texto, "green")
      insight::export_table(dfs, align= "right", title = "--", format = "markdown")
      #print.data.frame(dfs, row.names = FALSE)
    }
  } else{
    #### extract correction method
    control <- match.arg(control)

    #### extract which test (fisher or chi square)
    test = match.fun(test)

    #### test rows or columns
    if (!popsInRows) tabla <- t(tabla)
    popsNames <- rownames(tabla)

    #### come up with all possible comparisons
    prs <- utils::combn(1:nrow(tabla),2)

    #### preallocate
    tests <- ncol(prs)
    pvals <- numeric(tests)
    lbls <- character(tests)
    for (i in 1:tests) {
      pvals[i] <- test(tabla[prs[,i],])$p.value
      lbls[i] <- paste(popsNames[prs[,i]],collapse=" vs. ")
    }
    adj.pvals <- stats::p.adjust(pvals,method=control)

    dfs<- data.frame(Comparar=lbls,p=round(pvals,digits),p.ajust=round(adj.pvals,digits))
    dfs[,3]  <- format(dfs[,3], scientific = FALSE)
    dfs[,3]  <- as.numeric(substr(dfs[,3]  , start = 1, stop = 5))
    dfs[,3][dfs[,3]   < 0.001] <- "<0.001"
    dfs[,2]  <- format(dfs[,2], scientific = FALSE)
    dfs[,2]  <- as.numeric(substr(dfs[,2]  , start = 1, stop = 5))
    dfs[,2][dfs[,2]   < 0.001] <- "<0.001"
    dfs<- dplyr::as_tibble(dfs)

    if (length(dfs)) {
      cat("\n")
      texto<- paste("Los valores de p fueron ajustados con el metodo:", control)
      insight::print_color(texto, "green")
      insight::export_table(dfs, align= "right", title = "--", format = "markdown")
      #print.data.frame(dfs, row.names = FALSE)
    }
  }
}

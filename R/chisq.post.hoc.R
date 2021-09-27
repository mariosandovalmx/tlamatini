#' Contrastes post hoc de una matriz de datos con chi-cuadrada
#'
#' Muestra las comparaciones post-hoc por pares de una prueba Chi cuadrada. Calcula los contrastes si hay
#' más de dos grupos en una chisq. Las pruebas post-hoc para saber que pares de poblaciones difieren tras
#' una prueba de chi-cuadrado significativa pueden construirse realizando todas las pruebas de chi-cuadrado
#' para todos los pares de poblaciones y ajustando después los valores p resultantes. Los valores p
#' ajustados pueden calcularse con una amplia variedad de métodos: fdr, BH, BY, bonferroni, holm, hochberg y #' hommel. Esta función funciona básicamente como una función envolvente que envía los valores p "brutos" no #' ajustados de cada prueba de chi-cuadrado por pares a la función p.adjust en el programa R base. La
#' función p.adjust debe ser consultada para una mayor descripción de los métodos utilizados.
#' Función tomada de la paqueteria fifer. https://www.rdocumentation.org/packages/fifer/versions/1.1
#' @param tbl Una tabla o matriz usada para la función chisq.test
#' @param test Método usado para ajustar el valor de P.
#' @param  popsInRows Un argumento lógico que indica si las poblaciones forman las filas (por defecto; =TRUE
#' ) de la tabla o no (=FALSE).
#' @param control Indica el método de control a utilizar.
#' @param digits Número de digitos a mostrar.
#'
#' @return Contrastes post hoc de chi cuadrada.
#' @export
#'
#' @examples
#' table <- as.table(rbind(c(14, 43), c(23, 47)))
#' rownames(table) <- c("females", "males")
#' chisq.post.hoc(table,test = c("fisher.test"))
#' @encoding UTF-8
#'
chisq.post.hoc <- function(tbl,test=c("fisher.test"), popsInRows=TRUE,control=c("fdr","BH","BY","bonferroni","holm","hochberg","hommel"),digits=4) {
  #### extract correction method
  control <- match.arg(control)

  #### extract which test (fisher or chi square)
  test = match.fun(test)

  #### test rows or columns
  if (!popsInRows) tbl <- t(tbl)
  popsNames <- rownames(tbl)

  #### come up with all possible comparisons
  prs <- utils::combn(1:nrow(tbl),2)

  #### preallocate
  tests <- ncol(prs)
  pvals <- numeric(tests)
  lbls <- character(tests)
  for (i in 1:tests) {
    pvals[i] <- test(tbl[prs[,i],])$p.value
    lbls[i] <- paste(popsNames[prs[,i]],collapse=" vs. ")
  }
  adj.pvals <- stats::p.adjust(pvals,method=control)
  cat("Adjusted p-values used the",control,"method.\n\n")
  data.frame(comparison=lbls,raw.p=round(pvals,digits),adj.p=round(adj.pvals,digits))
}





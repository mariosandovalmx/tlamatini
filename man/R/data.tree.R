#' Graficar estructura anidada de los datos
#'
#' Exploracion visual de la estructura de los datos. Permite analizar la estructura de anidamiento de los datos y el tamano de muestra por cada nivel. Util para establecer el anidamiento en el componente aleatorio de los modelos mixtos LMM, GLMM y GAM. Funcion tomada de la paqueteria "Plotrix".
#' Cita del paquete: J L (2006). “Plotrix: a package in the red light district of R.” R-News, 6(4), 8-12.
#' @param x Dataframe en el que cada columna sucesiva representa subcategorías de la columna anterior.
#' @param left El borde izquierdo de los rectángulos apilados en unidades especificadas por el usuario.
#' @param top La parte superior de los rectángulos apilados en unidades especificadas por el usuario.
#' @param right El borde derecho de la pila actual de rectángulos en unidades especificadas por el usuario.
#' @param lastcenter El centro del rectángulo anterior del que surge el siguiente desglose de categorías. No hay casi ninguna razón para cambiarlo.
#' @param showval Si se muestran los valores que representan las categorías.
#' @param showcount Si se desea mostrar el recuento de las categorías.
#' @param stacklabels Si se desea mostrar los nombres del dataframe debajo de los rectángulos apilados.
#' @param firstcall indicador para la función - no lo modifique.
#' @param col Colores de relleno opcionales para los rectángulos.
#' @param border Color para el borde alrededor de los rectángulos.
#' @param toplab Etiquetas opcionales para mostrar en la parte superior de cada pila de rectangulos.
#' @param base.cex Tamano de los caracteres base para las etiquetas.
#' @param ... argumentos adicionales pasados a 'plot'.
#'
#' @return Grafico de tipo arbol que muestra la estructura de los datos.
#' @export
#'
#' @examples
#' data(sleep)
#' data.tree(sleep[c(2:3)])
#' @encoding UTF-8

data.tree <-function (x, left = 0, top, right = 1, lastcenter = NA,
    showval = TRUE, showcount = TRUE, stacklabels = TRUE, firstcall = TRUE,
    col = NULL, border = NA, toplab = NULL, base.cex = 1, ...)
{
    dimx <- dim(x)
    colname <- names(x)[1]
    if (firstcall) {
        x <- x[do.call(order, x), ]
        oldmar <- par("mar")
        par(mar = c(1, 2, 2, 1))
        top <- sum(!is.na(x[, 1]))
        if (top < dimx[1])
            cat(dimx[1] - top, "NA values dropped from first stack.\n")
        plot(0, xlim = c(0, dimx[2]), ylim = c(0, top), type = "n",
            axes = FALSE, xlab = "", ylab = "", ...)
    }
    xfreq <- table(x[, 1])
    lenxf <- length(xfreq)
    if (firstcall) {
        if (is.null(col)) {
            col <- list()
            for (index in 1:dimx[2]) col[[index]] <- grDevices::rainbow(length(table(x[,
                index])))
        }
        for (index in 1:dimx[2]) if (is.null(names(col[[index]])))
            names(col[[index]]) <- names(table(x[, index]))
    }
    if (lenxf) {
        if (is.list(col)) {
            barcol <- col[[1]]
            barcol <- barcol[names(col[[1]]) %in% names(xfreq)]
        }
        else barcol <- col[names(col) %in% names(xfreq)]
        labels <- names(xfreq)
        squeeze <- (right - left)/10
        for (bar in 1:lenxf) {
            if (length(xfreq[bar])) {
                if (!is.na(xfreq[bar])) {
                  if (xfreq[bar] > 0) {
                    graphics::rect(left + squeeze, top - xfreq[bar], right -
                      squeeze, top, col = barcol[bar], border = border)
                    labelheight <- graphics::strheight(labels[bar])
                    cex <- ifelse((1.5 * labelheight) > xfreq[bar],
                      base.cex * 0.75 * xfreq[bar]/labelheight,
                      base.cex)
                    if (showval) {
                      textcol <- ifelse(colSums(grDevices::col2rgb(unlist(barcol[bar])) *
                        c(1.4, 1.4, 0.5)) < 350, "white", "black")
                      bartext <- ifelse(showcount, paste(labels[bar],
                        " (", xfreq[bar], ")", sep = ""), labels[bar])
                      text((left + right)/2, top - xfreq[bar]/2,
                        bartext, cex = cex, col = textcol)
                    }
                    if (!is.na(lastcenter))
                      graphics::segments(left + squeeze, top - xfreq[bar]/2,
                        left - squeeze, lastcenter)
                    xvalue <- ifelse(is.numeric(x[, 1]), as.numeric(labels[bar]),
                      labels[bar])
                    if (dimx[2] > 1) {
                      newcol <- col
                      newcol[[1]] <- NULL
                      nextx <- subset(x, x[, 1] == xvalue, 2:dimx[2])
                      data.tree(nextx, right, top, right + 1,
                        lastcenter = top - xfreq[bar]/2, showval = showval,
                        showcount = showcount, stacklabels = stacklabels, firstcall = FALSE,
                        col = newcol, border = border, base.cex = base.cex)
                    }
                  }
                }
            }
            top <- top - xfreq[bar]
        }
        if (stacklabels)
          graphics::mtext(colname, side = 1, at = (left + right)/2, line = -0.4)
    }
    if (firstcall) {
        if (!is.null(toplab)) {
            par(xpd = TRUE)
            top <- sum(!is.na(x[, 1]))
            text(0.5:(dimx[2] + 0.5), 1.01 * top, toplab, adj = c(0.5,
                0))
            par(xpd = FALSE)
        }
        par(mar = oldmar)
    }
}

#' Grafico que muestra los datos faltantes en un dataframe
#'
#' Explorar visualmente donde están los datos faltantes en las diferentes columnas del dataframe. Esta función fue tomada de la paqueteria Amelia, https://github.com/IQSS/Amelia.
#' @param obj Dataframe
#' @param legend Titulo predefinido
#' @param col Colores usados predefinidos
#' @param main main predefinido
#' @param y.cex tamaño de leyendas de eje y predefinido
#' @param x.cex tamaño de leyendas de eje x predefinido
#' @param y.labels leyendas eje y predefinido
#' @param y.at y.at predefinido
#' @param csvar csvar predefinido
#' @param tsvar tsvar predefinido
#' @param rank.order rank order predefinido
#' @param margins margenes predefinido
#' @param ... mas argumentos predefinidos
#'
#' @return Gráfico de distribución de datos faltantes.
#' @export
#'
#' @examples
#' data("iris")
#' iris$Sepal.Length[c(1,3,5,7,9)] <- NA
#' iris$Petal.Length[c(7,24,21,5)] <- NA
#' NA.map(iris)
#' @encoding UTF-8
#' @importFrom graphics image
#' @importFrom graphics axis
#' @importFrom stats reshape
#' @importFrom grDevices heat.colors

NA.map <- function(obj, legend = TRUE, col = c("indianred", "dodgerblue"), main,
                   y.cex = 0.8, x.cex = 0.8, y.labels, y.at, csvar = NULL,
                   tsvar = NULL, rank.order = TRUE, margins = c(5, 5), ...) {

  if (class(obj) == "amelia") {
    vnames <- colnames(obj$imputations[[1]])
    n <- nrow(obj$missMatrix)
    p <- ncol(obj$missMatrix)
    percent.missing <- colMeans(obj$missMatrix)
    pmiss.all <- mean(obj$missMatrix)
    r1 <- obj$missMatrix
  } else {
    vnames <- colnames(obj)
    n <- nrow(obj)
    p <- ncol(obj)
    percent.missing <- colMeans(is.na(obj))
    pmiss.all <- mean(is.na(obj))
    r1 <- 1*is.na(obj)
  }


  if (!missing(y.labels) &&
      (missing(y.at) && (length(y.labels) != n))) {
    stop("y.at must accompany y.labels if there is less than onefor each row")
  }

  if (is.null(csvar)) csvar <- obj$arguments$cs
  if (is.null(tsvar)) tsvar <- obj$arguments$ts

  if (missing(y.labels)) {
    if (!is.null(csvar)) {
      if (class(obj) == "amelia") {
        cs <- obj$imputations[[1]][,csvar]
      } else {
        cs <- obj[,csvar]
      }
      y.labels <- cs
      if (is.factor(y.labels)) y.labels <- levels(y.labels)[unclass(y.labels)]

      cs.names <- y.labels


      if (!is.numeric(cs)) cs <- as.numeric(as.factor(cs))
      if (!is.null(tsvar)) {
        if (class(obj) == "amelia") {
          ts <- as.numeric(obj$imputations[[1]][,tsvar])
        } else {
          ts <- as.numeric(obj[,tsvar])
        }
        unit.period <- order(cs, ts)
      } else {
        unit.period <- 1:n
      }

      y.labels <- y.labels[unit.period]
      r1 <- r1[unit.period,]

      brks <- c(TRUE,rep(FALSE, times = (n-1)))
      for (i in 2:n) {
        brks[i] <- (cs[unit.period][i]!=cs[unit.period][i-1])
      }
      y.at <- which(brks)

      y.labels <- y.labels[brks]
    } else {
      y.labels <- row.names(obj$imputations[[1]])
      y.at <- seq(1, n, by=15)
      y.labels <- y.labels[y.at]
    }
  } else {
    if (missing(y.at))
      y.at <- n:1
  }
  missrank <- rev(order(percent.missing))
  if (rank.order) {
    chess <- t(!r1[n:1, missrank])
    vnames <- vnames[missrank]
  } else {
    chess <- t(!r1[n:1,])
  }
  y.at <- (n:1)[y.at]

  if (missing(main))
    main <- "Missingness Map"

  par(mar = c(margins, 2, 1) + 0.1)
  ## here we fork for data/tscs type plots. users cant set this yet.
  type <- "data"
  if (legend) {
    graphics::layout(matrix(c(1,2), nrow = 1), widths = c(0.75, 0.25))
    par(mar = c(margins, 2, 0) + 0.1, mgp = c(3, 0.25, 0))
  }
  if (type == "data") {

    col.fix <- col
    if (sum(!chess) == 0) {
      col.fix <- col[2]
    }
    image(x = 1:(p), y = 1:n, z = chess, axes = FALSE,
          col = col.fix, xlab="", ylab="", main = main)

    axis(1, lwd = 0, labels = vnames, las = 2, at = 1:p, cex.axis = x.cex)
    axis(2, lwd = 0, labels = y.labels, las =1, at = y.at, cex.axis = y.cex)


    if (legend) {
      pm.lab <- paste("Missing (", round(100 * pmiss.all), "%)", sep = "")
      po.lab <- paste("Observed (", 100-round(100 * pmiss.all), "%)", sep = "")
      par(mar = c(0, 0, 0, 0.3))
      plot(0,0, type = "n", axes=  FALSE, ann=FALSE)
      legend("left", col = col, bty = "n", xjust = 0, border = NA,
             legend = c(pm.lab, po.lab), fill = col, horiz = FALSE)

    }
  } else {
    tscsdata <- data.frame(cs.names, ts, rowMeans(r1))
    tscsdata <- reshape(tscsdata, idvar = "cs.names", timevar = "ts",
                        direction = "wide")
    rownames(tscsdata) <- tscsdata[,1]
    colnames(tscsdata) <- unique(ts)
    tscsdata <- as.matrix(tscsdata[,-1])

    cols <- rev(heat.colors(5))

    image( z=t(tscsdata), axes
           = FALSE, col = cols, main = main, ylab="", xlab="")
    axis(1, labels = unique(ts), at = seq(from = 0, to = 1, length =
                                            ncol(tscsdata)), tck = 0, lwd = 0, las
         = 2)
    axis(2, labels = rownames(tscsdata), at = seq(from = 0, to = 1, length =
                                                    nrow(tscsdata)), tck = 0, lwd =
           0, las = 1, cex.axis = .8)

    if (legend) {
      ## par(xpd = TRUE)
      legend(x = 0.95, y = 1.01, col = cols, bty = "n",
             xjust = 1, legend = c("0-0.2",
                                   "0.2-0.4","0.4-0.6","0.6-0.8","0.8-1"), fill =cols, horiz = TRUE)
    }
  }

  invisible(NULL)

}


#NA.map(dataframe)

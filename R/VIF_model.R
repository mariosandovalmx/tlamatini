#' @title Revisar la multicolinealidad de las variables del modelo
#' @name VIF_model
#'
#' @description
#'
#'  `VIF_model()` comprueba la multicolinealidad en los modelos de regresión
#  calculando el factor de inflación de la varianza (VIF). Confidence
#'  intervals for VIF and tolerance are based on Marcoulides et al.
#'  (2019, Appendix B).
#'  `check_concurvity()` es una envoltura alrededor de `mgcv::concurvity()`, y puede ser
#' considerado como una comprobación de colinealidad para los términos 'suavizados' en los
#' GAM. Confidence intervals for VIF and tolerance are based on Marcoulides et al.
#' (2019, Appendix B).
#'
#' @param x Un modelo. Actualmente acepta lm, glm, lmm y glmmm (hasta donde yo sé).
#' @param ... Actualmente no se utiliza.
#'
#' @return Un dataframe con información sobre el nombre del término del modelo, el
#' VIF y los intervalos de confianza asociados, el factor
#' por el que se incrementa el error estándar debido a la posible correlación
#' con otros términos, y los valores de tolerancia (incluyendo los intervalos de confianza),
#' donde `tolerance = 1/vif`.
#'
#'
#' @references
#'   \itemize{
#'   \item Marcoulides, K. M., and Raykov, T. (2019). Evaluation of Variance
#'   Inflation Factors in Regression Models Using Latent Variable Modeling
#'   Methods. Educational and Psychological Measurement, 79(5), 874–882.
#'   }
#'
#' @note El código para calcular los intervalos de confianza para el VIF y la tolerancia
#' fue adaptado del Apéndice B del artículo de Marcoulides et al.
#' Por lo tanto, los créditos van a estos autores el algoritmo original. También
#' a [`plot()`-method](https://easystats.github.io/see/articles/performance.html)
#' implementado en el \href{https://easystats.github.io/see/}{\pkg{see}-paquete}.
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' VIF_model(m)
#'
#' # plot results
#' if (require("see")) {
#'   x <- VIF_model(m)
#'   plot(x)
#' }
#' @export
VIF_model <- function(x, ...) {
  UseMethod("VIF_model")
}


#' @rdname VIF_model
#' @export
multicollinearidad <- VIF_model



# default ------------------------------

#' @export
VIF_model.default <- function(x, ci = 0.95, verbose = TRUE, ...) {
  .VIF_model(x, component = "conditional", ci = ci, verbose = verbose)
}



# methods -------------------------------------------

#' @export
print.VIF_model <- function(x, ...) {
  insight::print_color("# Comprobaci\u00f3n de la multicolinealidad\n", "blue")

  if ("Component" %in% colnames(x)) {
    comp <- split(x, x$Component)
    for (i in 1:length(comp)) {
      cat(paste0("\n* ", comp[[i]]$Component[1], " component:\n"))
      .print_collinearity(data_remove(comp[[i]], "Component"))
    }
  } else {
    .print_collinearity(x)
  }

  invisible(x)
}


#' @export
plot.VIF_model <- function(x, ...) {
  insight::check_if_installed("see", "to plot collinearity-check")
  NextMethod()
}


.print_collinearity <- function(x) {
  vifs <- x$VIF
  low_vif <- which(vifs < 5)
  mid_vif <- which(vifs >= 5 & vifs < 10)
  high_vif <- which(vifs >= 10)

  all_vifs <- insight::compact_list(list(low_vif, mid_vif, high_vif))

  # format table for each "ViF" group - this ensures that CIs are properly formatted
  x <- do.call(rbind, lapply(all_vifs, function(i) insight::format_table(x[i, ])))
  colnames(x)[4] <- "Increased SE"

  if (length(low_vif)) {
    cat("\n")
    insight::print_color("Correlaci\u00f3n baja\n\n", "green")
    print.data.frame(x[low_vif, ], row.names = FALSE)
  }

  if (length(mid_vif)) {
    cat("\n")
    insight::print_color("Correlaci\u00f3n moderada\n\n", "yellow")
    print.data.frame(x[mid_vif, ], row.names = FALSE)
  }

  if (length(high_vif)) {
    cat("\n")
    insight::print_color("Correlaci\u00f3n alta\n\n", "red")
    print.data.frame(x[high_vif, ], row.names = FALSE)
  }
}



# other classes ----------------------------------

#' @export
VIF_model.afex_aov <- function(x, verbose = TRUE, ...) {
  if (length(attr(x, "within")) == 0L) {
    return(VIF_model(x$lm, verbose = verbose, ...))
  }

  f <- insight::find_formula(x)[[1]]
  f <- Reduce(paste, deparse(f))
  f <- sub("\\+\\s*Error\\(.*\\)$", "", f)
  f <- stats::as.formula(f)

  d <- insight::get_data(x, verbose = verbose)
  is_num <- sapply(d, is.numeric)
  d[is_num] <- sapply(d[is_num], scale, center = TRUE, scale = FALSE)
  is_fac <- !is_num
  contrs <- lapply(is_fac, function(...) stats::contr.sum)[is_fac]

  if (verbose) {
    message(insight::format_message("Todos los predictores han sido centrados (factores con \'contr.sum()\', num\u00e9ricos con \'scale()\')."))
  }

  VIF_model(suppressWarnings(stats::lm(
    formula = f,
    data = d,
    contrasts = contrs
  )))
}

#' @export
VIF_model.BFBayesFactor <- function(x, verbose = TRUE, ...) {
  if (!insight::is_model(x)) {
    stop("La colinealidad s\u00f3lo es aplicable a los modelos de regresi\u00f3n.")
  }

  f <- insight::find_formula(x)[[1]]
  d <- insight::get_data(x)
  VIF_model(stats::lm(f, d))
}

# mfx models -------------------------------

#' @export
VIF_model.logitor <- function(x, ci = 0.95, verbose = TRUE, ...) {
  .VIF_model(x$fit, component = "conditional", ci = ci, verbose = verbose)
}

#' @export
VIF_model.logitmfx <- VIF_model.logitor

#' @export
VIF_model.probitmfx <- VIF_model.logitor

#' @export
VIF_model.poissonirr <- VIF_model.logitor

#' @export
VIF_model.poissonmfx <- VIF_model.logitor

#' @export
VIF_model.negbinirr <- VIF_model.logitor

#' @export
VIF_model.negbinmfx <- VIF_model.logitor

#' @export
VIF_model.betaor <- VIF_model.logitor

#' @export
VIF_model.betamfx <- VIF_model.logitor



# zi-models -------------------------------------
#' @export
VIF_model.glmmTMB <- function(x,
                                       component = c("all", "conditional", "count", "zi", "zero_inflated"),
                                       ci = 0.95,
                                       verbose = TRUE,
                                       ...) {
  component <- match.arg(component)
  .VIF_model_zi_model(x, component, ci = ci, verbose = verbose)
}


#' @export
VIF_model.MixMod <- function(x,
                                      component = c("all", "conditional", "count", "zi", "zero_inflated"),
                                      ci = 0.95,
                                      verbose = TRUE,
                                      ...) {
  component <- match.arg(component)
  .VIF_model_zi_model(x, component, ci = ci, verbose = verbose)
}


#' @export
VIF_model.hurdle <- function(x,
                                      component = c("all", "conditional", "count", "zi", "zero_inflated"),
                                      ci = 0.95,
                                      verbose = verbose,
                                      ...) {
  component <- match.arg(component)
  .VIF_model_zi_model(x, component, ci = ci, verbose = verbose)
}


#' @export
VIF_model.zeroinfl <- function(x,
                                        component = c("all", "conditional", "count", "zi", "zero_inflated"),
                                        ci = 0.95,
                                        verbose = verbose,
                                        ...) {
  component <- match.arg(component)
  .VIF_model_zi_model(x, component, ci = ci, verbose = verbose)
}


#' @export
VIF_model.zerocount <- function(x,
                                         component = c("all", "conditional", "count", "zi", "zero_inflated"),
                                         ci = 0.95,
                                         verbose = verbose,
                                         ...) {
  component <- match.arg(component)
  .VIF_model_zi_model(x, component, ci = ci, verbose = verbose)
}



# utilities ---------------------------------

.VIF_model_zi_model <- function(x, component, ci = 0.95, verbose = TRUE) {
  if (component == "count") component <- "conditional"
  if (component == "zi") component <- "zero_inflated"

  mi <- insight::model_info(x, verbose = FALSE)
  if (!mi$is_zero_inflated) component <- "conditional"

  if (component == "all") {
    cond <- .VIF_model(x, "conditional", ci = ci, verbose = verbose)
    zi <- .VIF_model(x, "zero_inflated", ci = ci, verbose = FALSE)
    if (is.null(cond) && is.null(zi)) {
      return(NULL)
    }
    if (is.null(cond)) {
      zi$Component <- "zero inflated"
      return(zi)
    }
    if (is.null(zi)) {
      cond$Component <- "conditional"
      return(cond)
    }

    # retrieve data for plotting
    dat_cond <- attr(cond, "data")
    dat_zi <- attr(zi, "data")
    ci_cond <- attr(cond, "CI")
    ci_zi <- attr(zi, "CI")

    # add component
    cond$Component <- "conditional"
    zi$Component <- "zero inflated"
    dat_cond$Component <- "conditional"
    dat_zi$Component <- "zero inflated"
    ci_cond$Component <- "conditional"
    ci_zi$Component <- "zero inflated"

    # create final data
    dat <- rbind(cond, zi)
    attr(dat, "data") <- rbind(dat_cond, dat_zi)
    attr(dat, "CI") <- rbind(ci_cond, ci_zi)
    dat
  } else {
    .VIF_model(x, component, ci = ci, verbose = verbose)
  }
}



.VIF_model <- function(x, component, ci = 0.95, verbose = TRUE) {
  v <- insight::get_varcov(x, component = component, verbose = FALSE)
  assign <- .term_assignments(x, component, verbose = verbose)

  # any assignment found?
  if (is.null(assign) || all(is.na(assign))) {
    if (verbose) {
      warning(insight::format_message(sprintf("No se han podido extraer los t\u00e9rminos del modelo para el componente %s del modelo.", component), call. = FALSE))
    }
    return(NULL)
  }


  # we have rank-deficiency here. remove NA columns from assignment
  if (isTRUE(attributes(v)$rank_deficient) && !is.null(attributes(v)$na_columns_index)) {
    assign <- assign[-attributes(v)$na_columns_index]
    if (isTRUE(verbose)) {
      warning(insight::format_message("La matriz del modelo tiene un rango deficiente. Los VIF pueden no ser sensatos."), call. = FALSE)
    }
  }

  # check for missing intercept
  if (insight::has_intercept(x)) {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else {
    if (isTRUE(verbose)) {
      warning("El modelo no tiene intercepto. Los VIFs pueden no ser sensibles.", call. = FALSE)
    }
  }

  f <- insight::find_formula(x)

  if (inherits(x, "mixor")) {
    terms <- labels(x$terms)
  } else {
    terms <- labels(stats::terms(f[[component]]))
  }

  if ("instruments" %in% names(f)) {
    terms <- unique(c(terms, labels(stats::terms(f[["instruments"]]))))
  }

  n.terms <- length(terms)

  if (n.terms < 2) {
    if (isTRUE(verbose)) {
      warning(insight::format_message(sprintf("No hay suficientes t\u00e9rminos del modelo en la parte %s del modelo para comprobar la multicolinealidad.", component)), call. = FALSE)
    }
    return(NULL)
  }

  R <- stats::cov2cor(v)
  detR <- det(R)

  result <- vector("numeric")
  na_terms <- vector("numeric")

  for (term in 1:n.terms) {
    subs <- which(assign == term)
    if (length(subs)) {
      result <- c(
        result,
        det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
      )
    } else {
      na_terms <- c(na_terms, term)
    }
  }

  # any terms to remove, due to rank deficiency?
  if (length(na_terms)) {
    terms <- terms[-na_terms]
  }

  # check for interactions, VIF might be inflated...
  if (!is.null(insight::find_interactions(x)) && any(result > 10)) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("El modelo tiene t\u00e9rminos de interacci\u00f3n. Los VIF pueden estar inflados. Puede comprobar la multicolinealidad entre los predictores de un modelo sin t\u00e9rminos de interacci\u00f3n."), call. = FALSE)
    }
  }

  # CIs, see Appendix B 10.1177/0013164418817803
  r <- 1 - (1 / result)
  n <- insight::n_obs(x)
  p <- insight::n_parameters(x)

  ci_lvl <- (1 + ci) / 2

  logis_r <- stats::qlogis(r) # see Raykov & Marcoulides (2011, ch. 7) for details.
  se <- sqrt((1 - r^2)^2 * (n - p - 1)^2 / ((n^2 - 1) * (n + 3)))
  se_log <- se / (r * (1 - r))
  ci_log_lo <- logis_r - stats::qnorm(ci_lvl) * se_log
  ci_log_up <- logis_r + stats::qnorm(ci_lvl) * se_log
  ci_lo <- stats::plogis(ci_log_lo)
  ci_up <- stats::plogis(ci_log_up)

  out <- insight::text_remove_backticks(
    data.frame(
      Term = terms,
      VIF = result,
      VIF_CI_low = 1 / (1 - ci_lo),
      VIF_CI_high = 1 / (1 - ci_up),
      SE_factor = sqrt(result),
      Tolerance = 1 / result,
      Tolerance_CI_low = 1 - ci_up,
      Tolerance_CI_high = 1 - ci_lo,
      stringsAsFactors = FALSE
    ),
    column = "Variable"
  )
  attr(out, "ci") <- ci

  attr(out, "data") <- insight::text_remove_backticks(
    data.frame(
      Term = terms,
      VIF = result,
      SE_factor = sqrt(result),
      stringsAsFactors = FALSE
    ),
    column = "Variable"
  )

  attr(out, "CI") <- data.frame(
    VIF_CI_low = 1 / (1 - ci_lo),
    VIF_CI_high = 1 / (1 - ci_up),
    Tolerance_CI_low = 1 - ci_up,
    Tolerance_CI_high = 1 - ci_lo,
    stringsAsFactors = FALSE
  )

  class(out) <- c("VIF_model", "see_check_collinearity", "data.frame")
  out


}



.term_assignments <- function(x, component, verbose = TRUE) {
  tryCatch(
    {
      if (inherits(x, c("hurdle", "zeroinfl", "zerocount"))) {
        assign <- switch(component,
                         conditional = attr(insight::get_modelmatrix(x, model = "count"), "assign"),
                         zero_inflated = attr(insight::get_modelmatrix(x, model = "zero"), "assign")
        )
      } else if (inherits(x, "glmmTMB")) {
        assign <- switch(component,
                         conditional = attr(insight::get_modelmatrix(x), "assign"),
                         zero_inflated = .zi_term_assignment(x, component, verbose = verbose)
        )
      } else if (inherits(x, "MixMod")) {
        assign <- switch(component,
                         conditional = attr(insight::get_modelmatrix(x, type = "fixed"), "assign"),
                         zero_inflated = attr(insight::get_modelmatrix(x, type = "zi_fixed"), "assign")
        )
      } else {
        assign <- attr(insight::get_modelmatrix(x), "assign")
      }

      if (is.null(assign)) {
        assign <- .find_term_assignment(x, component, verbose = verbose)
      }

      assign
    },
    error = function(e) {
      .find_term_assignment(x, component, verbose = verbose)
    }
  )
}



.find_term_assignment <- function(x, component, verbose = TRUE) {
  pred <- insight::find_predictors(x)[[component]]

  if (is.null(pred)) {
    return(NULL)
  }

  dat <- insight::get_data(x, verbose = verbose)[, pred, drop = FALSE]

  parms <- unlist(lapply(1:length(pred), function(i) {
    p <- pred[i]
    if (is.factor(dat[[p]])) {
      ps <- paste0(p, levels(dat[[p]]))
      names(ps)[1:length(ps)] <- i
      ps
    } else {
      names(p) <- i
      p
    }
  }))

  if (insight::is_gam_model(x)) {
    model_params <- as.vector(unlist(unlist(insight::find_parameters(x)[c(component, "smooth_terms")])))
  } else {
    model_params <- insight::find_parameters(x)[[component]]
  }

  as.numeric(names(parms)[match(
    insight::clean_names(model_params),
    parms
  )])
}



.zi_term_assignment <- function(x, component = "zero_inflated", verbose = TRUE) {
  tryCatch(
    {
      rhs <- insight::find_formula(x)[[component]]
      d <- insight::get_data(x, verbose = verbose)
      attr(insight::get_modelmatrix(rhs, data = d), "assign")
    },
    error = function(e) {
      NULL
    }
  )
}

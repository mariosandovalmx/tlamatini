# TODO:
# 1. Incluir el parámetro alfa para empezar a marcar el significado.

# ====================================================================
#' Formato de valores de p
#'
#' Functions to fromat p values.
#'
#' @return Un vector de caracteres con valores p formateados.
#'
#' @param p Un (vector de) valores de p. Numerico.
#'          O un `data.frame`.
#' @param digits_p (numerico) Número de dígitos significativos para redondear un valor p.
#'                           No menos de 2. \itemize{
#'         \item if `digits_p = 2`: \enumerate{
#'               \item los valores inferiores a 0.001 se imprimen como `"<0.001"`;
#'               \item valores entre 0.001 y 0.01 como `"<0.01"`;
#'               \item todos los demás valores se redondean a dos decimales.
#'               }
#'         \item if `digits_p = 3`, sólo formato `"<0.01"` se salta.
#'         }
#' @param signif_stars (logical) Marca si se deben añadir estrellas de significación a cada valor p.
#'                               No menos de 2.
#' @param rm_zero (logical) Marca si el cero inicial antes del punto debe ser removido.
#' @param add_p (logical) Marca si la letra "p" debe incluirse en la expresión.
#' @param rm_spaces (logical) Marca si se deben eliminar todos los espacios.
#' @param ... Argumentos para otros métodos.
#' @param cols,alpha,ss,decreasing,collapse,dec `# FIXME:` _not documented_
#'
#' @details
#'
#' \itemize{
#'  \item `format_p` - formatos único valor p.
#'  \item `format_as_p_columns` - formatea las columnas numericas indicadas en un marco de datos como valores p (las columnas se convierten en cadenas).
#'  \item `get_signif_stars` -  toma los valores numéricos de p aporta las estrellas de significación estadística apropiadas.
#'  \item `add_signif_stars` - formatea los valores p numéricos añadiendo estrellas de significación (el resultado es un vector de caracteres).
#'  \item `signif_stars_legend` - genera una leyenda para las estrellas de significación (el resultado es una cadena).
#'  \item `rm_zero` - elimina el cero al principio de un número (devuelve una cadena con el mismo valor pero sin el cero inicial).
#' }
#'
#' @note Función tomada y traducida de la paqueteria Biostat: https://github.com/GegznaV/biostat.
#' @export
#' @keywords internal
#' @importFrom magrittr %<>%
#' @examples
#' # Prettify p-values
#'
#' #format_p_values(0.0005)
#'
#' #format_p_values(0.005)
#' #format_p_values(0.005, signif_stars = FALSE)
#' #format_p_values(0.005, rm_zero = TRUE)
#' #format_p_values(0.005, digits_p = 2)
#' #format_p_values(0.005, digits_p = 2, rm_zero = TRUE, signif_stars = FALSE)
#'
#' #format_p_values("0.00022")
#' #format_p_values("0.052")
#'
#' #format_p_values(c(0.005, 0.0005, 0.052147))
#'
#'
#' #get_signif_stars(0.005)
#' #add_signif_stars(0.005)
#'
#' #get_signif_stars(0.0005)
#' #add_signif_stars(0.0005)
#'
#' #get_signif_stars(0.052147)
#' #add_signif_stars(0.052147)
#'
#' #signif_stars_legend()
format_p_values <- function(p,
                            digits_p = 3,
                            cols = NULL,
                            ...,
                            alpha = 0.05,
                            signif_stars = TRUE,
                            rm_zero = FALSE,
                            add_p = FALSE,
                            rm_spaces = FALSE,
                            ss = signif_syms) {
  UseMethod("format_p_values")
}

#' @rdname format_p_values
#' @export
format_p_values.default <- function(p,
                                    digits_p = 3,
                                    cols = NULL,
                                    ...,
                                    signif_stars = TRUE,
                                    rm_zero = FALSE,
                                    add_p = FALSE,
                                    rm_spaces = FALSE,
                                    ss = signif_syms) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p %<>% as.character() %>% as.numeric()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!checkmate::test_numeric(p, lower = 0, upper = 1)) {
    stop(
      "`p` debe contener valores num\u00e9ricos en el rango de 0 a 1.\n",
      "No se aceptan los valores NA, NULL, -Inf, e Inf."
    )
  }
  if (!checkmate::test_number(digits_p, lower = 2, na.ok = TRUE)) {
    stop(
      "`digits` debe ser un \u00fanico valor num\u00e9rico en el rango de 2 a infinito.\n",
      "No se aceptan los valores NA, NULL, -Inf, e Inf."
    )
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sapply(p, format_p,
    digits_p = digits_p,
    signif_stars = signif_stars,
    rm_zero = rm_zero,
    add_p = add_p,
    rm_spaces = rm_spaces,
    ss = ss,
    ...
  )
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
#' @examples
#'
#' #format_p(0)
#' #format_p(.02, digits_p = 2)
#' #format_p(.0002)
#' #format_p(.0002, signif_stars = FALSE)
#' #format_p(.0002, ss = c("*****" = 0.001))
#'
#' # TODO:
#' # 1. Añadir parámetro para activar la corrección del valor p
#' #    de p = 1 a, por ejemplo, p > 0,999;
#' #
#' # 2. fusionar parámetros `ss` y `signif_stars`
#' # 3. test: format_p(NaN) -- [OK]
#' # 4. test: format_p(NA) -- esta función falla con NA como entrada.
format_p <-
  function(p_i,
           digits_p = 3,
           signif_stars = TRUE,
           rm_zero = FALSE,
           add_p = FALSE,
           rm_spaces = FALSE,
           ss = signif_syms) {

    # if (is.na(p_i)) {
    #     return(as.character(p_i))
    # }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    s_i <- if (signif_stars == TRUE) {
      tlamatini::get_signif_stars(p_i, ss = ss)
    } else {
      ""
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is.na(digits_p) || is.na(p_i)) {
      p_i <- as.character(p_i)
    } else {
      min_limit <- 10^-(digits_p)

      p_i <- if (digits_p > 3 & p_i < min_limit) {
        paste0("<", formatC(min_limit, digits = digits_p, format = "f"))
      } else if (digits_p <= 3 & p_i < 0.001) {
        "<0.001"
      } else if (digits_p <= 2 & p_i < 0.01) {
        "<0.01"
      } else {
        paste0(" ", formatC(p_i, digits = digits_p, format = "f"))
      }
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      p_i <- if (signif_stars == TRUE) {
        sprintf(glue::glue("%{digits_p + 3}s %-3s"), p_i, s_i)
      } else {
        sprintf(glue::glue("%{digits_p + 3}s"), p_i)
      }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (rm_zero == TRUE) {
      p_i <- tlamatini::rm_zero(p_i)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (add_p == TRUE) {
      if (grepl("<", p_i)) {
        p_i <- paste0("p ", sub("<", "< ", p_i))
      } else {
        p_i <- paste0("p =", p_i)
      }
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (rm_spaces == TRUE) {
      p_i <- gsub(" ", "", p_i)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Output:
    p_i
  }
# ============================================================================
#' @rdname format_p_values
#' @export
format_p_values.data.frame <- function(p,
                                       digits_p = 3,
                                       # colnames = NULL,
                                       cols = NULL,
                                       ...,
                                       ss = signif_syms,
                                       signif_stars = TRUE,
                                       rm_zero = FALSE,
                                       add_p = FALSE,
                                       rm_spaces = FALSE) {
  data <- p
  data_colnames <- names(data)

  if (is.null(cols)) {
    is_p <- grepl("^p$|^p.?val|^p.?adj", data_colnames, ignore.case = TRUE)

    message(
      "El formato del valor p aplicado a estas columnas: ",
      paste(data_colnames[is_p], collapse = ", ")
    )
  } else if (is.character(cols)) {
    is_p <- data_colnames %in% cols
  } else if (is.numeric(cols)) {
    is_p <- cols
  } else if (is.logical(cols)) {
    is_p <- cols
  } else {
    stop("El tipo de argumento `cols` es incorrecto.")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  is_num <- sapply(data, is.numeric)
  not_numeric_p <- is_p & (!is_num)
  if (any(not_numeric_p)) {
    message(
      "Estas columnas no son num\u00e9ricas, por lo que se omite el formato p: ",
      paste(data_colnames[not_numeric_p], collapse = ", ")
    )
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  colname <- data_colnames[is_p & is_num]

  for (colname_i in colname) {
    data[[colname_i]] %<>%
      purrr::map_chr(format_p_values,
        digits_p = digits_p,
        signif_stars = signif_stars,
        ss = ss,
        rm_zero = rm_zero,
        add_p = add_p,
        rm_spaces = rm_spaces,
        ...
      )
  }
  data
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
add_signif_stars <- function(p, ss = signif_syms) {
  paste(p, format(get_signif_stars(p, ss = ss)))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
get_signif_stars <- function(p, ss = signif_syms) {
  checkmate::assert_numeric(p, lower = 0, upper = 1)
  checkmate::assert_numeric(ss, lower = 0, upper = 1)

  ss_obj <- signif_parse(ss)

  # sapply(p, function(p_i) {
  #     stats::symnum(
  #         p_i,
  #         corr = FALSE,
  #         na = FALSE,
  #         cutpoints = ss_obj$cutpoints,
  #         symbols   = ss_obj$symbols
  #     )
  # })

  res <- stats::symnum(
    p,
    corr = FALSE,
    na = FALSE,
    cutpoints = ss_obj$cutpoints,
    symbols   = ss_obj$symbols
  )

  unclass(res)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
signif_syms_05s <- c("*" = 0.05)
#' @rdname format_p_values
#' @export
p05 <- c("*" = 0.05)
#' @rdname format_p_values
#' @export
signif_syms_01s <- c("*" = 0.01)
#' @rdname format_p_values
#' @export
p01 <- c("*" = 0.01)
#' @rdname format_p_values
#' @export
signif_syms_001s <- c("*" = 0.001)
#' @rdname format_p_values
#' @export
p001 <- c("*" = 0.001)
#' @rdname format_p_values
#' @export
signif_syms_001 <- c("***" = 0.001)
#' @rdname format_p_values
#' @export
signif_syms_01 <- c("***" = 0.001, "**" = 0.01)
#' @rdname format_p_values
#' @export
signif_syms_05 <- c("***" = 0.001, "**" = 0.01, "*" = 0.05)
#' @rdname format_p_values
#' @export
signif_syms <- c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1)
#' @rdname format_p_values
#' @export
p05_01_001 <- c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1)
#' @rdname format_p_values
#' @export
p05plus <- c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.1)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
signif_parse <- function(ss = NULL) {
  # ss is a named numeric vector, e.g.,
  # ss <- c("***" = 0.001, "**" = 0.01, "*" = 0.05)
  ss <- ss[order(ss)]

  list(
    symbols = c(names(ss), " "),
    cutpoints = c(0, as.numeric(ss), 1)
  )
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
signif_stars_legend_2 <- function(ss = signif_syms) {
  # ss is a named numeric vector, e.g.,
  # ss <- c("***" = 0.001, "**" = 0.01, "*" = 0.05)

  ss_obj <- signif_parse(ss)

  tmp <- stats::symnum(1,
    corr = FALSE, na = FALSE,
    cutpoints = ss_obj$cutpoints,
    symbols = ss_obj$symbols
  )

  attr(tmp, "legend")
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
# **[!!!]** en dash (–) might cause error on CRAN checks.
signif_stars_legend <- function(ss = signif_syms,
                                decreasing = FALSE,
                                collapse = c("  \n", ", ", "; ")) {
  collapse <- match.arg(collapse)
  ss <- ss[order(ss, decreasing = decreasing)]
  xx <- c(
    names(ss),
    paste0("- p < ", as.numeric(ss))
  ) %>%
    matrix(ncol = 2)

  paste(paste(xx[, 1], xx[, 2]), collapse = collapse)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname format_p_values
#' @export
#' @examples
#' rm_zero(0.020)
rm_zero <- function(str, dec = ".") {
  sub(paste0("0", dec), dec, as.character(str), fixed = TRUE)
}


#' @rdname format_p_values
#' @export
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
format_as_p_columns <- function(data,
                                colnames = c("p.value", "p.adjust"),
                                digits_p = 3,
                                rm_zero = FALSE,
                                signif_stars = FALSE,
                                ...) {
  .Deprecated("format_p_values")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data_colnames <- names(data)
  colname <- data_colnames[data_colnames %in% colnames]

  for (colname_i in colname) {
    data[[colname_i]] %<>%
      purrr::map_chr(format_p_values,
        digits_p = digits_p,
        rm_zero = rm_zero,
        signif_stars = signif_stars,
        ...
      )
  }
  data
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

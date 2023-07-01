#' @title Limpia los nombres de un objeto (normalmente un data.frame).
#'
#' @description
#' Los nombres resultantes son unicos y estan compuestos unicamente por el caracter \code{_}, numeros y
#' letras. Las preferencias de mayusculas se pueden especificar utilizando el parametro \code{case}.
#'
#'
#' Los caracteres acentuados se transliteran a ASCII.  Por ejemplo, una "o" con una
#' dieresis alemana sobre ella se convierte en "o", y el caracter español "ñ" se convierte en
#' "n".
#'
#' Esta funcion toma y devuelve un data.frame. Se puede usar con pipelines
#' \code{`\%>\%`}. \code{limpiar_nombres} se basa en la versatil funcion
#'  \code{link[snakecase]{to_any_case}}, que acepta muchos argumentos.  Consulte la documentacion de
#'  esa funcion para obtener ideas sobre como sacar
#'  sacar el maximo provecho de \code{limpiar_nombres}.  A continuacion se incluyen algunos ejemplos.
#'
#'
#' Esta funcion fue tomada del paquete janitor: \code{link[janitor]{clean_names}}.
#'
#'
#' @param dat data.frame.
#' @inheritDotParams limpiar_nombres2 -string
#' @return Devuelve el data.frame con nombres limpios.
#'
#' @details \code{limpiar_nombres()} esta destinado a ser utilizado en \code{data.frames}
#'   y objetos similares a un \code{data.frame}. Para limpiar otros objetos con nombre como listas con
#'   nombre y vectores, use \code{limpiar_nombres2()}.
#'
#'
#' @export
#' @family Set names
#' @examples
#'
#' # --- uso ---
#' x <- data.frame(caseID = 1, DOB = 2, Other = 3)
#' limpiar_nombres(x)
#'
#' # o usando pipelines:
#' # x %>%
#' #  limpiar_nombres()
#'
#' # si prefiere los nombres de las variables en camelCase:
#' #x %>%
#'  # limpiar_nombres(., "lower_camel")
#'
#' # (not run) correr limpiar_nombres despues de cargar una base:
#' # library(readxl)
#' # read_excel("messy_excel_file.xlsx") %>%
#' #   limpiar_nombres()
#'
#' # --- Aprovechando las ventajas de snakecase::to_any_case arguments ---
#'
#' # Restaurar nombres de columnas para graficar
#' #mtcars %>%
#' #  limpiar_nombres(case = "title")
#'
#' # indicar a limpiar_nombres dejar intactas ciertas abreviaturas:
#' #x %>%
#' #  limpiar_nombres(case = "upper_camel", abbreviations = c("ID", "DOB"))
#' @encoding UTF-8
limpiar_nombres <- function(dat, ...) {
  UseMethod("limpiar_nombres")
}


#' @export
limpiar_nombres.default <- function(dat, ...) {
  if(is.null(names(dat)) && is.null(dimnames(dat))) {
    stop(
      "requiere que tanto names como dimnames no sean nulos.",
      call. = FALSE
    )
  }
  if(is.null(names(dat))) {
    dimnames(dat) <- lapply(dimnames(dat), limpiar_nombres2, ...)
  } else {
    names(dat) <- limpiar_nombres2(names(dat), ...)
  }
  dat
}

#' @export
limpiar_nombres.sf <- function(dat, ...) {
  if (!requireNamespace("sf", quietly = TRUE)) { # nocov start
    stop(
      "Paquete \'sf\' es necesario para que la funcion pueda ser usada. Por favor, inst\u00e1lelo.",
      call. = FALSE
    )
  } # nocov end
  # get old names
  sf_names <- names(dat)
  # identify ending column index to clean
  n_cols <- length(dat)-1
  # clean all but last column
  sf_cleaned <- limpiar_nombres2(sf_names[1:n_cols], ...)
  # rename original df
  names(dat)[1:n_cols] <- sf_cleaned

  return(dat)
}

#' @export
#' @importFrom dplyr rename_all
limpiar_nombres.tbl_graph <- function(dat, ...) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) { # nocov start
    stop(
      "Se necesita el paquete \'tidygraph\' para que esta funcion funcione. Por favor, inst\u00e1lelo.",
      call. = FALSE
    )
  } # nocov end
  dplyr::rename_all(dat, .funs=limpiar_nombres2, ...)
}

# TODO: Segun https://www.compart.com/en/unicode/U+03BC revisado el
# 2021-07-10, hay algunos caracteres de codificacion UTF-32 que tambien son mu o
# micro.  Esto solo maneja los valores utf-8; para agregar mas caracteres, solo agregue
# a este vector de caracteres.

#' Constante para ayudar a cambiar de mu a u
#'
#' Se trata de un vector de caracteres con los nombres de todos los puntos de codigo Unicode
#' conocidos que se parecen al simbolo griego mu o al micro y a los valores de "u".  Esto es
#' destinado a simplificar el mapeo de mu o micro en Unicode al caracter "u" con \code{limpiar_nombres()} y \code{limpiar_nombres2()}.
#'
#' Mirar la ayuda de \code{limpiar_nombres()} para saber como usarla.
#'
#' @family Set names
#' @keywords internal
mu_to_u <-
  # setNames is used instead of setting the names directly because it prevents a
  # warning like "unable to translate \'<U+3382>\' to native encoding" for several
  # of the items.
  setNames(
    rep("u", 10),
    nm=
      c(
        "\u00b5", "\u03bc", "\u3382", "\u338c", "\u338d",
        "\u3395", "\u339b", "\u33b2", "\u33b6", "\u33bc"
      )
  )

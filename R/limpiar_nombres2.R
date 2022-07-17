#' @title Limpia un vector de texto, que suele contener los nombres de un objeto.
#'
#' @description Los vectores resultantes son unicos y estan formadas unicamente por el caracter
#' \code{_}, numeros y letras. Por defecto, solo consistiran en caracteres ASCII, pero se puede
#' permitir que no sean ASCII (por ejemplo, Unicode) configurando \code{ascii=FALSE}.
#' Las preferencias de mayusculas pueden especificarse utilizando el parametro \code{case}.
#'
#'
#' Cuando \code{ascii=TRUE} (el valor predeterminado), los caracteres acentuados se transliteran
#' a ASCII.  Por ejemplo, una "o" con dieresis alemana se convierte en "o", y
#' el caracter español "enye" se convierte en "n".
#' Esta funcion fue tomada del paquete janitor.
#'
#'
#'
#' @param string Un vector de caracteres de nombres para limpiar.
#' @param case Preferencias de mayusculas
#'
#'
#' @param replace Un vector de caracteres con nombre en el que el nombre se sustituye por el
#'   value.
#' @param ascii Convertir los nombres a ASCII (TRUE, por defecto) o no (FALSE).
#' @param use_make_names ¿Deberia aplicarse el codigo {make.names()} para asegurar que la
#' sea utilizable como un nombre sin comillas?  (Evitar \code{make.names()}
#' asegura que la salida es independiente de la localizacion, pero las comillas pueden ser necesarias).
#' @inheritParams snakecase::to_any_case
#' @inheritDotParams snakecase::to_any_case
#'
#' @return Devuelve el vector de caracteres "limpio".
#' @export
#' @seealso \code{\link[snakecase]{to_any_case}()}
#' @examples
#'
#' # limpiar los nombres de un vector:
#' x <- structure(1:3, names = c("nombre con espacio", "DosPalabras", "total $ (2009)"))
#' x
#' names(x) <- limpiar_nombres2(names(x))
#' x # Ya tiene los nombres limpios
#'
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_replace str_replace_all
#' @importFrom snakecase to_any_case
#' @encoding UTF-8
limpiar_nombres2  <- function(string,
                             case = "snake",
                             replace=
                               c(
                                 "\'"="",
                                 "\""="",
                                 "%"="_percent_",
                                 "#"="_number_"
                               ),
                             ascii=TRUE,
                             use_make_names=TRUE,
                             # default arguments for snake_case::to_any_case
                             sep_in = "\\.",
                             transliterations = "Latin-ASCII",
                             parsing_option = 1,
                             numerals = "asis",
                             ...) {

  # Handling "old_janitor" case for backward compatibility
  if (case == "old_janitor") {
    return(old_make_clean_names(string))
  }

  warn_micro_mu(string=string, replace=replace)
  replaced_names <-
    stringr::str_replace_all(
      string=string,
      pattern=replace
    )
  transliterated_names <-
    if (ascii) {
      stringi::stri_trans_general(
        replaced_names,
        id=available_transliterators(c("Any-Latin", "Greek-Latin", "Any-NFKD", "Any-NFC", "Latin-ASCII"))
      )
    } else {
      replaced_names
    }
  # Remove starting spaces and punctuation
  good_start <-
    stringr::str_replace(
      string=transliterated_names,
      # Description of this regexp:
      # \A: beginning of the string (rather than beginning of the line as ^ would indicate)
      # \h: any horizontal whitespace character (spaces, tabs, and anything else that is a Unicode whitespace)
      # \s: non-unicode whitespace matching (it may overlap with \h)
      # \p{}: indicates a unicode class of characters, so these will also match punctuation, symbols, separators, and "other" characters
      # * means all of the above zero or more times (not + so that the capturing part of the regexp works)
      # (.*)$: captures everything else in the string for the replacement
      pattern="\\A[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]*(.*)$",
      replacement="\\1"
    )
  # Convert all interior spaces and punctuation to single dots
  cleaned_within <-
    stringr::str_replace(
      string=good_start,
      pattern="[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]+",
      replacement="."
    )
  # make.names() is dependent on the locale and therefore will return different
  # system-dependent values (e.g. as in issue #268 with Japanese characters).
  made_names <-
    if (use_make_names) {
      make.names(cleaned_within)
    } else {
      cleaned_within
    }

  cased_names <-
    snakecase::to_any_case(
      made_names,
      case = case,
      sep_in = sep_in,
      transliterations = transliterations,
      parsing_option = parsing_option,
      numerals = numerals,
      ...
    )

  # Handle duplicated names - they mess up dplyr pipelines.  This appends the
  # column number to repeated instances of duplicate variable names.
  while (any(duplicated(cased_names))) {
    dupe_count <-
      vapply(
        seq_along(cased_names), function(i) {
          sum(cased_names[i] == cased_names[1:i])
        },
        1L
      )

    cased_names[dupe_count > 1] <-
      paste(
        cased_names[dupe_count > 1],
        dupe_count[dupe_count > 1],
        sep = "_"
      )
  }
  cased_names
}

#' Avisa si el micro o el mu van a ser sustituidos por limpiar_nombres2()
#'
#' @inheritParams limpiar_nombres2
#' @param character Que caracter debe comprobarse ("micro" o "mu", o ambos)
#' @return TRUE si se emitio una advertencia o FALSE si no se emitio ninguna advertencia
#' @keywords Internal
#' @noRd
warn_micro_mu <- function(string, replace) {
  micro_mu <- names(mu_to_u)
  # The vector of characters that exist but are not handled at all
  warning_characters <- character()
  # The vector of characters that exist and may be handled by a specific replacement
  warning_characters_specific <- character()
  for (current_unicode in micro_mu) {
    # Does the character exist in any of the names?
    has_character <- any(grepl(x=string, pattern=current_unicode, fixed=TRUE))
    if (has_character) {
      # Is there a general replacement for any occurrence of the character?
      has_replacement_general <- any(names(replace) %in% current_unicode)
      # Is there a specific replacement for some form including the character,
      # but it may not cover all of replacements?
      has_replacement_specific <- any(grepl(x=names(replace), pattern=current_unicode, fixed=TRUE))
      warning_characters <-
        c(
          warning_characters,
          current_unicode[!has_replacement_general & !has_replacement_specific]
        )
      warning_characters_specific <-
        c(
          warning_characters_specific,
          current_unicode[!has_replacement_general & has_replacement_specific]
        )
    }
  }
  # Issue the consolidated warnings, if needed
  warning_message_general <- NULL
  if (length(warning_characters) > 0) {
    warning_characters_utf <-
      sprintf("\\u%04x", sapply(X=warning_characters, FUN=utf8ToInt))
    warning_message_general <-
      sprintf(
        "Los siguientes caracteres estan en los nombres a limpiar pero no son reemplazados: %s",
        paste(warning_characters_utf, collapse=", ")
      )
  }
  warning_message_specific <- NULL
  if (length(warning_characters_specific) > 0) {
    warning_characters_utf <-
      sprintf("\\u%04x", sapply(X=warning_characters_specific, FUN=utf8ToInt))
    warning_message_specific <-
      sprintf(
        "Los siguientes caracteres estan en los nombres a limpiar pero no pueden ser reemplazados, compruebe los nombres de salida cuidadosamente: %s",
        paste(warning_characters_utf, collapse=", ")
      )
  }
  if (!is.null(warning_message_general) | !is.null(warning_message_specific)) {
    warning_message <- paste(c(warning_message_general, warning_message_specific), collapse="\n")
    warning(
      "Cuidado",
      "El simbolo mu o micro esta en el vector de entrada, y puede haber sido convertido a \'m\' mientras que \'u\' puede haber sido esperado.  ",
      "Considere a\u00f1adir lo siguiente al argumento `replace`:\n",
      warning_message
    )
  }
  length(c(warning_characters, warning_characters_specific)) > 0
}

# copy of clean_names from janitor v0.3 on CRAN, to preserve old behavior
old_make_clean_names <- function(string) {

  # Takes a data.frame, returns the same data frame with cleaned names
  old_names <- string
  new_names <- old_names %>%
    gsub("\'", "", .) %>% # remove quotation marks
    gsub("\"", "", .) %>% # remove quotation marks
    gsub("%", "percent", .) %>%
    gsub("^[ ]+", "", .) %>%
    make.names(.) %>%
    gsub("[.]+", "_", .) %>% # convert 1+ periods to single _
    gsub("[_]+", "_", .) %>% # fix rare cases of multiple consecutive underscores
    tolower(.) %>%
    gsub("_$", "", .) # remove string-final underscores

  # Handle duplicated names - they mess up dplyr pipelines
  # This appends the column number to repeated instances of duplicate variable names
  dupe_count <- vapply(seq_along(new_names), function(i) {
    sum(new_names[i] == new_names[1:i])
  }, integer(1))

  new_names[dupe_count > 1] <- paste(
    new_names[dupe_count > 1],
    dupe_count[dupe_count > 1],
    sep = "_"
  )
  new_names
}

#' Detect the available transliterators for stri_trans_general
#' @param wanted The transliterators desired for translation
#' @return A semicolon-separated list of the transliterators that are available.
#' @noRd
#' @importFrom stringi stri_trans_list
available_transliterators <- function(wanted) {
  desired_available <- intersect(wanted, stringi::stri_trans_list())
  if (!identical(wanted, desired_available) & getOption("janitor_warn_transliterators", default=TRUE)) {
    warning(
      "Algunos transliteradores para convertir caracteres en nombres no estan disponibles \n",
      "en este sistema.  Los resultados pueden ser diferentes si se ejecuta en un sistema diferente.\n",
      "Los transliteradores que faltan son: ",
      paste0(setdiff(wanted, desired_available), collapse=", "),
      "\n\nEste aviso solo se muestra una vez por sesion.\n",
      "Para suprimirlo, utilice lo siguiente:\n `options(janitor_warn_transliterators=FALSE)`\n",
      "Para que todos los transliteradores esten disponibles en su sistema, reinstale el stringi con:\n",
      '`install.packages(\"stringi\", type=\"source\", configure.args=\"--disable-pkg-config\")`'
    )
    # Only warn once per session
    options(janitor_warn_transliterators=FALSE)
  }
  paste(desired_available, collapse=";")
}


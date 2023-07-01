#' Remover columnas
#' @description
#' Esta funcion fue tomada de la paquetería DataWizard, revisar para detalles.
#' @param data datos
#' @param select seleccionar
#' @param ignore_case Falso por default
#' @param verbose Falso por default
#' @return Un dataframe sin las columnas
#' @keywords internal
#' @export
data_remove <- function(data, select, ignore_case = FALSE, verbose = FALSE, ...) {
  ## TODO set verbose = TRUE by default in a later update?

  # evaluate arguments
  select <- .select_nse(select, data, exclude = NULL, ignore_case)

  # nothing to remove?
  if (!length(select)) {
    return(data)
  }

  new <- data[!colnames(data) %in% select]
  attributes(new) <- utils::modifyList(attributes(data), attributes(new))
  class(new) <- class(data)

  new
}



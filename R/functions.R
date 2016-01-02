


##### In einer oder mehrerer Variablen einer formula auf der rechten Seite (RHS) ersetzen, wobei positionsgetreu ersetzt wird #
#
#' sub_vars_rhs_one2one - Positionstreue ist ein Feature
#'
#' @param form formula
#' @param old zu ersetzende Variablen - stringvector
#' @param new neue Variablen - stringvector
#' @return formula aktualisierte Formel
#' @export
sub_vars_rhs_one2one <- function(form, old, new) {

  if (length(old) != length(new)) stop("length(old) != length(new)")

  lhs <- all.vars(form)[1]
  rhs <- attr(terms(form, keep.order = TRUE), "term.labels") # Weg über terms() erhält die Interaktionsterme

  for (i in 1:length(old)) {
    rhs <- gsub(old[i], new[i], rhs)
  }

  res <-as.formula(paste(lhs, "~", paste(rhs, collapse = "+")), env=environment(form))
  if (res == form) warning("Resulting formula is the same as input formula.")
  return(res)
}

### ALT: extrahiert die ggf. vorhandenen Interaktionsterme nicht
# sub_vars <- function(form, old, new) {
#
#   if (length(old) != length(new)) stop("length(old) != length(new)")
#
#   all.variables <- all.vars(form) # extrahiert die ggf. vorhandenen Interaktionsterme nicht
#
#   rhs <- all.variables[2:length(all.variables)]
#   for (i in 1:length(old)) {
#     rhs <- gsub(old[i], new[i], rhs)
#   }
#
#   return(as.formula(paste(all.variables[1], "~", paste(rhs, collapse = "+")), env=environment(form)))
# }




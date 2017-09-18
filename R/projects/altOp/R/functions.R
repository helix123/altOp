


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

sub_vars_rhs <- function(form, old, new) {
  # Todo Listen, matrizen etc. die eigentlich vectoren sind --> length(dim(new)) < 2 ?
  # Todo Datastreams

  functyp <- 0

  # check input
  #one 2 one replacement, each variable is replaced by exactly one other variable; n zu n
  #one 2 many replacement, each variable is replaced by many variables; 1 zu n bzw. n zu m*n
  if (class(old)=="character" & class(new)=="character" & length(old)==length(new)) {
    functyp <- 1
  } else if (class(old)=="character" & class(new)=="character" & length(old)==1 & length(new) > 1) {
    functyp <- 2
  } else if (class(old)=="character" & length(dim(new))==2 & length(old)==length(new[1,])) {
    functyp <- 3
  }

  if (functyp == 0) stop("incorrect input")

  lhs <- all.vars(form)[1]
  rhs <- attr(terms(form, keep.order = TRUE), "term.labels") # Weg über terms() erhält die Interaktionsterme
  res <- NULL

  if (functyp == 1){
    for (i in 1:length(old)) {
      rhs <- gsub(old[i], new[i], rhs)
    }
    res <-as.formula(paste(lhs, "~", paste(rhs, collapse = "+")), env=environment(form))
  } else if (functyp == 2){
    for (i in 1:length(new)) {
      rhs_e <- gsub(old, new[i], rhs)
      res <- c(res, as.formula(paste(lhs, "~", paste(rhs_e, collapse = "+")), env=environment(form)))
    }
  } else if (functyp == 3){
    # ohne austauschen auch ausgeben? --> option
    all_new <- do.call(expand.grid, split(new, sort(rep(1:ncol(new), nrow(new)))))  # gibts hier eine schönere variante?
    for (i in 1:length(all_new[,1])) {
      rhs_e <- rhs
      for (j in 1:length(all_new[1,])) {
        rhs_e <- gsub(old[j], all_new[i,j], rhs_e)
      }
      res <- c(res, as.formula(paste(lhs, "~", paste(rhs_e, collapse = "+")), env=environment(form)))
    }
  }

  #if (res == form) warning("Resulting formula is the same as input formula.")
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




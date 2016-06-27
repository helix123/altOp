


##### In einer oder mehrerer Variablen einer formula auf der rechten Seite (RHS) ersetzen, wobei positionsgetreu ersetzt wird #
#
#' sub_vars - Positionstreue ist ein Feature
#'
#' @param x formula
#' @param old zu ersetzende Variablen - stringvector
#' @param new neue Variablen - stringvector
#' @return formula aktualisierte Formel # Erwähnen: orginal formel wird bewusst nicht zurückgegeben, user kann sie einfach per c(form, res) vorne anhängen]
#' @export
# TODO: ausführliche examples section, zT aus Vignette nehmen
sub_vars.formula <- function(x, old, new){
  # [Todo] Listen als Input nutzbar machen - ja
  # [Todo] Datastreams (?), Dataframes als Input nutzbar machen - nein
  # [Todo] andere Formelzeichen außer "+" - ja, aber ggf. anderer Subsititionsmechanismus notwendig (über as.character)
  # [Todo] option, dass im fall "many to many" nicht gemischt wird, d.h. 3x3-Matrix --> 3 Formeln
  # [Todo] kompatibilität zu Paketen plm, Formula und lmer - später durch KT, über die S3generics sub_vars.pFormula, sub_vars.Formula etc.
  # [erledigt] Matrizen, die eigentlich Vektoren sind
  # [erledigt] old[1] durch zwei neue, old[2] durch 3 neue ersetzen --> Leerfeld in matrix mit "#NULL"
  # orginal formel wird bewusst nicht zurückgegeben, user kann sie einfach per c(form, res) vorne anhängen

  functyp <- 0

  # check input
  #one 2 one replacement, each variable is replaced by exactly one other variable; n to n
  #one 2 many replacement, each variable is replaced by one or more variables; 1 to n resp.. n to m*n
  #many to many replacement, each variable is replaced by one or more variables
  if (class(old)=="character" & class(new)=="character" & length(old)==length(new)) {
    functyp <- 1
  } else if (class(old)=="character" & class(new)=="character" & length(old)==1 & length(new) > 1) {
    functyp <- 2
  } else if (class(old)=="character" & length(dim(new))==2 & length(old)==length(new[1,])) {
    functyp <- 3
  }

  if (functyp == 0) stop("incorrect input")


  #lhs <- all.vars(x)[1]                                     # Only works if there is only one variable at the LHS
  #rhs <- all.vars(x)[2:length(all.vars(x))]                 # Only works if there is only one variable at the LHS
  rhs <- attr(terms(x, keep.order = TRUE), "term.labels")    # Weg über terms() erhält die Interaktionsterme --> was meinst du damit? => siehe tests
  lhs <- all.vars(x)[1:(length(all.vars(x))-length(rhs))]
  res <- NULL

  # check for all old variables to be included in formula
  for (i in 1:length(old)){
    if (!(old[i] %in% rhs)) {
      warning(paste("WARNING:",old[i],"not present in formula:", deparse(x)))
    }
  }



  if (functyp == 1){
    for (i in 1:length(old)) {
      rhs <- gsub(old[i], new[i], rhs)
    }
    res <-as.formula(paste(paste(lhs, collapse = "+"), "~", paste(rhs, collapse = "+")), env=environment(x))
  } else if (functyp == 2){
    for (i in 1:length(new)) {
      rhs_e <- gsub(old, new[i], rhs)
      res <- c(res, as.formula(paste(paste(lhs, collapse = "+"), "~", paste(rhs_e, collapse = "+")), env=environment(x)))
    }
  } else if (functyp == 3){
    # ohne austauschen auch ausgeben? --> option
    all_new <- do.call(expand.grid, split(new, sort(rep(1:ncol(new), nrow(new)))))  # is there a nicer way?
    for (i in 1:length(all_new[,1])) {
      rhs_e <- rhs
      if ("#NULL" %in% c(t(all_new[i,]))) next()    # #NULL for an empty place in the matrix
      for (j in 1:length(all_new[1,])) {
        rhs_e <- gsub(old[j], all_new[i,j], rhs_e)
      }
      res <- c(res, as.formula(paste(paste(lhs, collapse = "+"), "~", paste(rhs_e, collapse = "+")), env=environment(x)))
    }
  }

  return(res)
}

# Generic
#' @export
sub_vars <- function(x, ...) {
  UseMethod("sub_vars")
}

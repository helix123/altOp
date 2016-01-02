
#' Generate all formulae from given inputs
#'
#' @param abhVar A todo.
#' @param opList A todo.
#' @param festVar todo.
#' @return all formulae as list of formula.
#' @export
generiere_alle_formeln <- function(abhVar, opList, festVar) {

  testenRegressoren <- unlist(opList) # zusaezlich in vector f?r die Weiterverarbeitung konvertieren

  # Erzeugt alle Kombinationen (ohne Teilmengen, d. h. aus jeder testenGrp wird genau eine Variable gewaehlt)
  # => n*m*.. Kombinationen (n, m, ...: Anzahl moeglicher Operationalisierungen f?r jede testenGrp)
  # auch der wechselseitige Ausschluss von Variablen wird beachtet
  regMat_temp <- expand.grid(opList, stringsAsFactors = FALSE)

  # Berechnen von Reihen- und Spaltenanzahl fuer spaeteren data.frame
  reihenAnzahl <- 1L
  for (i in 1:length(opList)) {
    reihenAnzahl <- reihenAnzahl * length(opList[[i]])
  }
  spaltenAnzahl <- 0L
  for (i in 1:length(opList)) {
    spaltenAnzahl <- spaltenAnzahl + length(opList[[i]])
  }

  # regMat_format nimmt alle erzeugten Kombinationen auf
  regMat_format <- data.frame(matrix(NA, nrow = reihenAnzahl, ncol = spaltenAnzahl))
  names(regMat_format) <- testenRegressoren

  # data.frame ins passende Format konvertieren
  for (i in 1:length(opList)) {
    for (j in 1:length(opList[[i]])) {
      regMat_format[opList[[i]][j]] <- (opList[[i]][j] == regMat_temp[i])
    }
  }
  regMat <- regMat_format
  #print("regMat_format:")
  #print(regMat_format)


  ##################
  # Test, ob noch sich wechselseitig ausschliessende Variablen in den erzeugten Kombinationen befinden
  # Solle durch die Art der Generierung allerdings nicht der Fall sein
  # 10.12.2015, KT: weiss gerade nicht mehr, ob das noch sinnvoll ist/adaptierbar ist fuer unsere Zwecke
  ausschlussListe <- opList

  ## Zeilen, die wechselseitige Ausschluesse in der Formel enthalten identifizieren
  zeilenZuLoeschen <- numeric()

  for (zeileI in 1:dim(regMat)[1]) {
    for (m in 1:length(ausschlussListe)) {
      zaehlerTrue <- 0L # Zaehler fuer # sich wechselseitig ausschliessende Kombinationen fuer Variablen
      for (p in 1:length(ausschlussListe[[m]])) {
        if (regMat[zeileI, ausschlussListe[[m]][p]] == TRUE) {zaehlerTrue <- zaehlerTrue + 1}

        if (zaehlerTrue > 1) {
          zeilenZuLoeschen <- append(zeilenZuLoeschen, zeileI)

          break # Mind. 2 sich wechselseitig ausschliessende Variablen in Formel vorhanden;
          # Suche nach weitern Variablen, die in der Formel vorhanden sind, aber nicht sein sollten, kann abgebrochen werden
        }
      }
    }
  }

  ## Als zu loeschen identifizierte Zeilen nun aus Matrix loeschen
  if (length(zeilenZuLoeschen) > 0) {
    zeilenZuLoeschen <- unique(zeilenZuLoeschen) # unique() erforderlich, weil durch die Schleifen Zeilen ggf. mehrfach gezaehlt werden, wenn mehrere Ausschlusskriterien zutreffen
    print(paste0("# Formeln zu loeschen wg. wechselseitigem Ausschluss: ", length(zeilenZuLoeschen)))
    regMat <- regMat[-zeilenZuLoeschen, ]
    print(paste0("Verbleibende # Formeln: ", dim(regMat)[1]))
  } else {
    print("Keine zu loeschenden Formeln wg. wechselseitigem Ausschluss.")
  }

  # Generiere alle formulas fuer die Regressionen
  # 10.12.2015, KT: evtl. environment setzen in formula()
  allFormulaeList <- apply(regMat, 1, function(x) { formula(paste(c(paste(abhVar, " ~ ", paste(festVar, collapse = " + ")), testenRegressoren[x]), collapse = " + ")) })

  return(allFormulaeList)
} # function generiere_alle_formeln



## ---- echo=FALSE, results='asis'-----------------------------------------
library(altOp)
data(CEOcomp)
knitr::kable(head(CEOcomp, 5))

## ------------------------------------------------------------------------
# TODO: adapt parameters, function name etc., once the function is ready
ex1_depVar <- c("compvar") # dependend variable
ex1_opList <- list(c("roa", "roe", "tsr")) # various operationalizations of firm performance
ex1_fixVars <- c("emp", "marketcap") # fixed variables (to appear in all formulae)

ex1_all_formulae <- generiere_alle_formeln(abhVar = ex1_depVar,
                                           opList = ex1_opList,
                                           festVar = ex1_fixVars)

print(ex1_all_formulae)


## ------------------------------------------------------------------------
# TODO: adapt parameters, function name etc., once the function is ready

ex2_depVar <- c("compvar") # dependend variable
ex2_opList <- list(c("roa", "roe", "tsr"), c("marketcap", "emp")) # various operationalizations for firm perf. and firm size
ex2_fixVars <- c("beta") # fixed variables (to appear in all formulae)

ex2_all_formulae <- generiere_alle_formeln(abhVar = ex2_depVar,
                                           opList = ex2_opList,
                                           festVar = ex2_fixVars)

print(ex2_all_formulae)


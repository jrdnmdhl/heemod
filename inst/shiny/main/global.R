REGION <- NULL
COUNTRY <- NULL

try({
  REGION <- get_gho_codes(dimension = "REGION")
  COUNTRY <- get_gho_codes(dimension="COUNTRY")
})

MODULES <- c(
  "Simple equation" = "equation",
  "WHO mortality rate" = "rgho",
  "Survival modeling" = "survival",
  "Time-dependant variable" = "timedep"
)

enableBookmarking(store = "url")
observe_show_module_timedep <- list()
observe_show_module_survival <- list()


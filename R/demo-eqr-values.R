#' Demo simulated eqr values for Model 1 test sites
#'
#' A dataset containing demo observed values. Stored as .csv
#' demo_eqr_values <- utils::read.csv(
#'  system.file("extdat",
#'   "model-m1-gb-10000-simulated-eqr-values-whpt-ntaxa-spring-for-test-sites.csv",
#'          package = "rict"), check.names = F)
#'
#' demo_eqr_values <-  purrr::map_df(names(demo_eqr_values), function(name) {
#'   eqrs <-  list(c(demo_eqr_values[, name]))
#'   NTAXA <- list()
#'   test <- rbind(NTAXA,eqrs)
#'   eqrs <- data.frame(eqrs)
#'   test <-  data.frame(
#'     "RESULT" = name,
#'     "NTAXA EQR" = test,
#'     check.names = F)
#' })
#'
#'
#' @format A data frame with 24 rows and 2 variables:
#' \describe{
#'   \item{RESULT}{Result identifier for each Model 1 GB test site}
#'   \item{NTAXA_SPR}{Simulated EQR values for NTAXA SPR }
#' }
"demo_eqr_values"

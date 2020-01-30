#' Compare the statistical differences between classifications results
#'
#' The Compare function allows the user to assess whether there is a real
#' difference in EQR values and/or status class between a pair of results and/or
#' sites and/or time periods.
#'
#'
#' @param data Standard input data - TBC: default compare site with same name or extra column indicating pairs??
#' @param eqrs list of eqr type to compare for example ASPT, NTAXA, MINTA
#' What this does:
#' Run prediction
#' Run classification only until 10000 EQR have been generated?
#' Store EQI (correct for bias) as well as normal classification outputs? This could be a lot if big run?
#' When all samples have run - check which sites need to be compared
#'

rict_compare <- function(data = rict::demo_observed_values,
                         eqrs = c("ASPT", "NTAXA", "MINTA")) {

  # Test only these sites for now
  data <- demo_observed_values[demo_observed_values$SITE %in%
                                 c("MYR-GB-01-R", "MYR-GB-01-D"), ]
  # validate data
  valid <- rict_validate(data)

  # validate function returns a list - so assign required objects from list
  data <- valid[["data"]]
  checks <- valid[["checks"]]
  model <- valid[["model"]]
  area <- valid[["area"]]

  # Need a  function to identify which result(s)
  # will be compared to which result(s).
  # This could default to something or use extra column in the input data
  # Possible validation errors if users can select which results to compare?
  # PROTOTYPE:
  # Just take first result input as 'x'
  a <- data$SITE[1]
  # Take all the others as 'y'
  b <- unique(data$SITE[4:length(data$SITE) & !data$SITE %in% a ])

  # Run predictions
  predictions <- rict_predict(data, model = model, area = area)

  # Classify - 10000 EQRs are in the ASPT, NTAXA...etc columns
  results <- rict_classify(predictions)
  results$ASPT <- results$eqrs


  # Compare to samples ------------------------------------------------
  compare_test <-  function(results, eqrs, a, b) {
    # loop for each eqr type e.g. NTAXA, ASPT etc
    comparisons_eqr <- lapply(eqrs, function(eqr){
    # loop for each a
    comparisons <- lapply(a, function(a_result){
      # loop for each y
      comparison <- lapply(b, function(b_result) {
        diff <- unlist(results$`eqr`[results$SITE == b_result]) -
          unlist(results$`eqr`[results$SITE == a_result])
        compare <- data.frame(
          "Result A" = a,
          "Result B" = b,
          "Average EQR Result A" =
            mean(unlist(results$eqrs[results$SITE == a])),
          "Average EQR Result B" =
            mean(unlist(results$eqrs[results$SITE == b])),
          "Average EQR: Difference (B - A)" =
            mean(unlist(results$eqrs[results$SITE == b])) -
            mean(unlist(results$eqrs[results$SITE == a])),
          "Standard Deviation of Difference" =
           sd(unlist(results$eqrs[results$SITE == b]) -
            unlist(results$eqrs[results$SITE == a])),
          "Lower 95% (L95) of Difference" = quantile(sort(diff),
                                   probs = c(2.5, 97.5) / 100)[1],
          "Upper 95% (U95) of Difference" = quantile(sort(diff),
                                   probs = c(2.5, 97.5) / 100)[2],
          "2-sided test probability p of No difference in EQR" =
            2 * min(diff[diff >= 0],  diff[diff <= 0]),
            check.names = F, stringsAsFactors = F
        )
        return(compare)
      })
      comparison <- do.call("rbind", comparison)
    })
    comparisons <- do.call("rbind", comparisons)
    })
    comparisons_eqr <- do.call("rbind", comparisons_eqr)
  }

  compare_test <- compare_test(results, eqrs = eqrs[1], a, b)

  ### Create class proportion "confusion table" -------------------------------
  # EQR breaks - not needed!! get from classification function???
  compare_probability <-  function(results, eqrs, a, b) {
    # loop for each eqr type e.g. NTAXA, ASPT etc
    comparisons_eqr <- lapply(eqrs, function(eqr){
    # loop for each a
    comparisons <- lapply(a, function(a_result){
      # loop for each b
      comparison <- lapply(b, function(b_result) {

        eqr_bands <- c(0.0, 0.59, 0.72, 0.86, 0.97, 1.0)
        labels <-  5:1
        a_eqrs <- unlist(results$eqrs[results$SITE == a_result])
        b_eqrs  <- unlist(results$eqrs[results$SITE == b_result])
        # Cut eqr - get this from classification function???
        Results_A <- cut(a_eqrs, breaks = eqr_bands, labels, right = F, include.lowest = T)
        Results_B <- cut(b_eqrs, breaks = eqr_bands, labels, right = F, include.lowest = T)
        # Table proportion
        Results_A <- factor(Results_A, levels = 1:5)
        Results_B <- factor(Results_B, levels = 1:5)

        # table <- table(Results_A, Results_B)
        table_prop <- round(prop.table(table(Results_A, Results_B)), 3)

        # Format table
        # is.na(table) <- sapply(table, is.infinite)
        # table[is.na(table) | table == 0] <- "-"
        table_prop <- as.data.frame(table_prop)
        test <- data.frame(
       "Probability B more than one class Worse than A" =
        sum(table_prop$Freq[table_prop$Results_A %in% c(1,2,3,4) &
                              table_prop$Results_B %in% c(2,3,4,5) &
                              table_prop$Results_B != table_prop$Results_A]),


        "Probability B same Class as A" =
          sum(table_prop$Freq[table_prop$Results_B == table_prop$Results_A]),

        "Probability B more than one class Better than A" =
          sum(table_prop$Freq[table_prop$Results_A %in% c(2,3,4,5) &
                                table_prop$Results_B %in% c(1,2,3,4) &
                                table_prop$Results_B != table_prop$Results_A]),

       check.names = F, stringsAsFactors = F
        )
        return(test)
      })
      comparison <- do.call("rbind", comparison)
    })
    comparisons <- do.call("rbind", comparisons)
    })
    comparisons_eqr <- do.call("rbind", comparisons_eqr)
  }

  compare_probability <- compare_probability(results, eqrs[1], a, b)
  compare_output <- cbind(compare_test, compare_probability)
  return(compare_output)
}
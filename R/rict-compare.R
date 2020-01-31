#' Compare the statistical differences between classifications results
#'
#' The Compare function allows the user to assess whether there is a real
#' difference in EQR values and/or status class between a pair of results and/or
#' sites and/or time periods. What this function does:
#' \enumerate{
#'  \item Runs `rict_predict`.
#'  \item Runs  `rict_classify`
#'  \item Stores EQI (correct for bias) as well as normal classification outputs.
#'  \item When all results have run - check which sites need to be compareds.
#' }
#' @param data Standard input data - TBC: default compare site with same name or
#'   extra column indicating pairs??
#' @param eqrs list of eqr metrics to compare for example ASPT, NTAXA, MINTA
#' @return Dataframe of compare results
#' @export
#' @examples
#' \dontrun{
#' compare <- rict_compare(demo_observed_values)
#' }

rict_compare <- function(data = NULL,
                         eqrs = c("ASPT", "NTAXA")) {
  # validate data
  valid <- suppressMessages(rict_validate(data))

  # validate function returns a list - so assign required objects from list
  data <- valid[["data"]]
  checks <- valid[["checks"]]
  model <- valid[["model"]]
  area <- valid[["area"]]

  # Run predictions
  predictions <- rict_predict(data, model = model, area = area)

  # Classify - 10000 EQRs are in the ASPT, NTAXA...etc columns
  results <- rict_classify(predictions, store_eqrs = T)
  #results$ASPT <- results$eqrs
  print(results)
  # Need a  function to identify which result(s)
  # will be compared to which result(s).
  # This could default to something or use extra column in the input data
  # Possible validation errors if users can select which results to compare?
  # PROTOTYPE:
  # Compare everything!!
  message("Comparing results...")
  results$RESULT <- paste(results$SITE, results$YEAR)
  a <- unique(results$RESULT)
  # Take all the others as 'y'
  b <- unique(results$RESULT)

  ## Statistical compare test ---------------------------------------------------------
  compare_test <-  function(results, eqrs, a, b) {
    # loop for each eqr type e.g. NTAXA, ASPT etc
    loop_eqr_types <- lapply(eqrs, function(eqr){
    # loop for each result(s) "a"
    loop_a <- lapply(a, function(a_result){
      # loop for each result(s) "b"
      loop_b <- lapply(b, function(b_result) {
        if(a_result == b_result) {
          return()
        }
        # calculate difference between a and b eqrs
        diff <- unlist(results[results$RESULT == b_result, eqr]) -
          unlist(results[results$RESULT == a_result, eqr])
        # make dataframe of useful test statistics
        print("Hello from compare test")
        compare <- data.frame(
          "EQR metric compared" = eqr,
          "Result A" = a_result,
          "Result B" = b_result,
          "Average EQR for Result A" =
            mean(unlist(results[results$RESULT == a_result, eqr])),
          "Average EQR for Result B" =
            mean(unlist(results[results$RESULT == b_result, eqr])),
          "Average EQR: Difference (B - A)" =
            mean(unlist(results[results$RESULT == b_result, eqr])) -
            mean(unlist(results[results$RESULT == a_result , eqr])),
          "Standard Deviation of Difference" =
           sd(unlist(results[results$RESULT == b_result, eqr]) -
            unlist(results[results$RESULT == a_result, eqr])),
          "Lower 95% (L95) of Difference" = quantile(sort(diff),
                                   probs = c(2.5, 97.5) / 100)[[1]][1],
          "Upper 95% (U95) of Difference" = quantile(sort(diff),
                                   probs = c(2.5, 97.5) / 100)[[2]][2],
          "2-sided test probability p of No difference in EQR" =
            2 * min(diff[diff >= 0],  diff[diff <= 0]),
            check.names = F, stringsAsFactors = F
        )
        return(compare)
      })
      comparison <- do.call("rbind", loop_b) # bind all "b" result(s)
    })
    comparisons <- do.call("rbind", loop_a) # bind all "a" results(s)
    })
    comparisons_eqr <- do.call("rbind", loop_eqr_types) # bind all eqr type
  }

  compare_test <- compare_test(results, eqrs = eqrs, a, b)
  print(compare_test)
  ## Create class proportion "confusion table" ---------------------------------------
  # EQR breaks - not needed!! get from classification function???
  compare_probability <-  function(results, eqrs, a, b) {
    # loop for each eqr type e.g. NTAXA, ASPT etc
    comparisons_eqr <- lapply(eqrs, function(eqr){
    # loop for each a
    comparisons <- lapply(a, function(a_result){
      # loop for each b
      comparison <- lapply(b, function(b_result) {
        if(a_result == b_result) {
          return()
        }
        print("Hello from compare_probability")
        eqr_bands <- c(0.0, 0.59, 0.72, 0.86, 0.97, 1.0)
        labels <-  5:1
        a_eqrs <- unlist(results[results$RESULT == a_result, eqr])
        b_eqrs  <- unlist(results[results$RESULT == b_result, eqr])
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
        classes <- data.frame(

        "Probability B more than one class Worse than A" =
            sum(table_prop$Freq[table_prop$Results_A %in% c(1,2,3) &
                                  table_prop$Results_B %in% c(3,4,5) &
                                  table_prop$Results_B != table_prop$Results_A]),
       "Probability B one class Worse than A" =
        sum(table_prop$Freq[table_prop$Results_A %in% c(1,2,3,4) &
                              table_prop$Results_B %in% c(2,3,4,5) &
                              table_prop$Results_B != table_prop$Results_A]),


        "Probability B same Class as A" =
          sum(table_prop$Freq[table_prop$Results_B == table_prop$Results_A]),

        "Probability B one class Better than A" =
          sum(table_prop$Freq[table_prop$Results_A %in% c(2,3,4,5) &
                                table_prop$Results_B %in% c(1,2,3,4) &
                                table_prop$Results_B != table_prop$Results_A]),

       "Probability B more than one class Better than A" =
         sum(table_prop$Freq[table_prop$Results_A %in% c(3,4,5) &
                               table_prop$Results_B %in% c(1,2,3) &
                               table_prop$Results_B != table_prop$Results_A]),

       check.names = F, stringsAsFactors = F
        )

        table_prop$name <- paste("Prob A in", table_prop$Results_A,
                                  "& B in", table_prop$Results_B)
        t(table_prop[, c("name", "Freq")])
        table <-   data.frame(t(table_prop[, c( "Freq")]))
        names(table) <- table_prop$name
        class_table <- cbind(classes, table)
        return(class_table)
      })
      comparison <- do.call("rbind", comparison)
    })
    comparisons <- do.call("rbind", comparisons)
    })
    comparisons_eqr <- do.call("rbind", comparisons_eqr)
  }

  compare_probability <- compare_probability(results, eqrs, a, b)
  print(compare_probability)
  compare_output <- cbind(compare_test, compare_probability)
  return(compare_output)
}
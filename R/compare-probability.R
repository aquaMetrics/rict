## Create class proportion "confusion table" ---------------------------------------
compare_probability <- function(results, eqrs, a, b) {
  # loop for each eqr type e.g. NTAXA, ASPT etc
  comparisons_eqr <- lapply(eqrs, function(eqr) {
    # loop for each a
    comparisons <- lapply(a, function(a_result) {
      # loop for each b
      comparison <- lapply(b, function(b_result) {
        # check not comparing same result
        if (a_result == b_result) {
          return()
        }
        a_eqrs <- unlist(results[results$RESULT == a_result, eqr])
        b_eqrs <- unlist(results[results$RESULT == b_result, eqr])

        # Use classification function to get eqr band
        if (grepl("aspt", eqr, ignore.case = T)) {
          Results_A <- getClassarray_aspt(data.frame(a_eqrs))
          Results_B <- getClassarray_aspt(data.frame(b_eqrs))
        }
        if (grepl("ntaxa", eqr, ignore.case = T)) {
          Results_A <- getClassarray_ntaxa(data.frame(a_eqrs))
          Results_B <- getClassarray_ntaxa(data.frame(b_eqrs))
        }

        ## EQR breaks - could be provided to make more generic?
        # eqr_bands <- c(0.0, 0.59, 0.72, 0.86, 0.97, 1.0) # NTAXA
        # eqr_bands <- c(0.0, 0.47, 0.56, 0.68, 0.8, 1.0) # ASPT
        # labels <-  5:1
        # Results_A <- cut(a_eqrs, breaks = eqr_bands, labels, include.lowest = T)
        # Results_B <- cut(b_eqrs, breaks = eqr_bands, labels, include.lowest = T)

        # Need to factorise the results to create table
        Results_A <- factor(Results_A[, 1], levels = 1:5)
        Results_B <- factor(Results_B[, 1], levels = 1:5)
        # Cut EQRs into 1 to 5 classes
        Results_A <- cut(as.numeric(Results_A),
          breaks = c(0, 1, 2, 3, 4, 5),
          labels = c(1:5),
          include.lowest = T
        )
        Results_B <- cut(as.numeric(Results_B),
          breaks = c(0, 1, 2, 3, 4, 5),
          labels = c(1:5),
          include.lowest = T
        )

        # Cut EQRs into High to Bad status (for display purposes)
        results_a_class <- cut(as.numeric(Results_A),
          breaks = c(0, 1, 2, 3, 4, 5),
          labels = c("High", "Good", "Moderate", "Poor", "Bad"),
          include.lowest = T
        )
        results_b_class <- cut(as.numeric(Results_B),
          breaks = c(0, 1, 2, 3, 4, 5),
          labels = c("High", "Good", "Moderate", "Poor", "Bad"),
          include.lowest = T
        )

        # Create table for both class and status (for display purposes)
        table_prop_class <- round(prop.table(table(results_a_class, results_b_class)), 4) * 100
        table_prop <- round(prop.table(table(Results_A, Results_B)), 4) * 100

        # Calculate most probable class for A and B results (this gets re-used a few times)
        prob_class_result_a <- as.integer(cut(which.max(table(Results_A)),
          breaks = c(0, 1, 2, 3, 4, 5),
          labels = 1:5,
          include.lowest = T
        ))

        prob_class_result_b <- as.integer(cut(which.max(table(Results_B)),
          breaks = c(0, 1, 2, 3, 4, 5),
          labels = 1:5,
          include.lowest = T
        ))

        # Calculate most probable status for A (for display purposes)
        prob_status_result_a <- cut(which.max(table(Results_A)),
          breaks = c(0, 1, 2, 3, 4, 5),
          labels = c("H", "G", "M", "P", "B"),
          include.lowest = T
        )
        prob_status_result_b <- cut(which.max(table(Results_B)),
          breaks = c(0, 1, 2, 3, 4, 5),
          labels = c("H", "G", "M", "P", "B"),
          include.lowest = T
        )

        # Format table
        # is.na(table) <- sapply(table, is.infinite)
        # table[is.na(table) | table == 0] <- "-"

        # Pivot tbale into dataframe to make summing up probabilities easier
        table_prop <- as.data.frame(table_prop)
        table_prop_class <- as.data.frame(table_prop_class)

        # Create table for probable status, + same, one worse and more than
        # one worse probablilites.
        classes <- data.frame(
          "Result A" = a_result,
          "Result B" = b_result,
          "Most Probable Class for Result A" =
            prob_status_result_a,
          "Probability of most likely class for Result A" =
            max(prop.table(table(Results_A))) * 100,
          "Most Probable Class for Result B" =
            prob_status_result_b,
          "Probability of most likely class for Result B" =
            max(prop.table(table(Results_B))) * 100,
          "Probability B more than one class Worse than A" =
            sum(table_prop$Freq[table_prop$Results_A %in% prob_class_result_a &
              table_prop$Results_B %in% c(5:2 + prob_class_result_a)]),
          "Probability B one class Worse than A" =
            sum(table_prop$Freq[table_prop$Results_A %in% prob_class_result_a &
              table_prop$Results_B %in% c(5:1 + prob_class_result_a)]),
          "Probability B same Class as A" =
            sum(table_prop$Freq[table_prop$Results_B == table_prop$Results_A]),
          "Probability B one class Better than A" =
            sum(table_prop$Freq[table_prop$Results_B %in% prob_class_result_b &
              table_prop$Results_A %in% c(5:1 + prob_class_result_a)]),
          "Probability B more than one class Better than A" =
            sum(table_prop$Freq[table_prop$Results_B %in% prob_class_result_b &
              table_prop$Results_A %in% c(5:2 + prob_class_result_a)]),

          check.names = F, stringsAsFactors = F
        )

        # Label probablities with pretty names
        table_prop$name <- paste(
          "Probability A in", table_prop_class$results_a_class,
          "& B in", table_prop_class$results_b_class
        )
        # pivot 'confusion table' into one row so can be joined with 'classes' table
        table <- data.frame(t(table_prop[, c("Freq")]))
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
## Create class proportion "confusion table" ---------------------------------------
compare_probability <- function(a, b, eqr = NULL,
                                eqr_bands = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) {

  # Use classification function form RICT to get eqr bands
  if (!is.null(eqr) && grepl("aspt", eqr, ignore.case = T)) {
    Results_A <- getClassarray_aspt(data.frame(a))
    Results_B <- getClassarray_aspt(data.frame(b))
  }
  if (!is.null(eqr) && grepl("ntaxa", eqr, ignore.case = T)) {
    Results_A <- getClassarray_ntaxa(data.frame(a))
    Results_B <- getClassarray_ntaxa(data.frame(b))
  } else {
    # Generic EQR breaks - to make more generic for other WFD classification tools
    labels <-  5:1
    a[a > 1] <- 1
    a[a < 0] <- 0
    b[b > 1] <- 1
    b[b < 0] <- 0
    Results_A <- data.frame(cut(a, breaks = eqr_bands, labels, include.lowest = T))
    Results_B <- data.frame(cut(b, breaks = eqr_bands, labels, include.lowest = T))
    message(paste0("No EQR boundaries detected for '", eqr, "' using ",
                  paste(eqr_bands,collapse = ', ' )))
  }

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
  table_prop$Results_B <- as.numeric(table_prop$Results_B)
  table_prop$Results_A <- as.numeric(table_prop$Results_A)

  # Create table for probable status, + same, one worse and more than
  # one worse probablilites.
  classes <- data.frame(
    "Most Probable Class for Result A" =
      prob_status_result_a,
    "Probability of most likely class for Result A" =
      max(prop.table(table(Results_A))) * 100,
    "Most Probable Class for Result B" =
      prob_status_result_b,
    "Probability of most likely class for Result B" =
      max(prop.table(table(Results_B))) * 100,
    "Probability B more than one class Worse than A" =
      sum(table_prop$Freq[table_prop$Results_B > table_prop$Results_A + 1]),
    "Probability B one class Worse than A" =
      sum(table_prop$Freq[table_prop$Results_B == table_prop$Results_A + 1]),
    "Probability B same Class as A" =
      sum(table_prop$Freq[table_prop$Results_B == table_prop$Results_A]),
    "Probability B one class Better than A" =
      sum(table_prop$Freq[table_prop$Results_B == table_prop$Results_A - 1]),
    "Probability B more than one class Better than A" =
      sum(table_prop$Freq[table_prop$Results_B < table_prop$Results_A - 1]),

    check.names = F, stringsAsFactors = F
  )

  # Label probablities with pretty names
  table_prop$name <- paste(
    "Probability Result A in", table_prop_class$results_a_class,
    "& Result B in", table_prop_class$results_b_class
  )

  # Pivot 'confusion table' into one row so can be joined with 'classes' table
  table <- data.frame(t(table_prop[, c("Freq")]))
  names(table) <- table_prop$name
  table <- table[, c(
    "Probability Result A in High & Result B in High",
    "Probability Result A in High & Result B in Good",
    "Probability Result A in High & Result B in Moderate",
    "Probability Result A in High & Result B in Poor",
    "Probability Result A in High & Result B in Bad",
    "Probability Result A in Good & Result B in High",
    "Probability Result A in Good & Result B in Good",
    "Probability Result A in Good & Result B in Moderate",
    "Probability Result A in Good & Result B in Poor",
    "Probability Result A in Good & Result B in Bad",
    "Probability Result A in Moderate & Result B in High",
    "Probability Result A in Moderate & Result B in Good",
    "Probability Result A in Moderate & Result B in Moderate",
    "Probability Result A in Moderate & Result B in Poor",
    "Probability Result A in Moderate & Result B in Bad",
    "Probability Result A in Poor & Result B in High",
    "Probability Result A in Poor & Result B in Good",
    "Probability Result A in Poor & Result B in Moderate",
    "Probability Result A in Poor & Result B in Poor",
    "Probability Result A in Poor & Result B in Bad",
    "Probability Result A in Bad & Result B in High",
    "Probability Result A in Bad & Result B in Good",
    "Probability Result A in Bad & Result B in Moderate",
    "Probability Result A in Bad & Result B in Poor",
    "Probability Result A in Bad & Result B in Bad")]
  class_table <- cbind(classes, table)
  return(class_table)
}
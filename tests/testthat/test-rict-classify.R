context("test rict_classify")

test_that("Outputs match azure single-year outputs", {
  data <- utils::read.csv(system.file("extdat",
    "validation-input-single-year.csv",
    package = "rict"
  ),
  check.names = F, stringsAsFactors = F
  )
  data <- data[1, ] # only one year required
  test_validation_func <- rict:::rict_validate(data)
  predictions <- rict_predict(data)
  classification <- rict_classify(predictions, year_type = "single")
  expect_equal(class(classification), "data.frame")

  validation_classification <- utils::read.csv(system.file("extdat",
    "validation-class-single-year.csv",
    package = "rict"
  ))

  classification$mintawhpt_spr_aut_mostProb <-
    as.character(classification$mintawhpt_spr_aut_mostProb)
  validation_classification$mintawhpt_spr_aut_mostProb <-
    as.character(validation_classification$mintawhpt_spr_aut_mostProb)

  equal <- all.equal(
    classification$mintawhpt_spr_aut_mostProb,
    validation_classification$mintawhpt_spr_aut_mostProb[1]
  )
  expect_true(equal == T)
})


test_that("Outputs match azure multi-year outputs", {
 # skip("currently failing because change to set.seed code")
  predictions <- rict_predict(demo_observed_values)
  classification <- rict_classify(predictions)
  expect_equal(class(classification), "data.frame")

  # compare results downloaded from azure with package (Results on azure were manually user tested).
  validation_classification <- utils::read.csv(system.file("extdat",
    "validation-class-multi-year.csv",
    package = "rict"
  ))
  classification <- classification[, names(classification) %in% names(validation_classification)]

  # match up class between azure results and package:
  classification$WATERBODY <- as.character(classification$WATERBODY)
  validation_classification$WATERBODY <- as.character(validation_classification$WATERBODY)

  classification$H_NTAXA_spr_aut <- as.character(classification$H_NTAXA_spr_aut)
  classification$H_NTAXA_spr_aut <- as.numeric(classification$H_NTAXA_spr_aut)
  classification$G_NTAXA_spr_aut <- as.character(classification$G_NTAXA_spr_aut)
  classification$G_NTAXA_spr_aut <- as.numeric(classification$G_NTAXA_spr_aut)
  classification$M_NTAXA_spr_aut <- as.character(classification$M_NTAXA_spr_aut)
  classification$M_NTAXA_spr_aut <- as.numeric(classification$M_NTAXA_spr_aut)
  classification$P_NTAXA_spr_aut <- as.character(classification$P_NTAXA_spr_aut)
  classification$P_NTAXA_spr_aut <- as.numeric(classification$P_NTAXA_spr_aut)
  classification$B_NTAXA_spr_aut <- as.character(classification$B_NTAXA_spr_aut)
  classification$B_NTAXA_spr_aut <- as.numeric(classification$B_NTAXA_spr_aut)
  classification$mostProb_NTAXA_spr_aut <- as.character(classification$mostProb_NTAXA_spr_aut)
  validation_classification$mostProb_NTAXA_spr_aut <- as.character(validation_classification$mostProb_NTAXA_spr_aut)

  classification$H_ASPT_spr_aut <- as.character(classification$H_ASPT_spr_aut)
  classification$H_ASPT_spr_aut <- as.numeric(classification$H_ASPT_spr_aut)
  classification$G_ASPT_spr_aut <- as.character(classification$G_ASPT_spr_aut)
  classification$G_ASPT_spr_aut <- as.numeric(classification$G_ASPT_spr_aut)
  classification$M_ASPT_spr_aut <- as.character(classification$M_ASPT_spr_aut)
  classification$M_ASPT_spr_aut <- as.numeric(classification$M_ASPT_spr_aut)
  classification$P_ASPT_spr_aut <- as.character(classification$P_ASPT_spr_aut)
  classification$P_ASPT_spr_aut <- as.numeric(classification$P_ASPT_spr_aut)
  classification$B_ASPT_spr_aut <- as.character(classification$B_ASPT_spr_aut)
  classification$B_ASPT_spr_aut <- as.numeric(classification$B_ASPT_spr_aut)
  classification$mostProb_ASPT_spr_aut <- as.character(classification$mostProb_ASPT_spr_aut)
  validation_classification$mostProb_ASPT_spr_aut <- as.character(validation_classification$mostProb_ASPT_spr_aut)

  classification$mintawhpt_spr_aut_H_MINTA_ <- as.character(classification$mintawhpt_spr_aut_H_MINTA_)
  classification$mintawhpt_spr_aut_H_MINTA_ <- as.numeric(classification$mintawhpt_spr_aut_H_MINTA_)
  classification$mintawhpt_spr_aut_G_MINTA_ <- as.character(classification$mintawhpt_spr_aut_G_MINTA_)
  classification$mintawhpt_spr_aut_G_MINTA_ <- as.numeric(classification$mintawhpt_spr_aut_G_MINTA_)
  classification$mintawhpt_spr_aut_M_MINTA_ <- as.character(classification$mintawhpt_spr_aut_M_MINTA_)
  classification$mintawhpt_spr_aut_M_MINTA_ <- as.numeric(classification$mintawhpt_spr_aut_M_MINTA_)
  classification$mintawhpt_spr_aut_P_MINTA_ <- as.character(classification$mintawhpt_spr_aut_P_MINTA_)
  classification$mintawhpt_spr_aut_P_MINTA_ <- as.numeric(classification$mintawhpt_spr_aut_P_MINTA_)
  classification$mintawhpt_spr_aut_B_MINTA_ <- as.character(classification$mintawhpt_spr_aut_B_MINTA_)
  classification$mintawhpt_spr_aut_B_MINTA_ <- as.numeric(classification$mintawhpt_spr_aut_B_MINTA_)
  classification$mintawhpt_spr_aut_mostProb_MINTA_ <- as.character(classification$mintawhpt_spr_aut_mostProb_MINTA_)
  validation_classification$mintawhpt_spr_aut_mostProb_MINTA_ <- as.character(
    validation_classification$mintawhpt_spr_aut_mostProb_MINTA_
  )

  # remove row.names - not required for comparison
  row.names(classification) <- NULL
  row.names(validation_classification) <- NULL

  # test azure and package results match:
  equal <- all.equal(
    classification[, 1:23],
    validation_classification[, 1:23]
  )
  expect_true(equal == T)
})

test_that("Outputs on SEPA system", {
  skip("internal sepa test only")
  library(sepaTools)
  ecology_results <- getEcologyResults(
    locations = 122480,
    startDate = "01-JAN-2013",
    endDate = "17-NOV-2017"
  )

  observed_values <- transformRict(ecology_results)
  predictions <- rict::rict_predict(observed_values)
})

test_that("GIS variables classification against Ralph's output", {
  library(dplyr)
  library(tidyr)
  data("demo_gis_values_log")
  demo_gis_values$WATERBODY <- demo_gis_values_log$SITE
  predictions <- rict_predict(demo_gis_values_log)
  results <- rict_classify(predictions, year_type = "single")
  results_two <- rict(demo_gis_values_log, year_type = "single")
  # test that creating predictions then classifying works the same as  going straight to
  # classifying
  equal <- all.equal(
    results,
    results_two
  )
  expect_true(equal == T)
  # remove non-required  predictions variables
  predictions <- select(predictions, -starts_with("p"))
  # need both predictions and classificatoin outputs to fully check classification
  output <- inner_join(predictions, results, by = c("SITE" = "SITE"))
  # tidy data so it matches test data format
  output <- as.data.frame(t(output))
  output$SITE <- row.names(output)
  names(output)[1:24] <- c(as.matrix(filter(output, SITE == "SITE")))[1:24]
  # read in test data to check against
  test_data <- utils::read.csv(system.file("extdat",
    "test-sites-gb-model-44-classification-draft.csv",
    package = "rict"
  ),
  check.names = F, stringsAsFactors = F
  )

  # filter only things that match
  output <- filter(output, SITE %in% test_data$SITE)
  test_data <- filter(test_data, SITE %in% output$SITE)
  # select columns in same order
  output <- select(output, SITE, everything())
  output <- arrange(output, SITE)
  test_data <- arrange(test_data, SITE)

  output <- output %>% pivot_longer(-SITE, names_to = "SITES", values_to = "count")
  output <- output %>% pivot_wider(names_from = SITE, values_from = "count")

  test_data <- test_data %>% pivot_longer(-SITE, names_to = "SITES", values_to = "count")
  test_data <- test_data %>% pivot_wider(names_from = SITE, values_from = "count")
  # check differences!
  test_data <- type.convert(test_data)
  output <- type.convert(output)
  test <- data.frame(select_if(test_data, is.numeric))
  test2 <- data.frame(select_if(output, is.numeric))

  test3 <- 100 / (test + 1) * (test2 + 1) - 100
  # check end groups don't differ on average more than 1.5% - (Sampling error?)
  # this is not a very good test as it takes the mean! but Ralph happy that results match
  expect_true(mean(t(test3)) < 0.686)
  # write.csv(test_data, file = "testing-data-from-ralph.csv")
  # write.csv(output, file = "r-output.csv")
  # write.csv(results, file = "r-output-standard.csv")
})


test_that("Dynamically changing output depending on data provided", {
  skip("work in progress")
  remove_cols <- grep("Sum_Ntaxa|Spr_Ntaxa", names(demo_observed_values))
  demo_observed_values[, remove_cols] <- NA
  test <- rict(demo_observed_values, year_type = "single")
})

test_that("Test single row of multi-year input works", {

  # Data contains single year / single row sites at start and end of input file
  single_row_test <- utils::read.csv(system.file("extdat",
                                                           "test-data-single-site-multi-year.csv",
                                                           package = "rict"
  ), check.names = F)
  # Run data through multi-year classification and check output is created for all sites
  sites <- unique(single_row_test$SITE)
  check <- rict(single_row_test)
  expect_equal(sort(as.character(unique(check$SITE))), sort(as.character(sites)))

  # Test a single row input also works
  check <- rict(single_row_test[1, ])
  # Quick test to see it return a value
  expect_gte(as.numeric(as.character(check$H_NTAXA_spr_aut)), 0)

})

test_that("Test summer", {
  skip("work in progress")
  demo_observed_values <- demo_observed_values
  demo_gis_values_log <- demo_gis_values_log
  demo_observed_values$SITE <- demo_gis_values_log$SITE

  predictions <- rict_predict(demo_observed_values)
  results <- rict:::summer_single_year_classification(predictions, area = "gb")

})

test_that("Test missing seasons", {
  skip("work in progress")
  demo_observed_values <- rict::demo_observed_values

  demo_observed_values <- demo_observed_values[1 , ]

  demo_observed_values$Spr_Season_ID <- NA
  demo_observed_values$Spr_Ntaxa_Bias <- NA
  demo_observed_values$`Spr_TL2_WHPT_ASPT (AbW,DistFam)` <- NA
  demo_observed_values$`Spr_TL2_WHPT_NTaxa (AbW,DistFam)` <- NA

  class <- rict(demo_observed_values, year_type = "single", store_eqrs = TRUE)
  class2 <- rict(demo_observed_values, year_type = "single", store_eqrs = TRUE)

test <-    rict_compare(class, class2)

})




context("test rict_predict")

test_that("rict_predict for physical variables", {
  predictions <- rict_predict(data = demo_observed_values)

  expected_predictions <- utils::read.csv(system.file("extdat",
    "validation-prediction-multi-year.csv",
    package = "rict"
  ))

  # issue with rounding on LONGITUDE? Not real impact on prediction outputs
  # so rounding both LONGITUDE to 6 digits
  expected_predictions$LONGITUDE <- round(expected_predictions$LONGITUDE, 6)
  predictions$LONGITUDE <- round(predictions$LONGITUDE, 6)

  expected_predictions$SuitCode <- as.factor(expected_predictions$SuitCode)
  names(expected_predictions)[17] <- "belongs_to_end_grp"
  expected_predictions$belongs_to_end_grp <- NULL
  predictions$belongs_to_end_grp <- NULL

  expected_predictions <- dplyr::arrange(expected_predictions, SITE, TL2_WHPT_NTAXA_AbW_DistFam_spr)
  predictions <- dplyr::arrange(predictions, SITE, TL2_WHPT_NTAXA_AbW_DistFam_spr)

  equal <- all.equal(
    predictions[, names(predictions) %in% names(expected_predictions)],
    expected_predictions
  )

  expect_true(equal == T)
})

test_that("end group means for GIS variables", {
  # only need 12 sites for end group means (the 24 tests sites are repeated)
  test <- rict_validate(data = demo_gis_values_log)[[1]][1:12, ]
  predictions <- rict_predict(data = demo_gis_values_log)[1:12, ]
  # load unadjusted reference predicted values:
  expected_predictions <- utils::read.csv(system.file("extdat",
    "expected-unadjusted-biotic-scores-model-44.csv",
    package = "rict"
  ), check.names = F)

  expected_end_group <- utils::read.csv(system.file("extdat",
    "test-expected-end-group-model-44.csv",
    package = "rict"
  ), check.names = F, header = F)
  expected_end_group <- data.frame(t(expected_end_group))
  names(expected_end_group) <- paste0("p", 1:43)

  # expected_end_group <- dplyr::arrange(expected_end_group,  SITE, TL2_WHPT_NTAXA_AbW_DistFam_spr)
  # predictions     <-  dplyr::arrange(predictions, SITE, TL2_WHPT_NTAXA_AbW_DistFam_spr)
  # expected_predictions <- dplyr::arrange(expected_predictions,  SITE, TL2_WHPT_NTAXA_AbW_DistFam_spr)

  test <- predictions[, names(predictions) %in% names(expected_end_group)] -
    expected_end_group[, names(expected_end_group) %in% names(predictions)]
  # check end groups don't differ more than 0.00038 - Ralph's rule of thumb
  expect_true(max(test) < 0.00038)

  # Note: lat/lon calculation converts NGR slightly different to values and rounding on
  # log values may also cause slight changes. So rounding output to 2 decimal places:
  equal <- all.equal(
    round(predictions[, names(predictions) %in% names(expected_predictions)][2:5], 2),
    round(expected_predictions[, names(expected_predictions) %in% names(predictions)][2:5], 2)
  )

  expect_true(equal == T)
})

test_that("NI predictions", {
  ni_data <-
    utils::read.csv(system.file("extdat",
      "ni-model-1-test-data.csv",
      package = "rict"
    ), check.names = F)

  test <- rict(ni_data, year_type = "single")
})

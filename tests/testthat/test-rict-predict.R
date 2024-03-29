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
  # expected_predictions$belongs_to_end_grp <- NULL
  # predictions$belongs_to_end_grp <- NULL

  expected_predictions <- dplyr::arrange(expected_predictions, SITE, TL2_WHPT_NTAXA_AbW_DistFam_spr)
  predictions <- dplyr::arrange(predictions, SITE, TL2_WHPT_NTAXA_AbW_DistFam_spr)
  # correct mis-match in types before test:
  predictions$SuitCode <- as.numeric(predictions$SuitCode)
  expected_predictions$SuitCode <- as.numeric(expected_predictions$SuitCode)
  # trim names to match names in expected file
  predictions <- predictions[, names(predictions) %in% names(expected_predictions)]
  equal <- all.equal(
    predictions,
    expected_predictions
  )

  expect_true(equal == TRUE)
})

test_that("end group means for GIS variables", {
  # only need 12 sites for end group means (the 24 tests sites are repeated)
  test <- rict_validate(data = demo_gis_values_log)[[1]][1:12, ]
  predictions <- rict_predict(data = demo_gis_values_log)[1:12, ]
  # load unadjusted reference predicted values:
  expected_predictions <- utils::read.csv(system.file("extdat",
    "expected-unadjusted-biotic-scores-model-44.csv",
    package = "rict"
  ), check.names = FALSE)

  expected_end_group <- utils::read.csv(system.file("extdat",
    "test-expected-end-group-model-44.csv",
    package = "rict"
  ), check.names = FALSE, header = FALSE)
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
  # log values may also cause slight changes. So rounding output to 1 decimal places:
  predictions$TL2_WHPT_NTAXA_AbW_DistFam_aut <- round(predictions$TL2_WHPT_NTAXA_AbW_DistFam_aut, 1)
  expected_predictions$TL2_WHPT_NTAXA_AbW_DistFam_aut <- round(expected_predictions$TL2_WHPT_NTAXA_AbW_DistFam_aut, 1)

  equal <- all.equal(
    round(predictions[, names(predictions) %in% names(expected_predictions)][2:5], 2),
    round(expected_predictions[, names(expected_predictions) %in% names(predictions)][2:5], 2)
  )

  expect_true(equal == TRUE)
})


test_that("all_indices predictions work GB", {
  demo_observed_values <-
    utils::read.csv(system.file("extdat",
      "test-data-model-1.csv",
      package = "rict"
    ), check.names = FALSE)

  indices_prediction <- rict_predict(demo_observed_values[1:12, ], all_indices = TRUE)
  verified_predictions <-
    utils::read.csv(system.file("extdat",
      "rict-all-indices-predictions.csv",
      package = "rict"
    ), check.names = FALSE)

  indices_prediction <- indices_prediction[, names(indices_prediction) %in% names(verified_predictions)]
  verified_predictions <- verified_predictions[, names(verified_predictions) %in% names(indices_prediction)]
  expect_equal(indices_prediction[60:135], verified_predictions[60:135])
})


test_that("taxa predictions work GB", {
  demo_observed_values <-
    utils::read.csv(system.file("extdat",
      "test-data-model-1.csv",
      package = "rict"
    ), check.names = FALSE)

  indices_prediction <- rict_predict(demo_observed_values[1, ], taxa = TRUE)
  verified_predictions <-
    utils::read.csv(system.file("extdat",
      "rict-gb-taxa-prediction.csv",
      package = "rict"
    ), check.names = FALSE, stringsAsFactors = TRUE)

  indices_prediction <- indices_prediction[, names(indices_prediction) %in% names(verified_predictions)]
  verified_predictions <- verified_predictions[, names(verified_predictions) %in% names(indices_prediction)]
  expect_equal(indices_prediction[, 11:18], verified_predictions[, 11:18]) # all probabilities the same
})

test_that("All indices predictions work NI", {
  data <- demo_ni_observed_values

  indices_prediction <- rict_predict(data, all_indices = TRUE, crs = 29903)
  verified_predictions <-
    utils::read.csv(system.file("extdat",
      "rict-ni-all-indices-predictions.csv",
      package = "rict"
    ), check.names = FALSE, stringsAsFactors = TRUE) # from tested Azure data

  indices_prediction <- indices_prediction[, names(indices_prediction) %in% names(verified_predictions)]
  verified_predictions <- verified_predictions[, names(verified_predictions) %in% names(indices_prediction)]
  expect_equal(indices_prediction[, 26:105], verified_predictions[, 26:105]) # check all indices columns the same
})


test_that("taxa predictions work NI", {
  taxa_prediction <- rict_predict(demo_ni_observed_values[1, ],
                                  taxa = TRUE,
                                  crs = 29903)
  verified_predictions <-
    utils::read.csv(system.file("extdat",
      "rict-ni-taxa-prediction.csv",
      package = "rict"
    ), check.names = FALSE)

  taxa_prediction <- dplyr::arrange(taxa_prediction, Maitland_Name, Season_Code)
  verified_predictions <- dplyr::arrange(verified_predictions, Maitland_Name, Season_Code)
  taxa_prediction <- taxa_prediction[, names(taxa_prediction) %in% names(verified_predictions)]
  verified_predictions <- verified_predictions[, names(verified_predictions) %in% names(taxa_prediction)]

  expect_equal(round(taxa_prediction[11:18], 3), round(verified_predictions[11:18], 3))
})

test_that("predictions work NI", {
  prediction <- rict_predict(demo_ni_observed_values, crs = 29903)
  verified_predictions <-
    utils::read.csv(system.file("extdat",
      "rict-ni-predictions.csv",
      package = "rict"
    ), check.names = FALSE)


  prediction <- prediction[, names(prediction) %in% names(verified_predictions)]
  verified_predictions <- verified_predictions[, names(verified_predictions) %in% names(prediction)]
  # test predictions are the same for ASPT and NTAXA:
  expect_equal(prediction$TL2_WHPT_NTAXA_AbW_DistFam_sum, verified_predictions$TL2_WHPT_NTAXA_AbW_DistFam_sum)
  expect_equal(prediction$TL2_WHPT_ASPT_AbW_DistFam_sum, verified_predictions$TL2_WHPT_ASPT_AbW_DistFam_sum)
})

test_that("Isle of Man prediction", {

  input_predictions <- utils::read.csv(system.file("extdat",
                                                   "validation-classification-iom-predictions.csv",
                                                   package = "rict"
  ),
  check.names = FALSE, stringsAsFactors = FALSE
  )

  predictions <- rict_predict(input_predictions)
  expect_equal(round(input_predictions$`TL2 WHPT ASPT (AbW,DistFam)_E_1`, 1),
               round(predictions$TL2_WHPT_ASPT_AbW_DistFam_spr, 1))

  expect_equal(round(input_predictions$`TL2 WHPT ASPT (AbW,DistFam)_E_2`, 2),
               round(predictions$TL2_WHPT_ASPT_AbW_DistFam_sum, 2))

  expect_equal(round(input_predictions$`TL2 WHPT ASPT (AbW,DistFam)_E_3`, 2),
               round(predictions$TL2_WHPT_ASPT_AbW_DistFam_aut, 2))

})

test_that("Isle of Man suitcodes correct", {
  predictions <- rict_predict(demo_iom_observed_values)

  expect_equal(predictions$SuitCode, c("1", "1", "1", "1", "1", "1", "5", "2",
                                       "2", "2", "1", "1", "1", "1", "1", "1",
                                       "1", "1", "5", "2", "2", "2", "1", "1"))
  expect_equal(predictions$SuitText, c(">5%", ">5%", ">5%", ">5%", ">5%", ">5%",
                                       "<0.1%", "<5%", "<5%", "<5%", ">5%",
                                       ">5%", ">5%", ">5%", ">5%", ">5%", ">5%",
                                       ">5%", "<0.1%", "<5%", "<5%", "<5%",
                                       ">5%", ">5%"))
})

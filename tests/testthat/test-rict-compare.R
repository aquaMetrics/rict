test_that("rict_compare matches test data", {
  version <- paste0(R.version$major, ".", R.version$minor)
  if (version == "3.5.3" | version == "3.4.4") {
    version <- "3.4.4"
  }
  skip_if(version == "3.4.4",
    message =
      "Due to type.convert function not working on data.frames in
    R version <3.6 skip this test"
  )
  # Test dataset
  test_data <- utils::read.csv(
    system.file("extdat",
      "test-data-model-1.csv",
      package = "rict"
    ),
    check.names = FALSE
  )

  # Test dataset
  test_rict_compare <- utils::read.csv(
    system.file("extdat",
      "test-rict-compare.csv",
      package = "rict"
    ),
    check.names = FALSE
  )

  a <- rict(test_data[1:2, ], year_type = "single", store_eqrs = TRUE, seed = FALSE)
  b <- rict(test_data[13:14, ], year_type = "single", store_eqrs = TRUE, seed = FALSE)
  test <- rict_compare(a, b)

  test_rict_compare <-
    test_rict_compare[test_rict_compare$`Result A` %in% test$`Result A`, 2:45]

  test <- type.convert(test, as.is = TRUE)
  # remove summer values (weren't in original test dataset)
  test <- test[!seq_len(nrow(test)) %in%
                 grep("SUM", test$`EQR metric compared`), ]

  test_rict_compare <- type.convert(test_rict_compare, as.is = TRUE)

  expect_equal(
    round(
      sum(dplyr::select_if(test, is.numeric) - dplyr::select_if(test_rict_compare, is.numeric)
          , na.rm = TRUE), 3),
    -24.273 # -8.461
  )

  expect_equal(
    test$`Most Probable Class for Result A`,
    test_rict_compare$`Most Probable Class for Result A`
  )
})


test_that("Test missing seasons compare", {
  demo_observed_values <- rict::demo_observed_values[1, ]

  demo_observed_values$Spr_Season_ID <- NA
  demo_observed_values$Spr_Ntaxa_Bias <- NA
  demo_observed_values$`Spr_TL2_WHPT_ASPT (AbW,DistFam)` <- NA
  demo_observed_values$`Spr_TL2_WHPT_NTaxa (AbW,DistFam)` <- NA

  class <- rict(demo_observed_values, year_type = "single",
                store_eqrs = TRUE, seed = FALSE)

  demo_observed_values <- rict::demo_observed_values[1, ]
  demo_observed_values$Aut_Season_ID <- NA
  demo_observed_values$Aut_Ntaxa_Bias <- NA
  demo_observed_values$`Aut_TL2_WHPT_ASPT (AbW,DistFam)` <- NA
  demo_observed_values$`Aut_TL2_WHPT_NTaxa (AbW,DistFam)` <- NA

  class2 <- rict(demo_observed_values, year_type = "single",
                 store_eqrs = TRUE, seed = FALSE)

  test <- rict_compare(class, class2)
})


test_that("Test missing all seasons compare", {
  upstream <- utils::read.csv(
    system.file("extdat/compare-data",
                "rict-input-1-upstream-compare.csv",
                package = "rict"
    ),
    check.names = FALSE
  )

  downstream <- utils::read.csv(
    system.file("extdat/compare-data",
                "rict-input-2-downstream-compare.csv",
                package = "rict"
    ),
    check.names = FALSE
  )

  upstream <- rict(upstream,
                   store_eqrs = TRUE,
                   year_type = "single",
                   seed = FALSE
  )
  downstream <- rict(downstream,
                   store_eqrs = TRUE,
                   year_type = "single",
                   seed = FALSE
  )

compare_output <- rict_compare(
  results_a = upstream,
  results_b = downstream
  )


iom_fortran_input <- utils::read.csv(
  system.file("extdat",
              "input-file-to-test-iom-against-fortran-outputs.csv",
              package = "rict"
  ), check.names = FALSE)

upstream <- rict(iom_fortran_input[1:2, ],
                 store_eqrs = TRUE,
                 year_type = "single", seed = TRUE
)
downstream <- rict(iom_fortran_input[13:14, ],
                   store_eqrs = TRUE,
                   year_type = "single", seed = TRUE
)

compare_output <- rict_compare(
  results_a = upstream,
  results_b = downstream
)

upstream <- rict(iom_fortran_input[1:2, ],
                 store_eqrs = TRUE,
                 year_type = "multi", seed = TRUE
)
downstream <- rict(iom_fortran_input[13:14, ],
                   store_eqrs = TRUE,
                   year_type = "multi", seed = TRUE
)

compare_output <- rict_compare(
  results_a = upstream,
  results_b = downstream
)

})
test_that("multiplication works", {
 test <- rict(demo_observed_values[1:2, ])

 predict <-  rict_predict(demo_observed_values[1:2, ])
 data <-  rict_classify(predict)

 expect_equal(test, data)
})

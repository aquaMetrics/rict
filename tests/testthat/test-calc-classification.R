context("test calcClassifcation")


test_that("calcClassifcation outputs match azure single-year outputs", {

  observed_values <- utils::read.csv(system.file("extdat",
                                                 "validation-input-single-year.csv",
                                                 package = "rict"),check.names = F)


  predictions <- calcPrediction(observed_values = observed_values[1,])
  classification <- calcClassification(predictions, year_type = "single")
  expect_equal(class(classification), "data.frame")

  validation_classification <- utils::read.csv(system.file("extdat",
                                                           "validation-class-single-year.csv",
                                                           package = "rict"))

  classification$mintawhpt_spr_aut_mostProb <-
                                    as.character(classification$mintawhpt_spr_aut_mostProb)
  validation_classification$mintawhpt_spr_aut_mostProb <-
                                    as.character(validation_classification$mintawhpt_spr_aut_mostProb)

  equal <- all.equal(classification$mintawhpt_spr_aut_mostProb,
                     validation_classification$mintawhpt_spr_aut_mostProb[1])
  expect_true(equal == T)

})


test_that("calcClassifcation outputs match azure multi-year outputs", {
  predictions <- calcPrediction(observed_values = demo_observed_values)
  classification <- calcClassification(predictions)
  expect_equal(class(classification), "data.frame")

  # compare results downloaded from azure with package (Results on azure were manually user tested).
  validation_classification <- utils::read.csv(system.file("extdat",
                                                           "validation-class-multi-year.csv",
                                                           package = "rict"))
  classification <- classification[,names(classification) %in%  names(validation_classification)]

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
                                                         validation_classification$mintawhpt_spr_aut_mostProb_MINTA_)

  # remove row.names - not required for comparison
  row.names(classification) <- NULL
  row.names(validation_classification) <- NULL

  # test azure and package results match:
  equal <- all.equal(classification[, 1:23],
                       validation_classification[, 1:23])
  expect_true(equal == T)


})

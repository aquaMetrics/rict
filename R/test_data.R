
prediction <- function(data, predict_indices = NULL) {
  data <- utils::read.csv(system.file("extdat",
    "test-data.csv",
    package = "rict"
  ))

  # calc temp, lat, lon, logalt, logdist, logwith, logalk, log slope, mean_substrate - currently done in validation?
  data$temperature_range <- 15
  data$temperature_mean <- 10
  data$log_altitude <- log10(data$altitude)
  data$log_dist_from_source <- log10(data$dist_from_source)
  data$log_discharge <- log10(data$discharge)
  data$log_width <- log10(data$width)
  data$log_depth <- log10(data$depth)
  data$log_alkalinity <- log10(data$alkalinity)
  data$log_slope <- log10(data$slope)
  data$lat <- rict::osg_parse(grid_refs = paste0(data$ngr, data$northing, data$easting))[[1]]
  data$long <- rict::osg_parse(grid_refs = paste0(data$ngr, data$northing, data$easting))[[2]]
  data$mean_substrate <- rowMeans(data[, c("boulders_cobbles", "silt_clay", "pebbles_gravel")], na.rm = T)

  data$season_code <- as.numeric(format.Date(data$date_taken, "%m"))
  data$season_code[data$season_code %in% c(12, 1, 2)] <- 4
  data$season_code[data$season_code %in% c(3, 4, 5)] <- 1
  data$season_code[data$season_code %in% c(6, 7, 8)] <- 2
  data$season_code[data$season_code %in% c(9, 10, 11)] <- 3

  # These should be binary data files?
  mean_end_group <-
    utils::read.delim(
      system.file("extdat", "df-mean-gb-685.DAT", package = "rict"),
      header = FALSE,
      sep = "",
      as.is = TRUE
    )

  coefficents <-
    utils::read.delim(
      system.file("extdat", "df-coeff-gb-685.DAT", package = "rict"),
      header = FALSE,
      sep = "",
      as.is = TRUE
    )

  coefficents$predictors <- c(
    "lat",
    "long",
    "log_altitude",
    "log_dist_from_source",
    "log_width",
    "log_depth",
    "mean_substrate",
    "log_discharge",
    "alkalinity",
    "log_alkalinity",
    "log_slope",
    "temperature_mean",
    "temperature_range"
  )
  coefficents$V1 <- NULL
  # save coefficents as binary

  # Remove observed values - only need unique site predictor variables and
  # season to make predictions
  data <- select(data, -c(value, determinand)) %>% unique()

  # Select the predictors based on on the names in the coefficients data and
  # nest into a column called `predictors`
  data <- nest(data, predictors = c(coefficents$predictors))

  # Loop through each row of the `predictors` variable saving the product of
  # predictors * coefficients in `dfscore` variable

  data$df_scores <- map(data$predictors, function(predictors) {
    df_score <- data.frame(as.matrix(predictors) %*% as.matrix(select(coefficents, -predictors)))
  })

  # Calculate the Mahanalobis distance from each site to reference
  # groups means
  data$mah_dist <- map(data$df_scores, function(df_score) {
    mah_dist <- map_df(seq_len(mean_end_group), function(x) {
      data.frame(value = sum((df_score - mean_end_group[x, ])^2))
    })
    mah_dist <- as.data.frame(t(mah_dist))
  })

  # Find which reference group is has shortest Mahanalobis distance from each
  # site
  data$min_mah_dist <- map(data$mah_dist, `min`)

  # Calculate the probability distributions
  # nr_efg_groups should be binary file?
  nr_efg_groups <- utils::read.csv(system.file("extdat",
    "end-grp-assess-scores.csv",
    package = "rict"
  ))
  nr_efg_groups <- nr_efg_groups %>% select(-EndGrpNo)

  # Calculate probability distribution of belonging to each end group
  data$prob_distribution <- map(data$mah_dist, function(mah_dist) {
    endGrp_Score <- as.matrix(rowSums(nr_efg_groups)) %*% as.matrix(exp(-mah_dist / 2))
  })

  # Calculate total probability distribution of belonging to each end group
  data$prob_distribution_total <- map(data$prob_distribution, function(prob_distribution) {
    return(cbind(prob_distribution / rowSums(prob_distribution), rowSums(prob_distribution)))
  })

  # Load end group means
  end_group_index <- utils::read.csv(system.file("extdat", "x-103-end-group-means.csv", package = "rict"))
  end_group_index$season_code <- end_group_index$Season.Code
  # Get predicted end group mean scores based on season and metric
  data$end_group_index <- map(split(data, data$sample_id), function(site) {
    # nr_efg_groups should be binary file?

    end_group_index <- filter(end_group_index, season_code == site$season_code)
    end_group_index$season_code <- NULL
    return(end_group_index)
  })

  # Predict the indices by multiplying the end group score means by the probability
  # of end group membership
  data$prediction <- map(split(data, data$sample_id), function(site) {
    # Identify which indices to predict or predict all indices
    if (!is.null(predict_indices)) {
      predict_indices <- names(site$end_group_index[[1]])
    }
    prediction <- map_df(predict_indices, function(name) {
      return(as.data.frame(
        indice = name, prediction =
          as.matrix(site$prob_distribution_total) %*% as.matrix(site$end_group_index[, name])
      ))
    })
  })

  # Suitability codes
  suitability_codes <- data.frame(
    suit_code = c(1, 2, 3, 4, 5),
    suit_text = c(">5%", "<5%", "<2%", "<1%", "<0.1%")
  )
  chi_square_vals <- data.frame(
    chi_square_vals_1 = c(-Inf, 21.02606, 24.05393, 26.21696, 32.90923, Inf),
    chi_square_vals_2 = c(-Inf, 18.30700, 21.16080, 23.20930, 29.58830, Inf) # are these required?
  )
  data$suit_codes <- map(data$min_mah_dist, function(min_mah_dist) {

    suit_code <- as.numeric(cut(min_mah_dist, c(chi_square_vals$chi_square_vals_1)))
    return(suitability_codes[suit_code, ])
  })

  test <- data %>%
    select(-prob_distribution, -prob_distribution_total) %>%
    unnest()
  data <- unique(data)
}

## A statistical test to compare results ----------------------------------------------
  compare_test <-  function(results, eqrs, a, b) {
    # loop for each eqr type e.g. NTAXA, ASPT etc
    loop_eqr_types <- lapply(eqrs, function(eqr){
      # loop for each result(s) "a"
      loop_a <- lapply(a, function(a_result){
        # loop for each result(s) "b"
        loop_b <- lapply(b, function(b_result) {
          # check not comparing same result
          if(a_result == b_result) {
            return()
          }
          # calculate difference between a and b eqrs
          diff <- unlist(results[results$RESULT == b_result, eqr]) -
            unlist(results[results$RESULT == a_result, eqr])
          # make dataframe of useful test statistics
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
                                                       probs = c(2.5, 97.5) / 100)[[1]],
            "Upper 95% (U95) of Difference" = quantile(sort(diff),
                                                       probs = c(2.5, 97.5) / 100)[[2]],
            "2-sided test probability p of No difference in EQR" =
              2 * min(length(diff[diff >= 0])/ length(diff),
                      length(diff[diff >= 0])/ length(diff)),
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

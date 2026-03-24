library(readxl)
library(tidyverse)
library(dplyr)
library(matrixStats)

#defining aggregation function with the weight setting---------------
ces_aggregate <- function(values, weights, rho) {
  valid_idx <- !is.na(values)
  valid_values <- values[valid_idx]
  valid_weights <- weights[valid_idx]
  n <- length(valid_values)
  
  if(n == 0) return(NA)
  
  valid_weights <- valid_weights / sum(valid_weights)
  
  if(rho == -1) {
    exp(sum(valid_weights * valid_values))
  } else {
    (sum(valid_weights * valid_values^(-rho)))^(-1/rho)
  }
}

#defining SDG mapping function---------------
create_sdg_mapping <- function(column_names) {
  mapping_df <- data.frame(
    Indicator = column_names,
    SDG = NA_character_,
    stringsAsFactors = FALSE
  )
  for (i in seq_along(column_names)) {
    name <- column_names[i]
    
    if (grepl("^normalized(\\d{1,2})\\.\\d+$", name)) {
      sdg_num <- gsub("^normalized(\\d{1,2})\\..*$", "\\1", name)
      mapping_df$SDG[i] <- sdg_num
    }
  }
  return(mapping_df)
}

#defining Monte Carlo (MC) simulation functions at three levels---------------

## (1) Total index score using all indicators without organizing by SDGs
run_mc_simulation_without_SDGgroup <- function(data, n_simulations) {
  normalized_cols <- grep("^normalized", colnames(data), value = TRUE)
  indicator_data <- data[, normalized_cols, drop = FALSE]
  
  n_countries <- nrow(indicator_data)
  countries <- data$ISO_A3
  n_indicators <- ncol(indicator_data)
  
  mc_results <- matrix(NA, nrow = n_countries, ncol = n_simulations)
  rownames(mc_results) <- countries
  
  pb <- txtProgressBar(min = 0, max = n_simulations, style = 3)
  
  for (i in 1:n_simulations) {
    
    # generating random weights at the indicator level
    indicator_weights <- runif(n_indicators, 0, 1)
    
    # calculating the total score without organizing by SDGs
    for (j in 1:n_countries) {
      country_scores <- as.numeric(indicator_data[j, ])
      
      mc_results[j, i] <- ces_aggregate(country_scores, indicator_weights, rho = -1)
    }
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  return(mc_results)
}

## (2) Total index score using goals and indicators with varying weights
run_mc_simulation_double_random <- function(data, n_simulations ) {
  normalized_cols <- grep("^normalized", colnames(data), value = TRUE)
  indicator_data <- data[, normalized_cols, drop = FALSE]
  
  n_countries <- nrow(indicator_data)
  countries <- data$ISO_A3
  n_indicators <- ncol(indicator_data)
  
  sdg_mapping <- create_sdg_mapping(colnames(indicator_data))
  sdgs <- unique(sdg_mapping$SDG)
  n_sdgs <- length(sdgs)
  
  mc_results <- matrix(NA, nrow = n_countries, ncol = n_simulations)
  rownames(mc_results) <- countries
  
  pb <- txtProgressBar(min = 0, max = n_simulations, style = 3)
  
  for (i in 1:n_simulations) {
    # generating two sets of random weights
    # 1. random weights at the indicator level
    indicator_weights <- runif(n_indicators, 0, 1)
    
    # 2. random weights at the goal level
    sdg_weights <- runif(n_sdgs, 0, 1)
    names(sdg_weights) <- sdgs
    
    for (j in 1:n_countries) {
      country_scores <- as.numeric(indicator_data[j, ])
      
      # first calculating the individual SDG scores
      sdg_scores <- sapply(sdgs, function(sdg) {
        sdg_indicators <- which(sdg_mapping$SDG == sdg)
        sdg_values <- country_scores[sdg_indicators]
        sdg_w <- indicator_weights[sdg_indicators]
        ces_aggregate(sdg_values, sdg_w, rho = -1)
      })
      
      #  calculating the total score
      mc_results[j, i] <- ces_aggregate(sdg_scores, sdg_weights, rho = -1)
    }
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  return(list(results = mc_results, mapping = sdg_mapping))
}

## (3) Individual SDG score using indicators with varying weights
run_mc_simulation_per_sdg <- function(data, n_simulations ) {
  normalized_cols <- grep("^normalized", colnames(data), value = TRUE)
  indicator_data <- data[, normalized_cols, drop = FALSE]
  
  sdg_mapping <- create_sdg_mapping(colnames(indicator_data))
  sdgs <- unique(sdg_mapping$SDG)
  
  n_countries <- nrow(indicator_data)
  countries <- data$ISO_A3
  
  mc_results_sdg <- array(NA, dim = c(n_countries, length(sdgs), n_simulations),
                          dimnames = list(countries, sdgs, NULL))
  
  pb <- txtProgressBar(min = 0, max = n_simulations, style = 3)
  
  for (i in 1:n_simulations) {
    for (k in 1:length(sdgs)) {
      sdg <- sdgs[k]
      sdg_indicators <- which(sdg_mapping$SDG == sdg)
      
      sdg_weights <- runif(length(sdg_indicators), 0, 1)
      
      for (j in 1:n_countries) {
        sdg_values <- as.numeric(indicator_data[j, sdg_indicators])
        mc_results_sdg[j, k, i] <- ces_aggregate(sdg_values, sdg_weights, rho = -1)
      }
    }
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  return(list(results = mc_results_sdg, mapping = sdg_mapping))
}

#----------------------------------------defining result analyzing functions
##(1) Sensitivity of total index score
analyze_mc_results <- function( mc_results, original_scores, test_type) {
  n_countries <- nrow(mc_results)
  
  # Summarizing range of simulated scores from the lowest to the highest 5th percentile
  mc_summary <- data.frame(
    ISO_A3 = rownames(mc_results),
    Original_Score = original_scores,
    MC_Mean = rowMeans(mc_results, na.rm = TRUE),
    MC_P5 = apply(mc_results, 1, quantile, probs = 0.05, na.rm = TRUE),
    MC_P95 = apply(mc_results, 1, quantile, probs = 0.95, na.rm = TRUE)
  )

  world_avg <- mean(original_scores, na.rm = TRUE)
  
  # identifying the 'robust' country (whose range of scores consistently above/below the world average)
  mc_summary$Robust_Above <- apply(mc_results, 1, function(x) {
    if (all(is.na(x))) return(NA)
    mean(x > world_avg, na.rm = TRUE) > 0.95
  })
  
  mc_summary$Robust_Below <- apply(mc_results, 1, function(x) {
    if (all(is.na(x))) return(NA)
    mean(x < world_avg, na.rm = TRUE) > 0.95
  })
  mc_summary$Robust <- mc_summary$Robust_Above | mc_summary$Robust_Below

  titles <- list(
    "all_indicators" = "Total Index score using all indicators without organizing by SDGs",
    "varying_goal_weights" = "Total Index score using goals and indicators with varying weights"
  )
  
  title <- titles[[test_type]]
  
  p <- ggplot(mc_summary, aes(x = reorder(ISO_A3, Original_Score))) +
    geom_errorbar(aes(ymin = MC_P5, ymax = MC_P95), width = 0.2, alpha = 0.6) +
    geom_hline(yintercept = world_avg, linetype = "dashed", color = "blue") +
    geom_point(aes(y = Original_Score, shape = Robust, color = Robust), size = 1) +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "red")) +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) + 
    labs(
      title = paste( title),
      x = "Country/Region (ISO_A3)",
      y = "Score"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 4),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  print(p)
  
  avg_upper_change <- mean(mc_summary$MC_P95 - mc_summary$Original_Score, na.rm = TRUE)
  avg_lower_change <- mean(mc_summary$Original_Score - mc_summary$MC_P5, na.rm = TRUE)
  
  cat(paste(title, "Summary for MC simulation:\n"))
  cat("The global average may increase by: ", round(avg_upper_change, 3), "scores\n")
  cat("The global average may decrease by:  ", round(avg_lower_change, 3), "scores\n")
  cat("Number of countries consistently above the average: ", sum(mc_summary$Robust_Above, na.rm = TRUE), "\n")
  cat("Number of countries consistently below the average: ", sum(mc_summary$Robust_Below, na.rm = TRUE), "\n")
  
  return(mc_summary)
}

##(2) Sensitivity of individual goal score
analyze_sdg_results <- function(data, mc_results_sdg, selected_sdgs ) {
  sdgs <- dimnames(mc_results_sdg$results)[[2]]
  n_sdgs <- length(sdgs)
  countries <- data$ISO_A3
  selected_sdgs <- intersect(selected_sdgs, sdgs)
  
  sdg_summaries <- list()
  sdg_mapping <- mc_results_sdg$mapping
  normalized_cols <- grep("^normalized", colnames(data), value = TRUE)
  indicator_data <- data[, normalized_cols, drop = FALSE]
  
  for (sdg in selected_sdgs) {
    k <- which(sdgs == sdg)
    
    sdg_indicators <- which(sdg_mapping$SDG == sdg)
    
    original_scores <- rep(NA, length(countries))
    
    for (j in 1:length(countries)) {
      values <- as.numeric(indicator_data[j, sdg_indicators])
      if (length(values) > 0 && any(!is.na(values))) {
        weights <- rep(1/length(values), length(values))
        original_scores[j] <- ces_aggregate(values, weights, rho = -1)
      }
    }
    
    sdg_results <- mc_results_sdg$results[, k, ]
    
    sdg_summary <- data.frame(
      ISO_A3 = countries,
      Original_Score = original_scores,
      MC_Mean = apply(sdg_results, 1, mean, na.rm = TRUE),
      MC_P5 = apply(sdg_results, 1, quantile, probs = 0.05, na.rm = TRUE),
      MC_P95 = apply(sdg_results, 1, quantile, probs = 0.95, na.rm = TRUE)
    )
    
    valid_scores <- original_scores[!is.na(original_scores)]
    n_valid <- length(valid_scores)
    world_avg <- mean(valid_scores, na.rm = TRUE)
    
    sdg_summary$Robust_Above <- apply(sdg_results, 1, function(x) {
      if (all(is.na(x))) return(NA)
      mean(x > world_avg, na.rm = TRUE) > 0.95
    })
    
    sdg_summary$Robust_Below <- apply(sdg_results, 1, function(x) {
      if (all(is.na(x))) return(NA)
      mean(x < world_avg, na.rm = TRUE) > 0.95
    })
    
    sdg_summary$Robust <- sdg_summary$Robust_Above | sdg_summary$Robust_Below
    sdg_summaries[[sdg]] <- sdg_summary

    p <- ggplot(sdg_summary, aes(x = reorder(ISO_A3, Original_Score))) +
      geom_errorbar(aes(ymin = MC_P5, ymax = MC_P95), width = 0.2, alpha = 0.6) +
      geom_hline(yintercept = world_avg, linetype = "dashed", color = "blue") +
      geom_point(aes(y = Original_Score, shape = Robust, color = Robust), size = 1) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "red")) +
      scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1)) + 
      labs(
        title = paste("Scores for SDG", k,"using indicators with varying weights"),
        x = "Country/Region (ISO_A3)",
        y = "Score"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, size = 4),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
    
    print(p)

    valid_scores <- na.omit(original_scores)
    avg_upper_change <- mean(sdg_summary$MC_P95 - sdg_summary$Original_Score, na.rm = TRUE)
    avg_lower_change <- mean(sdg_summary$Original_Score - sdg_summary$MC_P5, na.rm = TRUE)
    
    cat(paste("\nSDG", k, "Summary for MC simulation:\n"))
    cat("· Number of valid countries:", n_valid, "\n")
    cat("· Global average Wetland-SDG score:", round(world_avg, 3), "\n")
    cat("The global average may increase by:", round(avg_upper_change, 3), "scores\n")
    cat("The global average may decrease by: ", round(avg_lower_change, 3), "scores\n")
    cat("Number of countries consistently above the average:", sum(sdg_summary$Robust_Above, na.rm = TRUE), "\n")
    cat("Number of countries consistently below the average:", sum(sdg_summary$Robust_Below, na.rm = TRUE), "\n")
  }
  
  return(sdg_summaries)
}

#performing MC simulation-----------------------------

SDG_data_panel<- read_xlsx('/.../SDG.xlsx',sheet = 1) 
normalized_data_panel <- read_xlsx('/.../SDG.xlsx',sheet = 3) 

# calculating the original total score
sdg_cols <- grep("SDG\\d+", colnames(SDG_data_panel), value = TRUE)

original_score <- apply(SDG_data_panel[, sdg_cols], 1, function(x) {
  ces_aggregate(x, rep(1/length(x), length(x)), rho = -1)
})

set.seed(42) 

## running MC simulations at three levels
### (1) Total index score using all indicators without organizing by SDGs
mc_results_without_SDGgroup <- run_mc_simulation_without_SDGgroup(normalized_data_panel, n_simulations = 1000)
mc_summary_without_SDGgroup <- analyze_mc_results( mc_results_without_SDGgroup, 
                                                   original_score, test_type="all_indicators")
ggsave("/.../Sensitivity_weights_type1.jpeg", 
       width = 10, height = 5, dpi = 600)

### (2) Total index score using goals and indicators with varying weights
mc_results_double_random <- run_mc_simulation_double_random(normalized_data_panel, n_simulations = 1000)
mc_summary_double_random <- analyze_mc_results(mc_results_double_random$results,
                                               original_score,  test_type="varying_goal_weights")
ggsave("/.../Sensitivity_weights_type2.jpeg", 
       width = 10, height = 5, dpi = 600)

### (3) Individual SDG score using indicators with varying weights
mc_results_per_sdg <- run_mc_simulation_per_sdg(normalized_data_panel, n_simulations = 1000)
sdg_summaries <- analyze_sdg_results(normalized_data_panel, mc_results_per_sdg,
                                     selected_sdgs = "17")
ggsave("/.../Sensitivity_weights_SDG17.jpeg", 
       width = 10, height = 5, dpi = 600)

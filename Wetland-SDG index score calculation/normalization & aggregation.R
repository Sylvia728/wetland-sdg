library(readxl)
library(purrr)
library(stringr)
#---defining normalization function (upper bound: 2.5 percentile; lower bound: 2.5 percentile)
normalize <- function(df, indicator_col, new_col_name = "normalized",
                       low_percentile = 0.025, high_percentile = 0.975,
                       symmetric) {

  df_out <- as.data.frame(df)
  
  df_out[ , indicator_col][df_out[ , indicator_col] == 0 | is.na(df_out[ , indicator_col])] <- NA
  
  valid_data <- na.omit(df_out[ , indicator_col])
  
  # checking if there is sufficient data for normalization
  if (length(valid_data) < 2) {
    warning("Insufficient data for normalization (less than 2 valid values).")
    df_out[ , new_col_name] <- NA
    return(df_out)
  }
  
  low_val <- quantile(valid_data, probs = low_percentile, na.rm = TRUE, names = FALSE)
  high_val <- quantile(valid_data, probs = high_percentile, na.rm = TRUE, names = FALSE)
  
  
  if (high_val - low_val == 0) {
    df_out[ , new_col_name] <- 0
    warning("Setting normalized values to 0.")
    return(df_out)
  }
  
  x <- df_out[[indicator_col]]
  
  normalized_vals <- rep(NA, length(x))
  
  # defining different normalization rules
  if (symmetric) {
    # indicator values with negative values are normalized to [-1, 1]
    normalized_vals <- ifelse(is.na(x), NA,
                              ifelse(x <= low_val, -1,
                                     ifelse(x >= high_val, 1,
                                            2 * ( (x - low_val) / (high_val - low_val) ) - 1
                                     )
                              )
    )
  } else {
    # indicator values without negative values are normalized to [0, 1]
    normalized_vals <- ifelse(is.na(x), NA,
                              ifelse(x <= low_val, 0,
                                     ifelse(x >= high_val, 1,
                                            (x - low_val) / (high_val - low_val)
                                     )
                              )
    )
  }
  
  df_out[[new_col_name]] <- normalized_vals
  return(df_out)
}


#---------------defining aggregation function (i.e.,constant-elasticity-of-substitution (CES) function)
ces_aggregate <- function(values, rho) {

  valid_values <- values[!is.na(values)]
  n <- length(valid_values)
  
  if(n == 0) {
    return(NA)
  }
  
  if(rho == 1) {
    return(prod(valid_values)^(1/n))
  } else if(rho == -1) {
    return(mean(valid_values))
  } else {
    return((1/n * sum(valid_values^(-rho)))^(-1/rho))
  }
}

#---------------performing normalization and score calculation for each SDG implementation
sheets <- 1:3

in_list <- map(sheets, ~ read_excel("/.../SDG1.xlsx", sheet = .x))

norm_list <- map2(in_list, sheets, ~ {
  normalize(
    df = .x,
    indicator_col = paste0("indicator1.", .y),
    new_col_name = paste0("normalized1.", .y),
    symmetric = TRUE # with negative values or not
  )
})

norm_cols <- map2(norm_list, sheets, ~ .x[[paste0("normalized1.", .y)]])

SDG1 <- data.frame(
  SDG_region = norm_list[[1]]$SDG_region,
  ISO_A3 = norm_list[[1]]$ISO_A3
)

for (i in seq_along(sheets)) {
  col_name <- paste0("normalized1.", sheets[i])
  SDG1[[col_name]] <- norm_cols[[i]]
}


indicator_cols <- paste0("normalized1.", 1:3)

SDG1$aggregated_score <- apply(SDG1[, indicator_cols], 1, function(x) {
  ces_aggregate(x, rho = -1) 
})

write.csv(SDG1, "/.../00result/SDG1.csv", row.names = FALSE)

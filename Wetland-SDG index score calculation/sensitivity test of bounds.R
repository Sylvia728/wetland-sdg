library(readxl)
library(purrr)
library(dplyr)
library(writexl)
library(viridis)
library(tidyverse)
#defining normalization function (upper bound: average of the top five performers; lower bound: average of the bpttom five performer)----
normalize_ave <- function(df, indicator_col, new_col_name = "normalized", 
                       bottom_n = 5, top_n = 5,
                       symmetric) {
  
  df_out <- as.data.frame(df)
  
  df_out[ , indicator_col][df_out[ , indicator_col] == 0 | is.na(df_out[ , indicator_col])] <- NA
  
  valid_data <- na.omit(df_out[ , indicator_col])
  
  if (length(valid_data) < 2) {
    warning("Insufficient data for normalization (less than 2 valid values).")
    df_out[ , new_col_name] <- NA
    return(df_out)
  }
  
  if (length(valid_data) < bottom_n) {
    bottom_mean <- min(valid_data)
    warning(paste("Only", length(valid_data), "valid values. Using minimum value as bottom_mean."))
  } else {
    bottom_vals <- head(sort(valid_data, decreasing = FALSE), n = bottom_n)
    bottom_mean <- mean(bottom_vals)
  }
  
  if (length(valid_data) < top_n) {
    top_mean <- max(valid_data)
    warning(paste("Only", length(valid_data), "valid values. Using maximum value as top_mean."))
  } else {
    top_vals <- head(sort(valid_data, decreasing = TRUE), n = top_n)
    top_mean <- mean(top_vals)
  }
  
  if (top_mean - bottom_mean == 0) {
    df_out[ , new_col_name] <- 0
    warning("Setting normalized values to 0.")
    return(df_out)
  }
  
  x <- df_out[ , indicator_col]
  
  normalized_vals <- numeric(length(x))
  
  if (symmetric) {
    normalized_vals <- ifelse(is.na(x), NA,
                              ifelse(x <= bottom_mean, -1,
                                     ifelse(x >= top_mean, 1,
                                            2 * ( (x - bottom_mean) / (top_mean - bottom_mean) ) - 1
                                     )
                              )
    )
  } else {
    normalized_vals <- ifelse(is.na(x), NA,
                              ifelse(x <= bottom_mean, 0,
                                     ifelse(x >= top_mean, 1,
                                            (x - bottom_mean) / (top_mean - bottom_mean)
                                     )
                              )
    )
  }
  
  df_out[ , new_col_name] <- normalized_vals
  return(df_out)
}

#defining normalization function (upper bound: mean + 2*MAD; lower bound: mean - 2*MAD)-----
normalize_mad <- function(df, indicator_col, new_col_name = "normalized", 
                          symmetric, mad_multiplier = 2) {
  
  df_out <- as.data.frame(df)

  df_out[ , indicator_col][df_out[ , indicator_col] == 0 | is.na(df_out[ , indicator_col])] <- NA
  
  valid_data <- na.omit(df_out[ , indicator_col])
  
  if (length(valid_data) < 2) {
    warning("Insufficient data for normalization (less than 2 valid values).")
    df_out[ , new_col_name] <- NA
    return(df_out)
  }
  
  mean_val <- mean(valid_data, na.rm = TRUE)
  mad_val <- mean(abs(valid_data - mean_val), na.rm = TRUE)
  
  bottom <- mean_val - mad_multiplier * mad_val
  top <- mean_val + mad_multiplier * mad_val
  
  if (top - bottom == 0) {
    df_out[ , new_col_name] <- 0
    warning("Setting normalized values to 0.")
    return(df_out)
  }
  
  x <- df_out[ , indicator_col]
  normalized_vals <- numeric(length(x))
  
  if (symmetric) {
    normalized_vals <- ifelse(is.na(x), NA,
                              ifelse(x <= bottom, -1,
                                     ifelse(x >= top, 1,
                                            2 * ( (x - bottom) / (top - bottom) ) - 1
                                     )
                              )
    )
  } else {
    normalized_vals <- ifelse(is.na(x), NA,
                              ifelse(x <= bottom, 0,
                                     ifelse(x >= top, 1,
                                            (x - bottom) / (top - bottom)
                                     )
                              )
    )
  }
  
  df_out[ , new_col_name] <- normalized_vals
  return(df_out)
}

#performing normalization for each SDG implementation---------------
sheets <- 1:4

in_list <- map(sheets, ~ read_excel("/.../SDG17.xlsx", sheet = .x))

##defining implementation function
perform_normalization <- function(in_list, sheets, normalizer, col_prefix, col_suffix = "normalized17") {

  norm_list <- map2(in_list, sheets, ~ {
    indicator_col = paste0("indicator17.", .y)
    indicator_data <- .x[[indicator_col]]
    
    has_negative <- any(indicator_data < 0, na.rm = TRUE)
    
    normalizer(
      df = .x,
      indicator_col = indicator_col,
      new_col_name = paste0(col_prefix, "_", col_suffix, ".", .y),
      symmetric = has_negative
    )
  })
  
  norm_cols <- map2(norm_list, sheets, ~ .x[[paste0(col_prefix, "_", col_suffix, ".", .y)]])
  
  result_df <- data.frame(
    SDG_region = norm_list[[1]]$SDG_region,
    ISO_A3 = norm_list[[1]]$ISO_A3
  )
  
  for (i in seq_along(sheets)) {
    col_name <- paste0(col_prefix, "_", col_suffix, ".", sheets[i])
    result_df[[col_name]] <- norm_cols[[i]]
  }
  
  return(result_df)
}

##applying implementation function with different normalizers
ave_SDG17 <- perform_normalization(
  in_list = in_list,
  sheets = sheets,
  normalizer = normalize_ave,
  col_prefix = "ave"
)

mad_SDG17 <- perform_normalization(
  in_list = in_list,
  sheets = sheets,
  normalizer = normalize_mad,
  col_prefix = "mad"
)

combine_data <- function(prefix) {
  combined <- NULL
  for(i in 1:17) {
    df_name <- paste0(prefix, "_SDG", i)
    
    if(exists(df_name)) {
      temp_df <- get(df_name)
      norm_col <- grep("normalized", colnames(temp_df), value = TRUE)
      
      if(length(norm_col) > 0) {
        temp_df <- temp_df[, c("SDG_region", "ISO_A3", norm_col)]
        
        if(is.null(combined)) {
          combined <- temp_df
        } else {
          temp_df_merge <- temp_df[, c("ISO_A3", norm_col)]
          combined <- merge(combined, temp_df_merge, by = c("ISO_A3"), all = TRUE)
        }
      }
    }
  }
  return(combined)
}

ave_combined <- combine_data("ave")
mad_combined <- combine_data("mad")

#performing aggregation for results with different bounds---------------------------
aggregate_by_sdg <- function(data) {
  
  sdg_columns <- grep("normalized", colnames(data), value = TRUE)
  
  sdg_map <- sdg_columns %>%
    str_extract("normalized(\\d+)\\.\\d+") %>%
    str_remove("normalized") %>%
    str_remove("\\.\\d+") %>%
    unique()
  
  sdg_aggregated <- map_dfc(sdg_map, function(sdg_num) {
    pattern <- paste0("normalized", sdg_num, "\\.\\d+")
    sdg_cols <- grep(pattern, colnames(data), value = TRUE)
    
    data %>%
      rowwise() %>%
      mutate(!!paste0("SDG", sdg_num) := ces_aggregate(
        c_across(all_of(sdg_cols)), 
        rho = -1
      )) %>%
      ungroup() %>%
      select(!!paste0("SDG", sdg_num))
  })
  
  result <- bind_cols(
    data %>% select(SDG_region, ISO_A3),
    sdg_aggregated
  )
  
  score_cols <- names(result)[grep("^SDG\\d+$", names(result))]
  
  result <- result %>%
    rowwise() %>%
    mutate(Total_Score = ces_aggregate(
      c_across(all_of(score_cols)), 
      rho = -1
    )) %>%
    ungroup()
  
  
  return(result)
}

ave_aggregated<-aggregate_by_sdg(ave_combined)
write_xlsx(
  list(
    normalized = ave_combined,
    aggregated = ave_aggregated
  ),
  path = "/.../ave_results.xlsx"
)

mad_aggregated<-aggregate_by_sdg(mad_combined)
write_xlsx(
  list(
    normalized = mad_combined,
    aggregated = mad_aggregated
  ),
  path = "/.../mad_results.xlsx"
)

#testing the sensitivity of ranks to different bounds----------------
base<-read_xlsx('/../SDG.xlsx',sheet = 1)

methods <- c("base", "ave", "mad")
data_list <- list(base, ave_aggregated, mad_aggregated)
names(data_list) <- methods

for (method in methods) {
  df <- data_list[[method]]
  df <-  df %>% filter(!is.na(Total_Score))
  
  sdg_cols <- grep("SDG\\d+", names(df), value = TRUE)
  
  df$n_valid_sdg <- rowSums(!is.na(df[, sdg_cols]))
  
  # Only ranking for countries with at least 4 valid SDG scores
  df <- df %>% filter(n_valid_sdg >= 4)

  df$Rank <- rank(-df$Total_Score, ties.method = "min")
  
  df$Method <- method
  
  data_list[[method]] <- df
}

all_data <- bind_rows(data_list) %>%
  select(ISO_A3, SDG_region, Method, Total_Score, Rank)

##calculating metrics to testify the consistency of ranks
### Kendall's coefficient of concordance
kendall_w <- all_data %>%
  select(ISO_A3, Method, Rank) %>%
  pivot_wider(names_from = Method, values_from = Rank) %>%
  column_to_rownames("ISO_A3") %>%
  as.matrix() %>%
  irr::kendall()

cat("Kendall's coefficient of concordance (Kendall's W):", kendall_w$value, "\n")
cat("p value for Chi-square test :", kendall_w$p.value, "\n")

### Spearman's Rank correlation coefficient matrix
spearman_cor <- all_data %>%
  select(ISO_A3, Method, Rank) %>%
  pivot_wider(names_from = Method, values_from = Rank) %>%
  select(-ISO_A3) %>%
  cor(method = "spearman", use = "complete.obs")

cat("Spearman's Rank correlation coefficient matrix:")
print(spearman_cor)

### visualizing the rank changes
base_isos <- unique(all_data$ISO_A3[all_data$Method == "base"])

all_data_filtered <- all_data %>%
  filter(ISO_A3 %in% base_isos)

all_data_filtered$Method <- factor(all_data_filtered$Method, levels = methods)

label_data <- all_data_filtered %>%
  filter(Method == "base") %>%
  mutate(
    label_x = -0.2, 
    label_y = Rank
  )

ggplot(all_data_filtered, aes(x = Method, y = Rank, group = ISO_A3, color = SDG_region)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 3) +
  geom_text(
    data = label_data,
    aes(x = label_x, y = label_y, label = ISO_A3),
    direction = "y",  
    hjust = 1,       
    size = 1,       
    segment.size = 0.3, 
    segment.color = "grey50",
    box.padding = 0.3, 
    min.segment.length = 0, 
    nudge_x = 1,    
    color = "black"    
  ) +
  scale_color_manual(
    values = c(
      "Australia and New Zealand" = "#00847e",
      "Central and Southern Asia" = "#578145",
      "Eastern and South-Eastern Asia" = "#b16214",
      "Europe and Northern America" = "#4c6a9c",
      "Latin America and the Caribbean" = "#883039",
      "Northern Africa and Western Asia" ="#bc8e5a",
      "Oceania" ="#38aaba",
      "Sub-Saharan Africa" = "#8c4569"
    )
  ) +
  scale_y_reverse(
    breaks = scales::pretty_breaks(n = 10),
    name = "Rank of countries/regions by Wet-SDG score "
  ) +
  scale_x_discrete(
    name = "Selection of bounds",
    labels = c("base" = "Base (2.5th percentile)",
               "ave" = "Average of max/min five", 
               "mad" = "mean +/- 2*MAD")
  ) +
  labs(
    title = "Rank changes"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 8, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 8, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8),
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 40, 20, 20)
  ) 

ggsave("/.../Sensitivity_bounds_ranking_changes.jpeg", 
       width = 8, height = 10, dpi = 600)

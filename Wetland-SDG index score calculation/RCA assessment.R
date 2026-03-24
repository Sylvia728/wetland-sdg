library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales) 
library(fmsb)
library(png)
library(grid)
library(vegan)
#defining base function for revealed comparative advantage (RCA) calculation-----
calculate_rca <- function(data) {

  sdg_cols <- names(data)
  
  rca_results <- data.frame(
    ISO_A3 = rownames(data),
    stringsAsFactors = FALSE
  )
  
  total_score_all <- sum(data, na.rm = TRUE)
  country_totals <- rowSums(data, na.rm = TRUE)
  goal_totals <- colSums(data, na.rm = TRUE)
  
  for(i in 1:length(sdg_cols)) {
    goal <- sdg_cols[i]
    
    valid_countries <- country_totals != 0  & !is.na(country_totals)

    rca_prime <- rep(NA, nrow(data))
    
    if(goal_totals[i] > 0) {
      rca_prime[valid_countries] <- (data[valid_countries,i] / country_totals[valid_countries]) / 
        (goal_totals[i] / total_score_all)
    }
    
    rca_results[, goal] <- rca_prime - 1
  }
  return(rca_results)
}

#defining function for calculating combined RCA with negative advantages--------
calculate_rca_bidirection <- function(data) {
  sdg_cols <- names(data)
  
  #calculating RCA separately for positive and negative impacts
  positive_impact <- data
  negative_impact <- data
  
  positive_impact[positive_impact < 0] <- NA
  
  negative_impact[negative_impact > 0] <- NA
  ##using the absolute value of negative impacts to quantify the degree of obstacle
  negative_impact <- abs(negative_impact)
  
  rca_positive <- calculate_rca(positive_impact)
  rca_negative <- calculate_rca(negative_impact)
  
  rca_combined <- data.frame(ISO_A3 = rownames(data))
  
  #calculating combined RCA as：rca_positive - rca_negative
  for(i in 1:length(sdg_cols)) {
    indicator <- sdg_cols[i]
    
    pos_values <- rca_positive[, indicator]
    neg_values <- rca_negative[, indicator]
    
    pos_values_fixed <- ifelse(is.na(pos_values), 0, pos_values)
    neg_values_fixed <- ifelse(is.na(neg_values), 0, neg_values)
    

    combined_values <- pos_values_fixed - neg_values_fixed
    combined_values[is.na(pos_values) & is.na(neg_values)] <- NA
    rca_combined[, indicator] <- combined_values
  }
  
  return(list(
    rca_positive = rca_positive,
    rca_negative = rca_negative,
    rca_combined = rca_combined
  ))
}

#performing RCA calculation-----------------------------------------
SDG_data_panel <- read_xlsx('/.../SDG.xlsx', sheet = 1) 

sdg_cols <- grep("SDG\\d+", colnames(SDG_data_panel), value = TRUE)

sdg_data <- SDG_data_panel %>%
  column_to_rownames("ISO_A3") %>%
  select(all_of(sdg_cols))
  
rca_result <- calculate_rca_bidirection(sdg_data)

rca_with_income<-rca_result$rca_combined %>%
  left_join(SDG_data_panel %>%
              select(Income_Group,ISO_A3), by = "ISO_A3")

write.csv(rca_with_income,"D:/Users/Sylvia/Desktop/wetlandSDGs/resutls/section2/cluster/RCA.csv")

#plotting-----------------------------------------
plot_data <-  rca_with_income %>%
  pivot_longer(
    cols = starts_with("SDG"), 
    names_to = "SDG", 
    values_to = "RCA"
  ) %>%
  mutate(
    SDG = factor(SDG, levels = paste0("SDG", 1:17)),
  )

clusters_ig <- unique(na.omit(plot_data$Income_Group))

##----------------------------heat maps
plots <- list()

common_fill_scale <- scale_fill_gradientn(
  colours = c("#1E3F66", "#386295","#60A8FF","#94D6FF", "#F0F0F0","#FFAA80","#FF8282", "#CA0020"),
  values = scales::rescale(c(-53.5, -25, -5,-2,0,2,5, 30.5)),
  na.value = "grey90",
  name = "RCA",
  breaks =c(-53.5, -25, -5,-2,0,2,5, 30.5),
  labels = c("-53.5", "-25","-5","-2", "0","2", "5","30.5"),
  limits = c(-53.5, 30.5),
  guide = guide_colourbar(
    direction = "vertical",  
    barheight = unit(6, "cm"),
    barwidth = unit(0.2, "cm"),
    title.position = "top"
  )
)

clusters_ig <-  c("L","LM","UM","H")

for (clust in clusters_ig) {
  cluster_data <- plot_data %>%
    filter(Income_Group == clust)
  
  #summarizing the average RCA of 17 SDGs for ordering countries
  country_order <- cluster_data %>%
    group_by(ISO_A3) %>%
    summarise(avg_rca = mean(RCA, na.rm = TRUE)) %>%
    arrange(avg_rca) %>%
    pull(ISO_A3)
  
  cluster_data$ISO_A3 <- factor(cluster_data$ISO_A3, levels = country_order)
  
  p <- ggplot(cluster_data, aes(x = SDG, y = ISO_A3, fill = RCA)) +
    geom_tile() +
    common_fill_scale +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    ) +
    ggtitle(paste("RCA of", clust))
  
  plots[[as.character(clust)]] <- p
}

ggarrange(
  plotlist = plots,
  nrow = 1, 
  ncol = 4,
  common.legend = TRUE,
  legend = "right" 
)

ggsave("/.../RCA_by income group.jpg",  width = 16, height = 8, dpi = 600)

par(mfrow = c(1, 1))

##----------------------------radar plots
radar_data <- plot_data %>%
  group_by(Income_Group, SDG) %>%
  #summarizing the average RCA by income groups for visualization
  summarise(mean_RCA = mean(RCA, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = SDG,
    values_from = mean_RCA
  )

radar_data_scaled <- radar_data %>%
  mutate(across(-Income_Group, ~ rescale(.x, to = c(-1, 1))))

custom_colors <- c(
  "NA" ="#999999", 
  "H" = "#274753",  
  "UM" = "#299d8f",
  "LM"= "#e7c66b",
  "L"= "#ea5b34"
)

sdg_icons_path <- "/.../E SDG Icons WEB/" 

par(mfrow = c(1, 4), mar = c(0.5, 0.5, 2, 0.5))

for (ig in clusters_ig) {
  ig_data <- radar_data_scaled %>% 
    filter(Income_Group == ig) %>%
    select(-Income_Group)
  
  feature_columns <- setdiff(names(ig_data), c("Income_Group"))
  
  radar_df <- data.frame(rbind(
    rep(1, length(feature_columns)),  
    rep(-1, length(feature_columns)),
    as.matrix(ig_data[, feature_columns])
  ))
  
  cluster_color <- custom_colors[ig]
  
  radarchart(
    radar_df,
    pcol = cluster_color,    
    pfcol = scales::alpha(cluster_color, 0.5),
    plwd = 2,
    plty = 1,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey",
    vlcex = 0,
    vlabels = rep("", ncol(radar_df)),
    title = paste(ig)
  )

  n_vars <- ncol(radar_df)
  angles <- seq(0, 2 * pi, length.out = n_vars + 1)[1:n_vars]
  
  icon_radius <- 1.15
  
  for (i in 1:n_vars) {
    x_pos <- icon_radius * sin(-angles[i])
    y_pos <- icon_radius * cos(-angles[i])
    
    sdg_number <- sprintf("%02d", i)
    icon_file <- paste0("E-WEB-Goal-", sdg_number, ".png")
    icon_path <- file.path(sdg_icons_path, icon_file)
    
    if (file.exists(icon_path)) {
      sdg_icon <- readPNG(icon_path, native = TRUE)
      
      icon_dim <- dim(sdg_icon)
      icon_width <- icon_dim[2] / 5000
      icon_height <- icon_dim[1] / 5000
      
      rasterImage(sdg_icon,
                  x_pos - icon_width/2, y_pos - icon_height/2,
                  x_pos + icon_width/2, y_pos + icon_height/2)
    }
  }
}

par(mfrow = c(1, 1))
ggsave("/.../radar_RCA by income groups.jpg",  width = 16, height = 5, dpi = 600)

#heterogeneity tests----------------------------------------
##Kruskal-Wallis test by income group for each SDG
kw_results <- data.frame(
  SDG = sdg_cols,
  p_income = sapply(sdg_cols, function(col) {
    
    data_sub <- rca_with_income %>%
      select(all_of(col), Income_Group) %>%
      na.omit()
    
    if (n_distinct(data_sub$Income_Group) > 1) {
      kruskal.test(
        as.formula(paste(col, "~ Income_Group")),
        data = data_sub
      )$p.value
    } else {
      NA_real_
    }
  }),
  row.names = NULL
)

kw_results$p_income_adj <- p.adjust(kw_results$p_income, method = "BH")

print(kw_results)

##PERMANOVA test for the general heterogeneity across groups
sdg_matrix <- rca_with_income %>% select(starts_with("SDG"))

dist_sdg <- vegdist(
  sdg_matrix,
  method = "gower",
  na.rm = TRUE
)

complete_rows <- !is.na(rca_with_income$Income_Group)
dist_sdg_sub <- as.dist(as.matrix(dist_sdg)[complete_rows, complete_rows])

pa_results <- adonis2(
  dist_sdg_sub ~ Income_Group,
  data = rca_with_income[complete_rows, ],
  permutations = 1000
)

print(pa_results)

##pairwise PERMANOVA test by groups
pairwise_adonis <- function(dist_mat, group, nperm = 1000) {
  
  group <- factor(group)
  lvls <- levels(group)
  combs <- combn(lvls, 2, simplify = FALSE)
  
  results <- lapply(combs, function(x) {
    
    idx <- group %in% x
    sub_dist <- as.dist(as.matrix(dist_mat)[idx, idx])
    sub_group <- droplevels(group[idx])
    
    ad <- adonis2(
      sub_dist ~ sub_group,
      permutations = nperm
    )
    
    data.frame(
      group1 = x[1],
      group2 = x[2],
      F = ad$F[1],
      R2 = ad$R2[1],
      p = ad$`Pr(>F)`[1]
    )
  })
  
  res <- do.call(rbind, results)
  res$p_adj <- p.adjust(res$p, method = "BH")
  res
}

pairwise_results <- pairwise_adonis(
  dist_sdg,
  rca_with_income$Income_Group,
  nperm = 1000
)

print(pairwise_results)

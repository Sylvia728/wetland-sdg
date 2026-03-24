library(readxl)
library(tidyverse)
library(corrplot)
library(igraph)
library(ggraph)
library(ggforce)
library(png)
library(grid)
library(ggimage)
library(writexl)
# preparing data-----------------------------
sdg_colors <- c(
  "1" ="#E5243B", "2" ="#DDA63A", "3" ="#4C9F38",  "4"="#C5192D", "5"="#FF3A21",
  "6"= "#26BDE2", "7"="#FCC30B", "8"="#A21942","9"= "#FD6925","10"= "#DD1367",
  "11"=  "#FD9D24","12"= "#BF8B2E","13"= "#3F7E44","14"= "#0A97D9", "15"="#56C02B",
  "16"=  "#00689D", "17"="#19486A")

sdg_icons_path <- "/.../E SDG Icons WEB/"

SDG_data_panel<- read_xlsx('/.../SDG.xlsx',sheet = 1)
sdg_cols <- grep("SDG\\d+", colnames(SDG_data_panel), value = TRUE)

normalized_data_panel <- read_xlsx('/.../SDG.xlsx',sheet = 3)

processed_data <- SDG_data_panel %>%
  column_to_rownames("ISO_A3") %>%
  select(all_of(sdg_cols)) %>%
  filter(Income_Group=='L')

processed_data <- normalized_data_panel %>%
  column_to_rownames("ISO_A3") %>%
  select(starts_with("normalized", ignore.case = FALSE))

# calculating Spearman correlation coefficient matrix----------
## 
safe_cor_test <- function(x, y) {
  valid <- !is.na(x) & !is.na(y)
  if (sum(valid) < 1) return(list(estimate = NA, p.value = NA))
  tryCatch(
    cor.test(x, y, method = "spearman", use = "pairwise.complete.obs"),
    error = function(e) list(estimate = NA, p.value = NA)
  )
}

n <- ncol(processed_data)
cor_matrix <- matrix(NA, n, n)
p_mat <- matrix(NA, n, n)
colnames(cor_matrix) <- colnames(p_mat) <- names(processed_data)
rownames(cor_matrix) <- rownames(p_mat) <- names(processed_data)

for (i in seq_len(n)) {
  for (j in seq_len(n)) {
    if (i > j) {
      res <- safe_cor_test(processed_data[[i]], processed_data[[j]])
      cor_matrix[i, j] <- res$estimate
      p_mat[i, j] <- res$p.value
    }
  }
}

cor_matrix[upper.tri(cor_matrix)] <- t(cor_matrix)[upper.tri(cor_matrix)]
p_mat[upper.tri(p_mat)] <- t(p_mat)[upper.tri(p_mat)]

## adjusting p-value with BH method
vals_to_adjust <- p_mat[upper.tri(p_mat) & !is.na(p_mat)]
p_adj <- p.adjust(vals_to_adjust, method = "BH")
p_mat_adj <- matrix(NA, nrow = ncol(p_mat), ncol = ncol(p_mat),
                    dimnames = dimnames(p_mat))
U_inds <- which(upper.tri(p_mat) & !is.na(p_mat), arr.ind = TRUE)
for (k in seq_len(nrow(U_inds))) {
  i <- U_inds[k, 1]
  j <- U_inds[k, 2]
  p_mat_adj[i, j] <- p_adj[k]
  p_mat_adj[j, i] <- p_adj[k]
}

## summarizing for correlations 
total_pairs <- sum(upper.tri(cor_matrix) & !is.na(cor_matrix))
cat("Total pairs:", total_pairs, "\n")

positive_pairs <- sum(cor_matrix > 0, na.rm = TRUE) / 2 
negative_pairs <- sum(cor_matrix < 0, na.rm = TRUE) / 2
cat("Positively correlated pairs:", positive_pairs, "\n")
cat("Negatively correlated pairs:", negative_pairs, "\n")

upper_cor <- cor_matrix[upper.tri(cor_matrix)]
upper_p <- p_mat_adj[upper.tri(p_mat_adj)]
sig_positive_pairs <- sum(upper_cor > 0 & upper_p < 0.05, na.rm = TRUE)
sig_negative_pairs <- sum(upper_cor < 0 & upper_p < 0.05, na.rm = TRUE)
cat("Significantly positive-correlated pairs (p < 0.05):", sig_positive_pairs, "\n")
cat("Significantly negative-correlated pairs (p < 0.05)", sig_negative_pairs, "\n")

## plotting correlation heat maps
corrplot(cor_matrix, type = "lower", method="color",
         col=colorRampPalette(c("#C5192D","#F3A361","white","#297270","#174753"))(200),
         addCoef.col = "black",number.cex = 0.7,  
         addgrid.col = "grey",diag =F, 
         tl.col="black",tl.cex = 0.7,
         addshade="all", 
         # p.mat = p_mat_adj,
         # insig="label_sig",
         # sig.level=0.05, pch.cex=0.8
         )

# constructing synergy and tradeoff network--------------------
## only considering significant correlations
p_mat_adj[is.na(p_mat_adj)] <- 1
cor_matrix[is.na(cor_matrix)] <- 0

adj_matrix <- ifelse(p_mat_adj < 0.05, cor_matrix, 0)

synergy_matrix <- adj_matrix
synergy_matrix[synergy_matrix < 0] <- 0

tradeoff_matrix <- adj_matrix
tradeoff_matrix[tradeoff_matrix > 0] <- 0

## constructing and plotting synergy network--------------
synergy_network <- graph_from_adjacency_matrix(
  synergy_matrix,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

### detecting community structure
E(synergy_network)$weight_clean <- E(synergy_network)$weight
E(synergy_network)$weight_clean[is.nan(E(synergy_network)$weight_clean) | is.na(E(synergy_network)$weight_clean)] <- 0

set.seed(42)
communities_synergy <- cluster_louvain(synergy_network, weights = abs(E(synergy_network)$weight_clean))

V(synergy_network)$membership <- communities_synergy$membership
V(synergy_network)$order <- order(V(synergy_network)$membership)

layout <- layout_in_circle(synergy_network,
                           order = V(synergy_network)$order)

eigen_centralities <- igraph::eigen_centrality(synergy_network,
                                               weights = abs(E(synergy_network)$weight_clean))$vector

### plotting network graphs
node_images <- sapply(1:17, function(i) {
  paste0(sdg_icons_path, "E-WEB-Goal-", sprintf("%02d", i), ".png")
  })

names(node_images) <- paste0("SDG", 1:17)

image_data <- data.frame(
  name = V(synergy_network)$name,
  x = layout[,1],
  y = layout[,2],
  image_path = node_images[V(synergy_network)$name],
  size = eigen_centralities[V(synergy_network)$name] * 0.08 + 0.05
)

p_synergy <- ggraph(synergy_network,layout=layout) +
  geom_mark_hull(aes(x, y, color = factor(V(synergy_network)$membership),
                     fill=factor(V(synergy_network)$membership),
                     group = factor(V(synergy_network)$membership)),
                 alpha = 0.3,
                 concavity = 8, expand = unit(2, "mm"), show.legend = F) +
  geom_edge_link(aes(color = abs(E(synergy_network)$weight_clean),
                     width=abs(E(synergy_network)$weight_clean) * 5)) +
  scale_edge_width(range=c(0,2.5)) +
  scale_edge_color_gradientn(colours = (RColorBrewer::brewer.pal(9,"Blues")[3:9])) +
  geom_image(data = image_data, 
             aes(x = x, y = y, image = image_path, size = I(size)),
             by = "height") +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  coord_fixed()+
  theme_void()

p_synergy

ggsave("/.../network_synergy.eps",
       width = 13, 
       height = 8,
       device = "eps")

## constructing and plotting tradeoff network-----------------
tradeoff_network <- graph_from_adjacency_matrix(
  tradeoff_matrix,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

### detecting community structure
E(tradeoff_network)$weight_clean <- E(tradeoff_network)$weight
E(tradeoff_network)$weight_clean[is.nan(E(tradeoff_network)$weight_clean) | is.na(E(tradeoff_network)$weight_clean)] <- 0

set.seed(42)
communities_tradeoff <- cluster_louvain(tradeoff_network, weights = abs(E(tradeoff_network)$weight_clean))

V(tradeoff_network)$membership <- communities_tradeoff$membership
V(tradeoff_network)$order <- order(V(tradeoff_network)$membership)

layout <- layout_in_circle(tradeoff_network,
                           order = V(tradeoff_network)$order)

eigen_centralities <- igraph::eigen_centrality(tradeoff_network,
                                               weights = abs(E(tradeoff_network)$weight_clean))$vector

### plotting network graphs
image_data <- data.frame(
  name = V(tradeoff_network)$name,
  x = layout[,1],
  y = layout[,2],
  image_path = node_images[V(tradeoff_network)$name],
  size = eigen_centralities[V(tradeoff_network)$name] * 0.08 + 0.05
)

p_tradeoff <- ggraph(tradeoff_network,layout=layout) +
  geom_mark_hull(aes(x, y, color = factor(V(tradeoff_network)$membership),
                     fill=factor(V(tradeoff_network)$membership),
                     group = factor(V(tradeoff_network)$membership)),
                 alpha = 0.3,
                 concavity = 8, expand = unit(2, "mm"), show.legend = F) +
  geom_edge_link(aes(color = abs(E(tradeoff_network)$weight_clean),
                     width=abs(E(tradeoff_network)$weight_clean) * 5)) +
  scale_edge_width(range=c(0,2.5)) +
  scale_edge_color_gradientn(colours = (RColorBrewer::brewer.pal(9,"Reds")[3:9])) +
  geom_image(data = image_data, 
             aes(x = x, y = y, image = image_path, size = I(size)),
             by = "height") +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  coord_fixed()+
  theme_void()

p_tradeoff

ggsave("/.../network_tradeoff.eps",
       width = 13, 
       height = 8,
       device = "eps")

## writing excel--------------------------------------------------
# synergy_list <- igraph::groups(communities_synergy)
# synergy_df <- data.frame(
#   community = rep(names(synergy_list), lengths(synergy_list)),
#   node = unlist(synergy_list),
#   row.names = NULL)
# 
# tradeoff_list <- igraph::groups(communities_tradeoff)
# tradeoff_df <- data.frame(
#   community = rep(names(tradeoff_list), lengths(tradeoff_list)),
#   node = unlist(tradeoff_list),
#   row.names = NULL)
# 
# writexl::write_xlsx(
#   list(synergy = synergy_df ,
#        tradeoff = tradeoff_df),
#   "/.../network_community.xlsx")

# calculating network metrics -----------------------------
## connectivity
synergy_network_connectivity <- sum(abs(E(synergy_network)$weight)) / (vcount(synergy_network) * (vcount(synergy_network) - 1) / 2)
tradeoff_network_connectivity <- sum(abs(E(tradeoff_network)$weight)) / (vcount(tradeoff_network) * (vcount(tradeoff_network) - 1) / 2)

## modularity
synergy_network_modularity <- modularity(communities_synergy)
tradeoff_network_modularity <- modularity(communities_tradeoff)

## centrality
V(synergy_network)$degree <- igraph::degree(synergy_network)
V(synergy_network)$closeness <- closeness(synergy_network, weights = abs(E(synergy_network)$weight_clean))
V(synergy_network)$betweenness <- betweenness(synergy_network, weights = abs(E(synergy_network)$weight_clean))
V(synergy_network)$eigen <- eigen_centrality(synergy_network, weights = abs(E(synergy_network)$weight_clean))$vector

V(tradeoff_network)$degree <- igraph::degree(tradeoff_network)
V(tradeoff_network)$closeness <- closeness(tradeoff_network, weights = abs(E(tradeoff_network)$weight_clean))
V(tradeoff_network)$betweenness <- betweenness(tradeoff_network, weights = abs(E(tradeoff_network)$weight_clean))
V(tradeoff_network)$eigen <- eigen_centrality(tradeoff_network, weights = abs(E(tradeoff_network)$weight_clean))$vector

synergy_centrality_df <- data.frame(
  SDG = V(synergy_network)$name,
  Degree = as.numeric(V(synergy_network)$degree),
  Closeness = as.numeric(V(synergy_network)$closeness),
  Betweenness = as.numeric(V(synergy_network)$betweenness),
  Eigenvector = as.numeric(V(synergy_network)$eigen)
)

tradeoff_centrality_df <- data.frame(
  SDG = V(tradeoff_network)$name,
  Degree = as.numeric(V(tradeoff_network)$degree),
  Closeness = as.numeric(V(tradeoff_network)$closeness),
  Betweenness = as.numeric(V(tradeoff_network)$betweenness),
  Eigenvector = as.numeric(V(tradeoff_network)$eigen)
)

cat("Synergy network connectivity:", round(synergy_network_connectivity, 3), "\n")
cat("Synergy network modularity:", round(synergy_network_modularity, 3), "\n")
print(synergy_centrality_df %>% arrange(SDG))

cat("Tradeoff network connectivity:", round(tradeoff_network_connectivity, 3), "\n")
cat("Tradeoff network modularity:", round(tradeoff_network_modularity, 3), "\n")
print(tradeoff_centrality_df %>% arrange(SDG))
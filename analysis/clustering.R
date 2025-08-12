library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(tidyr)
library(extrafont)
library(ggsignif)
library(reshape2)
library(arrow)
library(factoextra)
library(cluster)
library(corrplot)
library(gridExtra)
library(ggrepel)
library(RColorBrewer)

setwd("~/Desktop/UT/Projects/causal-filler-gap/analysis")
model_size = "2.8b"
model_name = paste0("pythia-", model_size)

generalization_df <- read_parquet("../results/generalization/classic.parquet")
generalization_df <- generalization_df[generalization_df$seed == 41 &
                                         generalization_df$model == model_name, ]

generalization_df_unique_single <- generalization_df %>%
  distinct() %>%
  filter(!grepl("embedded_clause", from)) %>%
  filter(!grepl("control", from)) %>%
  filter(!grepl("embedded_clause", to)) %>%
  filter(!to %in% c("embedded_control_lexical_animate", "embedded_control_lexical_inanimate", "embedded_control"))

gen_df <- generalization_df_unique_single
gen_df$to <- gsub("control_lexical_(animate|inanimate)", "control_lexical", gen_df$to)

max_odds_df <- gen_df %>%
  group_by(layer, pos, from, to, model, seed, leave_out, single_double) %>%
  summarize(mean_odds_ratio = mean(odds_ratio, na.rm = TRUE), 
            sd_avg = sd(odds_ratio, na.rm = TRUE),
            median_avg = median(mean_odds_ratio, na.rm = TRUE),
            n = n(), .groups = "drop")%>%
  group_by(pos, from, to, model, seed, leave_out, single_double)

clustering_df <- max_odds_df %>%
  ungroup() %>%
  mutate(layer_construct = paste0("layer_", layer, "_", to)) %>%
  select(pos, from, layer_construct, mean_odds_ratio) %>%
  pivot_wider(names_from = layer_construct, 
              values_from = mean_odds_ratio, 
              values_fill = NA)

filter_and_cluster <- function(filter_pos){
  # Filter by pos == filter_pos and drop NA rows
  clustering_df_pos <- clustering_df %>% 
    filter(pos == filter_pos) %>%
    select(-pos) %>%
    na.omit()%>%
    as.data.frame()  # Convert to base R data frame
  
  # Set row names from 'from' column, then drop it
  rownames(clustering_df_pos) <- clustering_df_pos$from
  clustering_df_pos <- clustering_df_pos %>% select(-from)
  
  # Remove columns with only one unique value
  clustering_df_pos_clean <- clustering_df_pos[, sapply(clustering_df_pos, function(x) length(unique(x)) > 1)]
  
  # plot optimal num clusters
  res <- fviz_nbclust(clustering_df_pos_clean, kmeans, method = "silhouette")
  
  # Or just use the data frame from the plot:
  df <- res$data
  optimal_k <- as.numeric(as.character(df$clusters[which.max(df$y)]))
  print(optimal_k)
  
  # K-means clustering
  k2 <- kmeans(clustering_df_pos_clean, centers = optimal_k, nstart = 100)
  out <- fviz_cluster(k2, data = clustering_df_pos_clean, labelsize = 7, main = paste0("Cluster Analysis at Position ", filter_pos))
  return(out)
}

filter_and_cluster(6)


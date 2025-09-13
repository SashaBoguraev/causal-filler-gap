library(tidyverse)
library(arrow)
library(ggrepel)
library(tibble)
library(ggtext)

model_size = "1.4b"
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

# Define construction colors as a global constant
CONSTRUCTION_COLORS <- c(
  'cleft' = '#636EFA',
  'embedded_wh_finite' = '#EF553B', 
  'embedded_wh_nonfinite' = '#00CC96',
  'pseudo_cleft' = '#AB63FA',
  'restricted_rc' = '#FFA15A',
  'topicalization' = '#19D3F3',
  'wh_question' = '#FF6692'
)

plot_pca_facet_by_pos <- function(clustering_df) {
  
  clustering_clean <- clustering_df %>%
    drop_na() %>%
    filter(!is.na(pos), !is.na(from))
  
  numeric_cols_all <- clustering_clean %>%
    select(-pos, -from) %>%
    select(where(is.numeric)) %>%
    names()
  
  pca_list <- list()
  
  for (current_pos in unique(clustering_clean$pos)) {
    df_sub <- clustering_clean %>%
      filter(pos == current_pos) %>%
      select(from, all_of(numeric_cols_all))
    
    numeric_cols_sub <- df_sub %>%
      select(-from) %>%
      select_if(~ length(unique(.)) > 1) %>%
      names()
    
    if (length(numeric_cols_sub) < 2 || nrow(df_sub) < 2) next
    
    df_sub_filtered <- df_sub %>%
      select(from, all_of(numeric_cols_sub)) %>%
      as.data.frame()
    
    rownames(df_sub_filtered) <- df_sub_filtered$from
    df_sub_filtered$from <- NULL
    
    pca_res <- prcomp(df_sub_filtered, scale. = TRUE)
    
    pca_df <- as.data.frame(pca_res$x[, 1:2])
    pca_df$sample_name <- rownames(pca_df)
    pca_df$pos <- current_pos
    
    pca_df <- pca_df %>%
      mutate(
        construction = sub("_(animate|inanimate)$", "", sample_name),
        animacy = ifelse(grepl("_animate$", sample_name), "animate", "inanimate")
      )
    
    pca_list[[as.character(current_pos)]] <- pca_df
  }
  
  all_pca_df <- bind_rows(pca_list)
  
  construction_colors <- c(
    'cleft' = '#008080',
    'embedded_wh_finite' = '#EF553B',
    'embedded_wh_nonfinite' = '#1f77b4',
    'pseudo_cleft' = '#FFA500',
    'restricted_rc' = '#2ca02c',
    'topicalization' = '#e377c2',
    'wh_question' = '#808080'
  )
  
  segments_df <- all_pca_df %>%
    select(pos, construction, animacy, PC1, PC2) %>%
    pivot_wider(names_from = animacy, values_from = c(PC1, PC2)) %>%
    filter(!is.na(PC1_animate) & !is.na(PC1_inanimate)) %>%
    mutate(
      mid_x = (PC1_animate + PC1_inanimate) / 2,
      mid_y = (PC2_animate + PC2_inanimate) / 2
    )
  
  pos_labels <- c(
    "2" = "{filler}",
    "4" = "the",
    "5" = "{np}",
    "6" = "{verb}"
  )
  
  construction_labels <- tibble(
    construction = c(
      'cleft',
      'embedded_wh_finite',
      'embedded_wh_nonfinite',
      'wh_question',
      'restricted_rc',
      'pseudo_cleft',
      'topicalization'
    ),
    construction_label = c(
      "Cleft",
      "Embedded Wh-Q\n(Know)",
      "Embedded Wh-Q\n(Wonder)",
      "Matrix\nWh-Q",
      "Res. Rel.\nClause",
      "Pseudo-Cleft",
      "Topicalization"
    )
  )
  
  all_pca_df <- all_pca_df %>%
    left_join(construction_labels, by = "construction")
  
  segments_df <- segments_df %>%
    left_join(construction_labels, by = "construction")
  
  p <- ggplot(all_pca_df, aes(x = PC1, y = PC2)) +
    geom_point(
      aes(color = construction, shape = animacy),
      position = position_jitter(width = 0.03, height = 0.03),
      size = 3,
      alpha = 0.9
    ) +
    geom_segment(
      data = segments_df,
      aes(x = PC1_animate, y = PC2_animate, xend = PC1_inanimate, yend = PC2_inanimate),
      linetype = "dotted",
      color = "gray40"
    ) +
    geom_text_repel(
      data = segments_df,
      aes(x = mid_x, y = mid_y, label = construction_label, color = construction),
      box.padding = 1.25,
      point.padding = 0.5,
      segment.curvature = -0.1,
      segment.ncp = 3,
      segment.angle = 20,
      force = 3,
      max.overlaps=Inf,
      size = 4, show.legend = FALSE, family = "Palatino", fontface = "bold"
    ) +
    scale_color_manual(values = construction_colors, na.value = "gray") +
    scale_shape_manual(values = c("animate" = 16, "inanimate" = 15)) +
    guides(color = "none") +
    facet_wrap(~ pos, labeller = labeller(pos = pos_labels)) +
    labs(
      # title = "PCA Faceted by Position",
      x = "PC1",
      y = "PC2",
      color = "Construction",
      shape = "Animacy"
    ) +
    theme_bw(base_size = 24, base_family = "Palatino") +
    theme(
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "grey80", color = "black", size = 1),
      strip.text = element_text(face = "bold", size = 18, family = "Palatino"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 14),
      axis.title = element_text(face = "bold", size = 18),
      axis.text = element_text(size = 16, color = "black"),
      legend.margin = margin(t = -10),  
      legend.box.margin = margin(t = -10),
      plot.margin = margin(10, 10, 10, 10),
    )
  
  return(p)
}
        
p <- plot_pca_facet_by_pos(clustering_df)

save_path <- paste0("plots/pca_", model_name, ".pdf")
ggsave(save_path, p, width=8, height=8)

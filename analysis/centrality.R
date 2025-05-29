library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(arrow) 
library(extrafont)
library(rstatix)  
library(tidyr)
library(patchwork)

model_suffix = "_1.4b"

# Get Data
df_all <- bind_rows(
  read_csv(paste0("../results/generalization/out_degree_auc",model_suffix,".csv")) %>%
    mutate(Clause = "Single Clause", Direction = "Out-Degree"),
  
  read_csv(paste0("../results/generalization/in_degree_auc",model_suffix,".csv")) %>%
    mutate(Clause = "Single Clause", Direction = "In-Degree"),
) %>%
  mutate(
    parent = recode(parent,
      "cleft" = "Cleft",
      "embedded_wh_nonfinite" = "Emb. Wh-Q\n(Wonder)",
      "embedded_wh_finite" = "Emb. Wh-Q\n(Know)",
      "wh_question" = "Matrix Wh-Q",
      "pseudo_cleft" = "Pseudocleft",
      "topicalization" = "Topicalization",
      "restricted_rc" = "Res. Rel.\nClause"
    ),
    parent = factor(parent, levels = c(
      "Res. Rel.\nClause",
      "Emb. Wh-Q\n(Wonder)",
      "Emb. Wh-Q\n(Know)",
      "Matrix Wh-Q",
      "Cleft",
      "Pseudocleft",
      "Topicalization"
    )),
    Clause = factor(Clause, levels = c("Single Clause", "Embedded Clause"))
  )

# Compute mean AUC
df_avg <- df_all %>%
  group_by(Clause, Direction, parent) %>%
  summarize(mean_auc = mean(auc, na.rm = TRUE), .groups = "drop")

make_plot <- function(df, dir, clause, filename) {
  df_filtered <- df %>%
    filter(Direction == dir, Clause == clause)
  
  df_avg <- df_filtered %>%
    group_by(parent) %>%
    summarize(auc = mean(auc, na.rm = TRUE)) %>%
    mutate(position = "Average")
  
  df_plot <- bind_rows(df_filtered, df_avg)
  positions <- unique(df_filtered$position)
  df_plot$position <- factor(df_plot$position, levels = c(positions, "Average"))
  
  p <- ggplot(df_plot, aes(x = parent, y = auc, fill = parent)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
    facet_grid(. ~ position, scales = "free_x") +
    labs(title = paste(dir, "—", clause), x = NULL, y = "AUC") +
    theme_bw(base_size = 14) +
    theme(
      text = element_text(face = "bold", family="Palatino"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(color = "black"),
      panel.spacing = unit(1, "lines"),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(hjust=.5, size=24)
    ) +
    scale_fill_brewer(palette = "Paired")
  
  ggsave(filename, p, width = 12, height = 3.5)
}

# Create all 4 plots (in appendix)
make_plot(df_all, "In-Degree", "Single Clause", paste0("plots/in_degree_single_clause_facet", model_suffix,".pdf"))
make_plot(df_all, "Out-Degree", "Single Clause", paste0("plots/out_degree_single_clause_facet", model_suffix,".pdf"))



# ================ SCATTER PLOTS ===============================================


freq_df <- tibble(
  parent = c(
    "Res. Rel.\nClause",
    "Emb. Wh-Q\n(Wonder)",
    "Emb. Wh-Q\n(Know)",
    "Matrix Wh-Q",
    "Cleft",
    "Pseudocleft",
    "Topicalization"
  ),
  frequency = c(504, 308, 308, 82, 20, 6, 6)  
)


df_avg_freq <- df_avg %>%
  left_join(freq_df, by = "parent") %>% filter(!grepl("Embedded Clause", Clause))


base_theme <- theme_bw(base_size = 24, base_family = "Palatino") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color = "black"),
    strip.background = element_blank(),
    strip.text = element_blank()
  )


label_colors <- c(
  "Res. Rel.\nClause" = "#2ca02c", # Blue
  "Emb. Wh-Q\n(Wonder)" = "#1f77b4", # Orange
  "Emb. Wh-Q\n(Know)" = "#d62728", # Green
  "Matrix Wh-Q" = "#808080", # Red
  "Cleft" = "#008080", # Purple
  "Pseudocleft" = "#FFA500", # Brown
  "Topicalization" = "#e377c2" # Pink
)

# Make text readable
# df_avg_freq_jittered_in <- df_avg_freq %>%
#   filter(Direction == "In-Degree") %>%
#   mutate(
#     jitter_x = case_when(
#       parent == "Topicalization" ~ frequency + 5,
#       parent == "Pseudocleft" ~ frequency + 3.5,
#       parent == "Emb. Wh-Q\n(Wonder)" ~ frequency - 175,
#       parent == "Emb. Wh-Q\n(Know)" ~ frequency,
#       parent == "Res. Rel.\nClause" ~ frequency - 125,
#       TRUE ~ frequency + runif(1, -0.01, 0.01)
#     ),
#     jitter_y = case_when(
#       parent == "Topicalization" ~ mean_auc - 0.075,
#       parent == "Pseudocleft" ~ mean_auc + 0.01, 
#       parent == "Emb. Wh-Q\n(Wonder)" ~ mean_auc - 0.125,
#       parent == "Emb. Wh-Q\n(Know)" ~ mean_auc -.035 ,
#       parent == "Res. Rel.\nClause" ~ mean_auc - 0.15, 
#       TRUE ~ mean_auc + runif(1, -0.02, 0.02)
#     )
#   )
# 
# df_avg_freq_jittered_out <- df_avg_freq %>%
#   filter(Direction == "Out-Degree") %>%
#   mutate(
#     jitter_x = case_when(
#       parent == "Topicalization" ~ frequency + 5,
#       parent == "Pseudocleft" ~ frequency + 3.5,
#       parent == "Emb. Wh-Q\n(Wonder)" ~ frequency,
#       parent == "Emb. Wh-Q\n(Know)" ~ frequency -.035,
#       parent == "Res. Rel.\nClause" ~ frequency - 125,
#       TRUE ~ frequency + runif(1, -0.01, 0.01)
#     ),
#     jitter_y = case_when(
#       parent == "Topicalization" ~ mean_auc + 0.01,
#       parent == "Pseudocleft" ~ mean_auc - 0.075, 
#       parent == "Emb. Wh-Q\n(Wonder)" ~ mean_auc - 0.13,
#       parent == "Emb. Wh-Q\n(Know)" ~ mean_auc - .035 ,
#       parent == "Res. Rel.\nClause" ~ mean_auc - 0.125, 
#       parent == "Matrix Wh-Q" ~ mean_auc - 0.05, 
#       TRUE ~ mean_auc + runif(1, -0.02, 0.02)
#     )
#   )

df_avg_freq_jittered_in <- df_avg_freq %>%
  filter(Direction == "In-Degree") %>%
  mutate(
    jitter_x = case_when(
      parent == "Topicalization" ~ frequency + 4,
      parent == "Pseudocleft" ~ frequency + 3.5,
      parent == "Emb. Wh-Q\n(Wonder)" ~ frequency - 175,
      parent == "Emb. Wh-Q\n(Know)" ~ frequency,
      parent == "Res. Rel.\nClause" ~ frequency - 100,
      TRUE ~ frequency + runif(1, -0.01, 0.01)
    ),
    jitter_y = case_when(
      parent == "Topicalization" ~ mean_auc - 0.05,
      parent == "Pseudocleft" ~ mean_auc + 0.01, 
      parent == "Emb. Wh-Q\n(Wonder)" ~ mean_auc - 0.1,
      parent == "Emb. Wh-Q\n(Know)" ~ mean_auc -.035 ,
      parent == "Res. Rel.\nClause" ~ mean_auc - 0.105, 
      TRUE ~ mean_auc + runif(1, -0.02, 0.02)
    )
  )

df_avg_freq_jittered_out <- df_avg_freq %>%
  filter(Direction == "Out-Degree") %>%
  mutate(
    jitter_x = case_when(
      parent == "Topicalization" ~ frequency + 5,
      parent == "Pseudocleft" ~ frequency + 3.5,
      parent == "Emb. Wh-Q\n(Wonder)" ~ frequency,
      parent == "Emb. Wh-Q\n(Know)" ~ frequency -.035,
      parent == "Res. Rel.\nClause" ~ frequency - 200,
      TRUE ~ frequency + runif(1, -0.01, 0.01)
    ),
    jitter_y = case_when(
      parent == "Topicalization" ~ mean_auc + 0.01,
      parent == "Pseudocleft" ~ mean_auc - 0.075, 
      parent == "Emb. Wh-Q\n(Wonder)" ~ mean_auc - 0.125,
      parent == "Emb. Wh-Q\n(Know)" ~ mean_auc - .035 ,
      parent == "Res. Rel.\nClause" ~ mean_auc- 0.07, 
      parent == "Matrix Wh-Q" ~ mean_auc - 0.05, 
      TRUE ~ mean_auc + runif(1, -0.02, 0.02)
    )
  )


p_in_scatter <- ggplot(df_avg_freq_jittered_in,
                       aes(x = frequency, y = mean_auc)) +  # removed color here
  geom_smooth(method = 'lm', se = FALSE, linetype = "dotted", color = "black") +  # dotted line
  geom_point(aes(color = parent), size = 3, alpha = 0.5) +
  geom_text(aes(x = jitter_x, y = jitter_y, label = parent, color = parent), 
            vjust = -1, size = 4, family = "Palatino", fontface = "bold")+ 
  labs(
    title = "In-Degree",
    x = NULL,
    y = "Mean AUC"
  ) +
  scale_x_log10() +
  ylim(0, .6) +
  scale_color_manual(values = label_colors) +
  base_theme +
  # theme(
  #   # Keep y-axis text and ticks
  #   axis.text.x = element_blank(),  
  #   axis.ticks.x = element_blank(),  
  #   axis.title.x = element_blank(),  
  #   axis.title.y = element_blank(),  
  #   plot.title = element_text(size = 36, family = "Palatino", hjust = 0.5, face = "bold"), 
  #   legend.position = "none" 
  # )
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(size = 36, family = "Palatino", hjust = 0.5, face = "bold"),
    legend.position = "none"
  )


p_out_scatter <- ggplot(df_avg_freq_jittered_out,
                        aes(x = frequency, y = mean_auc)) + 
  geom_smooth(method = 'lm', se = FALSE, linetype = "dotted", color = "black") + 
  geom_point(aes(color = parent), size = 3, alpha = 0.5) +
  geom_text(aes(x = jitter_x, y = jitter_y, label = parent, color = parent), 
            vjust = -1, size = 4, family = "Palatino", fontface = "bold") + 
  labs(
    title = "Out-Degree",
    x = NULL,
    y = NULL
  ) +
  scale_x_log10() +
  ylim(0, .6) +
  scale_color_manual(values = label_colors) +
  base_theme +
  # theme(
  #   axis.title.x = element_blank(),
  #   plot.title = element_text(size = 36, family = "Palatino", hjust = 0.5, face = "bold"),
  #   legend.position = "none"
  # )
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(size = 36, family = "Palatino", hjust = 0.5, face = "bold"),
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

common_y_title <- ggplot() + 
  annotate("text", x = 0, y = 0.5, label = "Mean AUC", 
           angle = 90, size = 12, family = "Palatino", fontface = "bold") +
  theme_void()

combined_plot <- p_in_scatter + p_out_scatter + 
  plot_layout(ncol = 2, widths = c(1, 1), guides = "collect")

# common_y_title <- ggplot() + 
#   annotate("text", x = 0, y = 0.5, label = "Mean AUC", 
#            angle = 90, size = 12, family = "Palatino", fontface = "bold") +
#   theme_void()

final_plot <- (common_y_title + combined_plot) + 
  plot_layout(widths = c(0.05, 1)) +
  plot_annotation(
    caption = "Construction Frequency (Log Scale)",
    theme = theme(
      plot.caption = element_text(size = 30, family = "Palatino", hjust = 0.6, face = "bold", margin = margin(t = 15))
    )
  )

final_plot

ggsave(paste0("plots/frequency_source_sink_narrow", model_suffix,".pdf"), final_plot, device = pdf, height = 6, width = 8, dpi = 10000)

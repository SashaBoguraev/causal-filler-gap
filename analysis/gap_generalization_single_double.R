library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(arrow) 
library(extrafont)
library(rstatix)  
library(tidyr)

model_size = "6.9b"
model_name = paste0("pythia-", model_size)

generalization_df_classic <- read_parquet("../results/generalization/classic.parquet")
generalization_df_classic <- generalization_df_classic[generalization_df_classic$seed == 41 &
                                                       generalization_df_classic$model == model_name, ]

generalization_df_classic <- generalization_df_classic %>%
  filter(!grepl("_only", from)) %>%
  # distinct() %>%
  filter(grepl("(embedded_clause)|(embedded_control)", from))%>%
  filter(!grepl("control", to))

generalization_df_classic$to <- gsub("embedded_control_lexical_(animate|inanimate)", "embedded_control_lexical", generalization_df_classic$to)

max_odds_df_classic <- generalization_df_classic %>%
  group_by(layer, pos, from, to, model, seed, leave_out, single_double) %>%
  summarize(mean_odds_ratio = mean(odds_ratio, na.rm = TRUE), 
            sd_avg = sd(odds_ratio, na.rm = TRUE),
            median_avg = median(mean_odds_ratio, na.rm = TRUE),
            n = n(), .groups = "drop")%>%
  group_by(pos, from, to, model, seed, leave_out, single_double) %>%
  summarize(
    max_avg = max(mean_odds_ratio, na.rm = TRUE)
  )%>%
  mutate(df_source = "classic") 

generalization_df_sd <- read_parquet("../results/generalization/single_double.parquet")
generalization_df_sd <- generalization_df_sd[generalization_df_sd$seed == 41 &
                                             generalization_df_sd$model == model_name, ]%>%
  filter(!grepl("control", from))%>%
  filter(!grepl("control", to))


max_odds_df_sd <- generalization_df_sd %>%
  group_by(layer, pos, from, to, model, seed, leave_out, single_double) %>%
  summarize(mean_odds_ratio = mean(odds_ratio, na.rm = TRUE), 
            sd_avg = sd(odds_ratio, na.rm = TRUE),
            median_avg = median(mean_odds_ratio, na.rm = TRUE),
            n = n(), .groups = "drop")%>%
  group_by(pos, from, to, model, seed, leave_out, single_double) %>%
  summarize(
    max_avg = max(mean_odds_ratio, na.rm = TRUE)
  ) %>%
  mutate(df_source = "sd")


max_odds_df <- bind_rows(max_odds_df_classic, max_odds_df_sd)

# ========= NORMALIZED ==========

from_order <- c(
  "embedded_control",
  "embedded_control_lexical_animate",
  "embedded_control_lexical_inanimate",
  "wh_question_animate",  
  "embedded_wh_finite_animate", 
  "embedded_wh_nonfinite_animate", 
  "restricted_rc_animate", 
  "cleft_animate", 
  "pseudo_cleft_animate", 
  "topicalization_animate", 
  "wh_question_inanimate",
  "embedded_wh_finite_inanimate", 
  "embedded_wh_nonfinite_inanimate", 
  "restricted_rc_inanimate", 
  "cleft_inanimate", 
  "pseudo_cleft_inanimate", 
  "topicalization_inanimate"
)

to_order <- c(
  "wh_question_animate",  
  "embedded_wh_finite_animate", 
  "embedded_wh_nonfinite_animate", 
  "restricted_rc_animate", 
  "cleft_animate", 
  "pseudo_cleft_animate", 
  "topicalization_animate", 
  "wh_question_inanimate",
  "embedded_wh_finite_inanimate", 
  "embedded_wh_nonfinite_inanimate", 
  "restricted_rc_inanimate", 
  "cleft_inanimate", 
  "pseudo_cleft_inanimate", 
  "topicalization_inanimate"
)

# Add animacy indicators
max_odds_df$animate_from <- !grepl("inanimate", max_odds_df$from)
max_odds_df$animate_to <- !grepl("inanimate", max_odds_df$to)
max_odds_df$from <- factor(gsub("_embedded_clause", "", max_odds_df$from), levels = from_order)
max_odds_df$to <- factor(gsub("_embedded_clause", "", max_odds_df$to), levels = to_order)
max_odds_df$parent_construction_from <- with(max_odds_df, 
                                             gsub("_animate", "", gsub("_inanimate", "", from))
)
max_odds_df$parent_construction_to <- with(max_odds_df, 
                                           gsub("_animate", "", gsub("_inanimate", "", to))
)

max_odds_df$animacy_condition <- with(max_odds_df, ifelse(
  from == "embedded_control",
  "control",
  ifelse(
    grepl("control_lexical", from),
    "control_lexical",
    paste0(
      ifelse(parent_construction_from == parent_construction_to, "SameDataset", "DiffDataset"), "_",
      ifelse(animate_from == animate_to, "SameAnimacy", "DiffAnimacy")
    )
  )
)
)

# ============ BAR CHART Discussed with Kyle ==========

# For proper error bar calculation, we need to go back to the original data
# Create a new result_summary dataframe with error statistics
result_summary_classic <- data.frame(
  diff_animacy_same_mean = numeric(),
  diff_animacy_same_ci = numeric(),
  same_animacy_other_mean = numeric(),
  same_animacy_other_ci = numeric(),
  diff_animacy_other_mean = numeric(),
  diff_animacy_other_ci = numeric(),
  same_animacy_same_mean = numeric(),
  same_animacy_same_ci = numeric(),
  controls_mean = numeric(),
  controls_ci = numeric(),
  pos = character(),
  from = character(),
  stringsAsFactors = FALSE
)

result_summary_sd <- data.frame(
  diff_animacy_same_mean = numeric(),
  diff_animacy_same_ci = numeric(),
  same_animacy_other_mean = numeric(),
  same_animacy_other_ci = numeric(),
  diff_animacy_other_mean = numeric(),
  diff_animacy_other_ci = numeric(),
  same_animacy_same_mean = numeric(),
  same_animacy_same_ci = numeric(),
  controls_mean = numeric(),
  controls_ci = numeric(),
  pos = character(),
  from = character(),
  stringsAsFactors = FALSE
)

unique_pos <- unique(max_odds_df$pos)
unique_from <- unique(max_odds_df$from)
unique_sources <- unique(max_odds_df$df_source)

# ===== AGG ALL =======
compute_mean_ci <- function(values) {
  values <- values[!is.na(values)]
  n <- length(values)
  mean_val <- mean(values)
  ci <- if(n > 1) {
    sd(values) / sqrt(n) * qt(0.975, df = n - 1)
  } else {
    NA  # Not enough data to compute CI
  }
  return(c(mean = mean_val, ci = ci))
}

for (pos_i in unique_pos) {
  for (src in unique_sources){
    from_data <- max_odds_df[max_odds_df$pos == pos_i & max_odds_df$df_source == src, ]
    
    diff_animacy_same_rows <- from_data[from_data$animacy_condition=="SameDataset_DiffAnimacy",]
    same_animacy_other_rows <- from_data[from_data$animacy_condition=="DiffDataset_SameAnimacy",]
    diff_animacy_other_rows <- from_data[from_data$animacy_condition=="DiffDataset_DiffAnimacy",]
    same_animacy_same_rows <- from_data[from_data$animacy_condition=="SameDataset_SameAnimacy",]
    control_rows <- from_data[from_data$animacy_condition == "control", ]
    control_lexical_rows <- from_data[from_data$animacy_condition == "control_lexical", ]
    
    diff_animacy_same_mean <- mean(diff_animacy_same_rows$max_avg, na.rm = TRUE)
    diff_animacy_same_ci <- sd(diff_animacy_same_rows$max_avg, na.rm = TRUE) / 
      sqrt(sum(!is.na(diff_animacy_same_rows$max_avg)))# * qt(0.975, df = sum(!is.na(diff_animacy_same_rows$max_avg)) - 1)
    
    same_animacy_other_mean <- mean(same_animacy_other_rows$max_avg, na.rm = TRUE)
    same_animacy_other_ci <- (sd(same_animacy_other_rows$max_avg, na.rm = TRUE) / 
                                sqrt(sum(!is.na(same_animacy_other_rows$max_avg))))# * qt(0.975, df = sum(!is.na(diff_animacy_same_rows$max_avg)) - 1)
    
    diff_animacy_other_mean <- mean(diff_animacy_other_rows$max_avg, na.rm = TRUE)
    diff_animacy_other_ci <- sd(diff_animacy_other_rows$max_avg, na.rm = TRUE) / 
      sqrt(sum(!is.na(diff_animacy_other_rows$max_avg)))# * qt(0.975, df = sum(!is.na(diff_animacy_same_rows$max_avg)) - 1)
    
    same_animacy_same_mean <- mean(same_animacy_same_rows$max_avg, na.rm = TRUE)
    same_animacy_same_ci <- sd(same_animacy_same_rows$max_avg, na.rm = TRUE) / 
      sqrt(sum(!is.na(same_animacy_same_rows$max_avg)))# * qt(0.975, df = sum(!is.na(diff_animacy_same_rows$max_avg)) - 1)
    
    controls_mean <- mean(control_rows$max_avg, na.rm = TRUE)
    controls_ci <- sd(control_rows$max_avg, na.rm = TRUE) / 
      sqrt(sum(!is.na(control_rows$max_avg)))# * qt(0.975, df = sum(!is.na(diff_animacy_same_rows$max_avg)) - 1)
    
    controls_lexical_mean <- mean(control_lexical_rows$max_avg, na.rm = TRUE)
    controls_lexical_ci <- sd(control_lexical_rows$max_avg, na.rm = TRUE) / 
      sqrt(sum(!is.na(control_lexical_rows$max_avg)))# * qt(0.975, df = sum(!is.na(diff_animacy_same_rows$max_avg)) - 1)
    
    row_vector <- data.frame(
      diff_animacy_same_mean = diff_animacy_same_mean,
      diff_animacy_same_ci = diff_animacy_same_ci,
      same_animacy_other_mean = same_animacy_other_mean,
      same_animacy_other_ci = same_animacy_other_ci,
      diff_animacy_other_mean = diff_animacy_other_mean,
      diff_animacy_other_ci = diff_animacy_other_ci,
      same_animacy_same_mean = same_animacy_same_mean,
      same_animacy_same_ci = same_animacy_same_ci,
      controls_mean = controls_mean,
      controls_ci = controls_ci,
      controls_lexical_mean = controls_lexical_mean,
      controls_lexical_ci = controls_lexical_ci,
      pos = pos_i
    )
    
    if (src == "sd") {
      result_summary_sd <- rbind(result_summary_sd, row_vector)
    } else {
      result_summary_classic <- rbind(result_summary_classic, row_vector)
    }
  }
}

result_summary_sd <- result_summary_sd %>% mutate(df_source = "sd")
result_summary_classic <- result_summary_classic %>% mutate(df_source = "classic")

result_summary <- rbind(result_summary_sd, result_summary_classic)

df_means <- result_summary %>%
  select(pos, df_source, ends_with("_mean")) %>%
  pivot_longer(
    cols = -c(pos, df_source),
    names_to = "Condition",
    values_to = "Value"
  ) %>%
  mutate(Condition = gsub("_mean$", "", Condition))

df_cis <- result_summary %>%
  select(pos, df_source, ends_with("_ci")) %>%
  pivot_longer(
    cols = -c(pos, df_source),
    names_to = "Condition",
    values_to = "SE"
  ) %>%
  mutate(Condition = gsub("_ci$", "", Condition))

df_long <- left_join(df_means, df_cis, by = c("pos", "df_source", "Condition"))

df_long$pos <- factor(df_long$pos, levels = c(2, 4, 5, 6, 7, 8, 9, 10), 
                      labels = c("{filler}", "the_1", "{np1}", "{verb1}", "that", "the_2", "{np2}", "{verb2}"))

df_long$source_group <- factor(df_long$df_source, levels = c("classic", "sd"), 
                               labels = c("Embedded → Embedded", "Single → Embedded"))

condition_labels <- c(
  "same_animacy_same" = "Same Animacy, In Train Set",
  "diff_animacy_same" = "Different Animacy, In Train Set",
  "same_animacy_other" = "Same Animacy, Left Out",
  "diff_animacy_other" = "Different Animacy, Left Out",
  "embedded_control" = "Controls",
  "embedded_control_lexical" = "Lexical Controls"
)

df_long$Condition <- factor(df_long$Condition, 
                            levels = c("same_animacy_same", 
                                       "diff_animacy_same",
                                       "same_animacy_other",
                                       "diff_animacy_other",
                                       "controls",
                                       "controls_lexical"),
                            labels = condition_labels)

df_long <- df_long[df_long$Condition == "Same Animacy, In Train Set" |
                     (grepl("Controls", df_long$Condition) & df_long$source_group == "Embedded → Embedded"), ]


total_plot <- ggplot(df_long, aes(x = interaction(source_group, Condition), y = Value, fill = interaction(Condition, source_group))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8, alpha = 0.9) +
  
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  
  facet_grid(. ~ pos, scales = "free_x") +
  
  theme_bw(base_size = 11) +
  theme(
    text = element_text(family = "Palatino", face = "bold", size = 12),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1, "lines"),
    strip.text = element_text(face = "bold"),
    strip.text.y = element_blank(),
    strip.background = element_rect(color = "black", linetype = "solid"),
    
    legend.position = "bottom",            
    legend.direction = "horizontal",    
    legend.box = "horizontal",
    legend.text = element_text(size = 9),   
    legend.key.size = unit(0.4, "cm"),     
    legend.spacing.y = unit(0.1, "cm"),    
    legend.margin = margin(t = -5, unit = "pt"), 
    
    plot.title = element_text(hjust = 0.5)
  ) +
  
  scale_fill_brewer(
    palette = "Paired",  # Use a palette with more distinguishable colors
    name = element_blank(),
    labels = function(x) {
      sapply(x, function(label) {
        if (grepl("Controls", label)) {
          # Extract what comes before the period for Controls
          before_period <- sub("\\..*", "", label)
          return(before_period)
        } else {
          # Extract what comes after the period for non-Controls
          after_period <- sub(".*\\.", "", label)
          return(after_period)
        }
      })
    }
  ) +
  
  labs(
    title = paste0(model_name, ", Average Max Odds Across Position"),
    x = "Evaluation Condition",
    y = element_blank()
  )
total_plot




save_path_agg_all <- paste0("plots/sd_aggregated_max_", model_name,".pdf")
ggsave(plot = total_plot, filename = save_path_agg_all, width = 8, height = 1.75, dpi = 10000, device = 'pdf')

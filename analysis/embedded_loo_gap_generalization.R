library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(arrow) 
library(extrafont)
library(rstatix)  
library(tidyr)

model_size = "1.4b"
model_name = paste0("pythia-", model_size)

generalization_df <- read_parquet("../results/generalization/leave_out.parquet")
generalization_df <- generalization_df[generalization_df$to != "control_lexical_animate" &
                                       generalization_df$seed == 41 &
                                       generalization_df$model == model_name, ]%>%
  filter(!grepl("_only", from))

generalization_df_unique_single <- generalization_df %>%
  filter(grepl("embedded_clause|embedded_control", from))%>%
  distinct()

generalization_df_unique_single$`__index_level_0__`=NULL

gen_df <- generalization_df_unique_single
gen_df$to <- gsub("embedded_control_lexical_(animate|inanimate)", "embedded_control_lexical", gen_df$to)

max_odds_df <- gen_df %>%
  group_by(layer, pos, from, to, model, seed, leave_out, single_double) %>%
  summarize(mean_odds_ratio = mean(odds_ratio, na.rm = TRUE), 
            sd_avg = sd(odds_ratio, na.rm = TRUE),
            median_avg = median(mean_odds_ratio, na.rm = TRUE),
            n = n(), .groups = "drop")%>%
  group_by(pos, from, to, model, seed, leave_out, single_double) %>%
  summarize(
    max_avg = max(mean_odds_ratio, na.rm = TRUE)
  )


max_odds_df <- max_odds_df %>%
  mutate(
    from = gsub("_embedded_clause", "", from),
    to = gsub("_embedded_clause", "", to)
  ) %>%
  distinct()


# Add animacy indicators
max_odds_df$animate_from <- !grepl("inanimate", max_odds_df$from)
max_odds_df$animate_to <- !grepl("inanimate", max_odds_df$to)
max_odds_df$parent_construction_from <- with(max_odds_df, 
                                             gsub("_animate", "", gsub("_inanimate", "", from))
)
max_odds_df$parent_construction_to <- with(max_odds_df, 
                                           gsub("_animate", "", gsub("_inanimate", "", to))
)

max_odds_df$animacy_condition <- with(max_odds_df, ifelse(
  to == "embedded_control",
  "control",
  ifelse(
    to == "embedded_control_lexical",
    "control_lexical",
    paste0(
      ifelse(parent_construction_from == parent_construction_to, "SameDataset", "DiffDataset"), "_",
      ifelse(animate_from == animate_to, "SameAnimacy", "DiffAnimacy")
    )
  )
)
)


# ========= NORMALIZED ==========
unique_pos = unique(max_odds_df$pos)
unique_from = unique(max_odds_df$from)
max_odds_df$normal <- NA

for (pos_i in unique_pos) {
  for (from_val in unique_from) {

    # Subset the data for the current 'from' and 'pos'
    from_data <- max_odds_df[max_odds_df$from == from_val
                             & max_odds_df$pos == pos_i, ]

    row_index_from <- which(from_data$animacy_condition == "DiffDataset_SameAnimacy")

    normal <- mean(from_data$max_avg[row_index_from])

    for (row_i in 1:nrow(from_data)) {

      # Get 'to' value and the metrics for the row
      current_val <- from_data$max_avg[row_i]

      # Perform the normalization for the current row
      max_odds_df$normal[max_odds_df$from == from_val
                         & max_odds_df$pos == pos_i
                         & max_odds_df$to == from_data$to[row_i]] <- current_val / normal
    }
  }
}

from_order <- c(
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
  "embedded_control",
  "embedded_control_lexical",
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

max_odds_df$from <- factor(gsub("_embedded_clause", "", max_odds_df$from), levels = from_order)
max_odds_df$to <- factor(gsub("_embedded_clause", "", max_odds_df$to), levels = to_order)

pos_labels <- c(
  "2" = "{filler}",
  "4" = "the_1",
  "5" = "{np1}",
  "6" = "{verb1}",
  "7" = "that",
  "8" = "the_2",
  "9" = "{np2}",
  "10" = "{verb2}"
)

max_odds_df$pos <- factor(max_odds_df$pos, levels = c(2, 4, 5, 6, 7, 8 ,9, 10), labels = pos_labels)

heatmap_plot <- ggplot(max_odds_df, aes(x = to, y = from, fill = max_avg)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "magenta4") +
  facet_wrap(~ as.factor(pos), ncol = 8) +
  
  geom_vline(xintercept = c(2.5, 9.5), color = "black", size = 1) +  # After 1st and 8th column
  geom_hline(yintercept = 7.5, color = "black", size = 1) +         # After 7th row
  
  theme_bw() +
  theme(
    text = element_text(family="Times New Roman", face="bold", size=12),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    strip.background = element_rect(color = "black", size = 1)
  ) +
  labs(
    title = "Max Log Odds",
    x = "Eval Construction",
    y = "Train Construction",
    fill ="Max Log Odds"
  )
heatmap_plot
save_path <- paste0("plots/loo_gap_generalization_embedded_max_", model_name,".png")
ggsave(plot = heatmap_plot, filename = save_path, width = 16, height = 4)

unique(max_odds_df$to)
normal_df = max_odds_df[max_odds_df$to != "embedded_control",]

heatmap_plot_normal <- ggplot(max_odds_df, aes(x = to, y = from, fill = normal)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "magenta4") +
  facet_wrap(~ as.factor(pos), ncol = 8) +
  
  geom_vline(xintercept = c(1.5, 8.5), color = "black", size = 1) +  # After 1st and 8th column
  geom_hline(yintercept = 7.5, color = "black", size = 1) +         # After 7th row
  
  theme_bw() +
  theme(
    text = element_text(family="Times New Roman", face="bold", size=12),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1), 
    strip.background = element_rect(color = "black", size = 1)
  ) +
  labs(
    title = "Max Log Odds Ratio",
    x = "Eval Construction",
    y = "Train Construction",
    fill ="Max Log Odds Ratio"
  )
heatmap_plot_normal
save_path <- paste0("plots/loo_normalized_embedded_max_", model_name,".png")
ggsave(plot = heatmap_plot_normal, filename = save_path, width = 16, height = 2.5)

# ============ BAR CHART Discussed with Kyle ==========

result_summary <- data.frame(
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

# ===== AGG ALL =======

result_summary <- data.frame()
significance_data <- data.frame()

for (pos_i in unique_pos) {
  
  from_data <- max_odds_df[max_odds_df$pos == pos_i, ]
  
  test_data <- data.frame(
    value = from_data$normal,
    condition = from_data$animacy_condition
  )
  
  # Filter NA values
  test_data <- test_data[!is.na(test_data$value), ]
  
  if(length(unique(test_data$condition)) > 1 && nrow(test_data) > 2) {
    stat_test <- test_data %>%
      pairwise_t_test(value ~ condition, p.adjust.method = "bonferroni") %>%
      add_significance()
    stat_test$pos <- pos_i
    significance_data <- rbind(significance_data, stat_test)
  }
  
  diff_animacy_same_rows <- from_data[from_data$animacy_condition=="SameDataset_DiffAnimacy",]
  same_animacy_other_rows <- from_data[from_data$animacy_condition=="DiffDataset_SameAnimacy",]
  diff_animacy_other_rows <- from_data[from_data$animacy_condition=="DiffDataset_DiffAnimacy",]
  same_animacy_same_rows <- from_data[from_data$animacy_condition=="SameDataset_SameAnimacy",]
  control_rows <- from_data[from_data$animacy_condition == "control", ]
  control_lexical_rows <- from_data[from_data$animacy_condition == "control_lexical", ]
  
  diff_animacy_same_mean <- mean(diff_animacy_same_rows$normal, na.rm = TRUE)
  diff_animacy_same_ci <- sd(diff_animacy_same_rows$normal, na.rm = TRUE) / 
    sqrt(sum(!is.na(diff_animacy_same_rows$normal))) #* qt(0.975, df = sum(!is.na(diff_animacy_same_rows$normal)) - 1)
  
  same_animacy_other_mean <- mean(same_animacy_other_rows$normal, na.rm = TRUE)
  same_animacy_other_ci <- (sd(same_animacy_other_rows$normal, na.rm = TRUE) / 
                              sqrt(sum(!is.na(same_animacy_other_rows$normal)))) #* qt(0.975, df = sum(!is.na(diff_animacy_same_rows$normal)) - 1)
  
  diff_animacy_other_mean <- mean(diff_animacy_other_rows$normal, na.rm = TRUE)
  diff_animacy_other_ci <- sd(diff_animacy_other_rows$normal, na.rm = TRUE) / 
    sqrt(sum(!is.na(diff_animacy_other_rows$normal))) #* qt(0.975, df = sum(!is.na(diff_animacy_same_rows$normal)) - 1)
  
  same_animacy_same_mean <- mean(same_animacy_same_rows$normal, na.rm = TRUE)
  same_animacy_same_ci <- sd(same_animacy_same_rows$normal, na.rm = TRUE) / 
    sqrt(sum(!is.na(same_animacy_same_rows$normal))) #* qt(0.975, df = sum(!is.na(diff_animacy_same_rows$normal)) - 1)
  
  controls_mean <- mean(control_rows$normal, na.rm = TRUE)
  controls_ci <- sd(control_rows$normal, na.rm = TRUE) / 
    sqrt(sum(!is.na(control_rows$normal))) #* qt(0.975, df = sum(!is.na(diff_animacy_same_rows$normal)) - 1)
  
  controls_lexical_mean <- mean(control_lexical_rows$normal, na.rm = TRUE)
  controls_lexical_ci <- sd(control_lexical_rows$normal, na.rm = TRUE) / 
    sqrt(sum(!is.na(control_lexical_rows$normal))) #* qt(0.975, df = sum(!is.na(diff_animacy_same_rows$normal)) - 1)
  
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
    pos = pos_i,
    from = from_val
  )
  
  result_summary <- rbind(result_summary, row_vector)
}

df_means <- result_summary %>%
  select(pos, ends_with("_mean")) %>%
  pivot_longer(
    cols = -pos,
    names_to = "Condition",
    values_to = "Value"
  ) %>%
  mutate(Condition = gsub("_mean$", "", Condition))

df_cis <- result_summary %>%
  select(pos, ends_with("_ci")) %>%
  pivot_longer(
    cols = -pos,
    names_to = "Condition",
    values_to = "SE"
  ) %>%
  mutate(Condition = gsub("_ci$", "", Condition))

df_long <- left_join(df_means, df_cis, by = c("pos", "Condition"))

condition_labels <- c(
  "same_animacy_same" = "Same Animacy, Left Out",
  "diff_animacy_same" = "Different Animacy, Left Out",
  "same_animacy_other" = "Same Animacy, In Train Set",
  "diff_animacy_other" = "Different Animacy, In Train Set",
  "control" = "Controls",
  "control_lexical" = "Lexical Controls"
)

df_long$Condition <- factor(df_long$Condition, 
                            levels = c("same_animacy_same", 
                                       "diff_animacy_same",
                                       "same_animacy_other",
                                       "diff_animacy_other",
                                       "controls",
                                       "controls_lexical"),
                            labels = condition_labels)


df_long$pos <- factor(df_long$pos, levels = c("{filler}", "the_1", "{np1}", "{verb1}", "that", "the_2", "{np2}", "{verb2}"))


total_plot <- ggplot(df_long, aes(x = Condition, y = Value, fill = Condition)) +
  geom_col(position = "dodge", width = 0.7, alpha = 0.7) +
  geom_errorbar(aes(ymin = Value - SE, ymax = Value + SE), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  #geom_jitter(data = original_data, 
  #            aes(x = Condition, y = Value),
  #            position = position_jitterdodge(jitter.width = 1.0, dodge.width = 1.0),
  #            shape = 21, color = "black", size = .5, alpha = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +  
  facet_grid(. ~ pos, scales = "free_x") +
  theme_bw(base_size = 12) +
  theme(
    text=element_text(family="Palatino", face="bold", size=12),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1, "lines"),
    strip.text = element_text(face = "bold"),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(
      color="black", linetype="solid"
    ),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_brewer(palette = "Set2") +
  
  scale_y_continuous(breaks = c(0.0, 0.25, 0.5, 0.75, 1.0)) +
  labs(
    title = paste0(model_name, ", Average Max Log Odds Ratio by Position"),
    x = element_blank(),
    y = element_blank()
  )

total_plot

save_path_agg_all <- paste0("plots/loo_embed_aggregated_max_", model_name,".pdf")
ggsave(plot = total_plot, filename = save_path_agg_all, width = 8, height = 3, device='pdf')










calculate_pvalues_modified <- function(df) {
  conditions <- c("SameDataset_SameAnimacy", "SameDataset_DiffAnimacy", "DiffDataset_SameAnimacy", "DiffDataset_DiffAnimacy")
  
  results <- data.frame()
  
  for (pos_i in unique(df$pos)) {
    pos_data <- df[df$pos == pos_i, ]
    
    control_data <- pos_data$normal[pos_data$to == "embedded_control"]
    control_lexical_data <- pos_data$normal[pos_data$to == "embedded_control_lexical"]
    
    for (cond in conditions) {
      cond_data <- pos_data$normal[pos_data$animacy_condition == cond]
      
      if (length(cond_data) < 2) {
        next
      }
      
      if (length(control_data) >= 2) {
        t_result <- t.test(cond_data, control_data)
        p_value <- t_result$p.value
        
        results <- rbind(results, data.frame(
          position = pos_i,
          condition = cond,
          comparison = "vs_standard_control",
          p_value = p_value,
          significant = p_value < 0.05
        ))
      }
      
      if (length(control_lexical_data) >= 2) {
        t_result_lex <- t.test(cond_data, control_lexical_data)
        p_value_lex <- t_result_lex$p.value
        
        results <- rbind(results, data.frame(
          position = pos_i,
          condition = cond,
          comparison = "vs_lexical_control",
          p_value = p_value_lex,
          significant = p_value_lex < 0.05
        ))
      }
    }
  }
  
  results$condition <- factor(results$condition,
                              levels = c("SameDataset_SameAnimacy", "SameDataset_DiffAnimacy", 
                                         "DiffDataset_SameAnimacy", "DiffDataset_DiffAnimacy"),
                              labels = c("Same Animacy, Not in Train Set", 
                                         "Different Animacy, Not in Train Set",
                                         "Same Animacy, In Train Set", 
                                         "Different Animacy, In Train Set"))
  
  return(results)
}

# Calculate p-values
pvalue_results <- calculate_pvalues_modified(max_odds_df)

for (pos_i in unique(pvalue_results$position)) {
  cat("\nPosition:", pos_i, "\n")
  cat("==============================================\n")
  
  pos_results <- pvalue_results[pvalue_results$position == pos_i, ]
  
  for (cond in unique(pos_results$condition)) {
    
    cat(cond, "\n")
    cat("---------------------------------------------\n")
    cond_results <- pos_results[pos_results$condition == cond, ]
    
    # vs standard controls
    std_comp <- cond_results[cond_results$comparison == "vs_standard_control", ]
    if (nrow(std_comp) > 0) {
      sig_symbol <- ifelse(std_comp$significant, " *", "")
      cat(sprintf("vs Standard Controls           : p = %.4f%s\n", 
                  std_comp$p_value, sig_symbol))
    }
    
    # vs lexical controls
    lex_comp <- cond_results[cond_results$comparison == "vs_lexical_control", ]
    if (nrow(lex_comp) > 0) {
      sig_symbol <- ifelse(lex_comp$significant, " *", "")
      cat(sprintf("vs Lexical Controls            : p = %.4f%s\n", 
                  lex_comp$p_value, sig_symbol))
    }
    
    cat("\n")
  }
}

cat("\nWith Holm-Bonferroni Correction (controlling FDR):\n")
cat("==============================================\n")

for (pos_i in unique(pvalue_results$position)) {
  cat("\nPosition:", pos_i, "\n")
  cat("==============================================\n")
  
  pos_results <- pvalue_results[pvalue_results$position == pos_i, ]
  
  p_values_df <- data.frame(
    condition = character(),
    comparison = character(),
    p_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (cond in unique(pos_results$condition)) {
    cond_results <- pos_results[pos_results$condition == cond, ]
    
    for (comp in unique(cond_results$comparison)) {
      comp_result <- cond_results[cond_results$comparison == comp, ]
      if (nrow(comp_result) > 0) {
        p_values_df <- rbind(p_values_df, data.frame(
          condition = as.character(cond),
          comparison = comp,
          p_value = comp_result$p_value,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  if (nrow(p_values_df) > 0) {
    p_values_df$adj_p_value <- p.adjust(p_values_df$p_value, method = "holm")
    
    p_values_df$significant <- p_values_df$adj_p_value < 0.05
  }
  
  for (cond in unique(pos_results$condition)) {
    
    cat(cond, "\n")
    cat("---------------------------------------------\n")
    
    # vs standard controls
    cond_std <- subset(p_values_df, condition == cond & comparison == "vs_standard_control")
    if (nrow(cond_std) > 0) {
      sig_symbol <- ifelse(cond_std$significant, " *", "")
      cat(sprintf("vs Standard Controls           : p = %.4f (adjusted: %.4f)%s\n", 
                  cond_std$p_value, cond_std$adj_p_value, sig_symbol))
    }
    
    # vs lexical controls
    cond_lex <- subset(p_values_df, condition == cond & comparison == "vs_lexical_control")
    if (nrow(cond_lex) > 0) {
      sig_symbol <- ifelse(cond_lex$significant, " *", "")
      cat(sprintf("vs Lexical Controls            : p = %.4f (adjusted: %.4f)%s\n", 
                  cond_lex$p_value, cond_lex$adj_p_value, sig_symbol))
    }
    
    cat("\n")
  }
}


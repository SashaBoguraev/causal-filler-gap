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

generalization_df <- read_parquet("../results/generalization/leave_out.parquet")
generalization_df <- generalization_df[generalization_df$model == model_name,]

generalization_df_unique <- generalization_df %>%
  distinct() %>%
  filter(pos != 3,                                                               # "NC" position
         seed == 41)

generalization_df_unique$`__index_level_0__`=NULL

gen_df <- generalization_df_unique
gen_df$to <- gsub("control_lexical_(animate|inanimate)", "control_lexical", gen_df$to) # Collapse lexical controls into one class

odds_df <- gen_df %>%
  group_by(layer, pos, from, to, model, seed, leave_out, single_double) %>%
  summarize(mean_odds_ratio = mean(odds_ratio, na.rm = TRUE), 
            sd_avg = sd(odds_ratio, na.rm = TRUE),
            median_avg = median(mean_odds_ratio, na.rm = TRUE),
            n = n(), .groups = "drop")


# Add animacy indicators
odds_df$animate_from <- !grepl("inanimate", odds_df$from)
odds_df$animate_to <- !grepl("inanimate", odds_df$to)
odds_df$parent_construction_from <- with(odds_df, 
                                             gsub("_animate", "", gsub("_inanimate", "", from))
)
odds_df$parent_construction_to <- with(odds_df, 
                                           gsub("_animate", "", gsub("_inanimate", "", to))
)

odds_df$animacy_condition <- with(odds_df, ifelse(
  to == "control" | to == "embedded_control",
  "Control",
  ifelse(
    to == "control_lexical" | to == "embedded_control_lexical",
    "Lexical Control",
    paste0(
      ifelse(parent_construction_from == parent_construction_to, "Left Out", "In Train Set"), ", \n",
      ifelse(animate_from == animate_to, "Same Animacy", "Different Animacy")
    )
  )
)
)


single_df <- odds_df %>% filter(!grepl("embedded_clause", from))
embed_df <- odds_df %>% filter(grepl("embedded_clause", from))




# SINGLE CLAUSE WITH CONTROLS

plot_df <- single_df[single_df$pos %in% c(2, 4, 5, 6), ]
plot_df$pos <- as.numeric(as.character(plot_df$pos))
plot_df$pos <- factor(plot_df$pos, levels = rev(c(2, 4, 5, 6)))
plot_df$animacy_condition <- factor(plot_df$animacy_condition, levels = c(
  "In Train Set, \nSame Animacy",
  "Left Out, \nSame Animacy",
  "In Train Set, \nDifferent Animacy",
  "Left Out, \nDifferent Animacy",
  "Control",
  "Lexical Control"
))

plot_df$construction_label <- plot_df$parent_construction_from
construction_mapping <- c(
  "cleft" = "Cleft",
  "embedded_wh_finite" = "Embedded \nWh-Question \n(Know-Class)",
  "embedded_wh_nonfinite" = "Embedded \nWh-Question \n(Wonder-Class)",
  "pseudo_cleft" = "Pseudocleft",
  "restricted_rc" = "Restricted \nRelative Clause",
  "topicalization" = "Topicalization",
  "wh_question" = "Matrix \nWh-Question"
)

for (old_name in names(construction_mapping)) {
  plot_df$construction_label[plot_df$parent_construction_from == old_name] <- construction_mapping[old_name]
}

p <- ggplot(plot_df, aes(x = layer, y = factor(pos), fill = mean_odds_ratio)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "magenta4") +
  facet_grid(construction_label ~ animacy_condition, scales = "free_y", space = "free_y") +
  scale_y_discrete(
    breaks = c("2", "4", "5", "6"),
    labels = c("{filler}", "the", "{np}", "{verb}")
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Palatino", face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(color = "black", size = 1),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 270, hjust = 0.5)#, size = 7)
  ) +
  labs(
    title = "Generalization Across Constructions",
    x = "Layer",
    y = "Position",
    fill = "Odds Ratio"
  )
print(p)
save_path = paste0("plots/stacked_constructions_all_", model_name,".pdf")
ggsave(filename = save_path, 
       plot = p, 
       width = 10.5, 
       device = 'pdf',
       height = 10)

# SINGLE CLAUSE WITHOUT CONTROLS

plot_df <- plot_df[plot_df$animacy_condition != "Control" & 
                     plot_df$animacy_condition != "Lexical Control", ]
p <- ggplot(plot_df, aes(x = layer, y = factor(pos), fill = mean_odds_ratio)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "magenta4") +
  facet_grid(construction_label ~ animacy_condition, scales = "free_y", space = "free_y") +
  scale_y_discrete(
    breaks = c("2", "4", "5", "6"),
    labels = c("{filler}", "the", "{np}", "{verb}")
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Palatino", face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.position = "bottom",
    legend.title = element_text(vjust = .75),
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(color = "black", size = 1),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 270, hjust = 0.5, size = 12)
  ) +
  labs(
    title = "Generalization Across Constructions",
    x = "Layer",
    y = "Position",
    fill = "Odds Ratio"
  )
print(p)
save_path = paste0("plots/stacked_constructions_no_controls_", model_name,".pdf")
ggsave(filename = save_path, 
       plot = p, 
       width = 6.5, 
       device = 'pdf',
       height = 14.3)



# EMBEDDED CLAUSE WITH CONTROLS
plot_df <- embed_df[embed_df$pos %in% c(2, 4, 5, 6, 7, 8, 9, 10), ]
plot_df$pos <- as.numeric(as.character(plot_df$pos))
plot_df$pos <- factor(plot_df$pos, levels = rev(c(2, 4, 5, 6, 7, 8, 9, 10)))
plot_df$animacy_condition <- factor(plot_df$animacy_condition, levels = c(
  "In Train Set, \nSame Animacy",
  "Left Out, \nSame Animacy",
  "In Train Set, \nDifferent Animacy",
  "Left Out, \nDifferent Animacy",
  "Control",
  "Lexical Control"
))

plot_df$construction_label <- plot_df$parent_construction_from
construction_mapping <- c(
  "cleft_embedded_clause" = "Cleft",
  "embedded_wh_finite_embedded_clause" = "Embedded \nWh-Question \n(Know-Class)",
  "embedded_wh_nonfinite_embedded_clause" = "Embedded \nWh-Question \n(Wonder-Class)",
  "pseudo_cleft_embedded_clause" = "Pseudocleft",
  "restricted_rc_embedded_clause" = "Restricted \nRelative Clause",
  "topicalization_embedded_clause" = "Topicalization",
  "wh_question_embedded_clause" = "Matrix \nWh-Question"
)

for (old_name in names(construction_mapping)) {
  plot_df$construction_label[plot_df$parent_construction_from == old_name] <- construction_mapping[old_name]
}

p <- ggplot(plot_df, aes(x = layer, y = factor(pos), fill = mean_odds_ratio)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "magenta4") +
  facet_grid(construction_label ~ animacy_condition, scales = "free_y", space = "free_y") +
  scale_y_discrete(
    breaks = c("2", "4", "5", "6", "7", "8", "9", "10"),
    labels = c("{filler}", "the_1", "{np1}", "{verb1}", "that", "the_2", "{np2}", "{verb2}")
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Palatino", face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(color = "black", size = 1),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 270, hjust = 0.5)#, size = 7)
  ) +
  labs(
    title = "Generalization Across Constructions",
    x = "Layer",
    y = "Position",
    fill = "Odds Ratio"
  )
print(p)
ggsave(filename = paste0("plots/stacked_constructions_all_embed_", model_name,".pdf"), 
       plot = p, 
       width = 10.5, 
       device = 'pdf',
       height = 12)

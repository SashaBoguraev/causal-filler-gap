library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(arrow) 
library(extrafont)
library(rstatix)  
library(tidyr)

generalization_df_classic <- read_parquet("../results/generalization/classic.parquet")
generalization_df_classic <- generalization_df_classic[generalization_df_classic$seed == 41 &
                                                       generalization_df_classic$model == "pythia-1.4b", ]

generalization_df_unique_embed <- generalization_df_classic %>%
  distinct() %>%
  filter(grepl("embedded_clause", from))

# control_df <- read_parquet("results/generalization/classic_lexical_controls.parquet")
# control_df <- control_df %>%
#   filter(pos != 3,
#          to %in% c("embedded_control_lexical_animate", "embedded_control_lexical_inanimate"),
#          seed == 41,
#          model == "pythia-1.4b")

gen_df <- rbind(generalization_df_unique_embed)
gen_df$to <- gsub("embedded_control_lexical_(animate|inanimate)", "embedded_control_lexical", gen_df$to)

max_odds_df_classic <- gen_df %>%
  group_by(layer, pos, from, to, model, seed, leave_out, single_double) %>%
  summarize(mean_odds_ratio = mean(odds_ratio, na.rm = TRUE), 
            sd_avg = sd(odds_ratio, na.rm = TRUE),
            median_avg = median(mean_odds_ratio, na.rm = TRUE),
            n = n(), .groups = "drop")%>%
  mutate(df_source = "classic")

generalization_df_sd <- read_parquet("../results/generalization/single_double.parquet")
generalization_df_sd <- generalization_df_sd[generalization_df_sd$seed == 41 &
                                               generalization_df_sd$model == "pythia-1.4b", ]

generalization_df_sd$to <- gsub("embedded_control_lexical_(animate|inanimate)", "embedded_control_lexical", generalization_df_sd$to)


max_odds_df_sd <- generalization_df_sd %>%
  group_by(layer, pos, from, to, model, seed, leave_out, single_double) %>%
  summarize(mean_odds_ratio = mean(odds_ratio, na.rm = TRUE), 
            sd_avg = sd(odds_ratio, na.rm = TRUE),
            median_avg = median(mean_odds_ratio, na.rm = TRUE),
            n = n(), .groups = "drop")%>%
  mutate(df_source = "sd")


odds_df <- bind_rows(max_odds_df_classic, max_odds_df_sd)
odds_df$from <- gsub("_embedded_clause", "", odds_df$from)
odds_df$to <- gsub("_embedded_clause", "", odds_df$to)

# Add animacy indicators
odds_df$animate_from <- !grepl("inanimate", odds_df$from)
odds_df$animate_to <- !grepl("inanimate", odds_df$to)
odds_df$parent_construction_from <- with(odds_df, 
                                         gsub("_animate", "", gsub("_inanimate", "", from))
)
odds_df$parent_construction_to <- with(odds_df, 
                                       gsub("_animate", "", gsub("_inanimate", "", to))
)

# Create more detailed animacy condition with source type
odds_df$animacy_condition <- with(odds_df, ifelse(
  to == "embedded_control",
  "Control",
  ifelse(
    to == "embedded_control_lexical",
    "Lexical Control",
    paste0(
      ifelse(df_source == "sd", "Single→", "Embedded→"),
      ifelse(parent_construction_from == parent_construction_to, "Left Out", "In Train Set"), ", \n",
      ifelse(animate_from == animate_to, "Same Animacy", "Different Animacy")
    )
  )
)
)

odds_df_clone <- odds_df[odds_df$animacy_condition == "Single→Left Out, \nSame Animacy" |
                     odds_df$animacy_condition == "Embedded→Left Out, \nSame Animacy" | 
                       odds_df$animacy_condition == "Control" |
                       odds_df$animacy_condition == "Lexical Control",]

# # Modified plot_mech function to handle the new animacy condition categories
# plot_mech <- function(val_df, from_const){
#   df <- val_df[val_df$parent_construction_from == from_const, ]
#   df <- df[df$pos %in% c(2, 4, 5, 6, 7, 8, 9, 10), ]
#   df$pos <- as.numeric(as.character(df$pos))
#   df$pos <- factor(df$pos, levels = rev(c(2, 4, 5, 6, 7, 8, 9, 10)))
#   
#   df$animacy_condition <- recode(df$animacy_condition,
#                                  "Embedded→Left Out, \nSame Animacy" = "Embedded→Embedded",
#                                  "Single→Left Out, \nSame Animacy" = "Single→Embedded"
#   )
#   condition_order <- c("Embedded→Embedded", "Single→Embedded", "Control", "Lexical Control")
#   df$animacy_condition <- factor(df$animacy_condition, levels = condition_order)
#   
#   p <- ggplot(df, aes(x = layer, y = factor(pos), fill = mean_odds_ratio)) +
#     geom_tile() +
#     scale_fill_gradient(low = "white", high = "magenta4") +
#     facet_grid(.~as.factor(animacy_condition)) +  # Keep facets on y-axis
#     scale_y_discrete(
#       breaks = c("2", "4", "5", "6", "7", "8", "9", "10"),
#       labels = c("{filler}", "the", "{np}", "{verb}",  "that", "the", "{np}", "{verb}"),
#     ) +
#     theme_minimal() +  # Changed from theme_bw() to remove the grid
#     theme(
#       text = element_text(family="Palatino", face="bold", size=12),
#       axis.text.x = element_blank(),
#       axis.text.y = element_text(size = 8),
#       legend.position = "right",
#       plot.title = element_text(hjust = 0.5, size = 18),
#       plot.background = element_blank(),
#       panel.border = element_rect(color = "black", fill = NA, size = 1),
#       panel.grid.major = element_blank(),  # Explicitly remove major grid lines
#       panel.grid.minor = element_blank(),  # Explicitly remove minor grid lines
#       strip.background = element_rect(color = "black", size = 1),
#       strip.placement = "outside",
#       strip.text.y = element_text(angle = 0, hjust = 0.5)  # Horizontal text for y strips
#     ) +
#     labs(
#       title = element_blank(),
#       x = element_blank(),
#       y = element_blank(),
#       fill ="Odds"
#     )
#   
#   save_path <- paste0("plots/generalization/", from_const, "_sd.pdf")
#   ggsave(plot = p, filename = save_path, width = 14, height = 1.5, device = 'pdf')
#   return(p)
# }
# 
# # Create plots for each construction
# sources = unique(odds_df$parent_construction_from)
# for (source in sources){
#   plot_mech(odds_df_clone, source)
# }

# Create stacked plot with all constructions
# Filter data for the positions we want to visualize
plot_df <- odds_df_clone[odds_df_clone$pos %in% c(2, 4, 5, 6, 7, 8, 9, 10), ]
plot_df$pos <- as.numeric(as.character(plot_df$pos))
plot_df$pos <- factor(plot_df$pos, levels = rev(c(2, 4, 5, 6, 7, 8, 9, 10)))

plot_df$animacy_condition <- recode(plot_df$animacy_condition,
                               "Embedded→Left Out, \nSame Animacy" = "Embedded→Embedded",
                               "Single→Left Out, \nSame Animacy" = "Single→Embedded"
)
condition_order <- c("Embedded→Embedded", "Single→Embedded", "Control", "Lexical Control")
plot_df$animacy_condition <- factor(plot_df$animacy_condition, levels = condition_order)

# Create more descriptive construction labels
plot_df$construction_label <- plot_df$parent_construction_from
# Customize construction name mapping
construction_mapping <- c(
  "cleft" = "Cleft",
  "embedded_wh_finite" = "Embedded \nWh-Question \n(Know-Class)",
  "embedded_wh_nonfinite" = "Embedded \nWh-Question \n(Wonder-Class)",
  "pseudo_cleft" = "Pseudocleft",
  "restricted_rc" = "Restricted \nRelative Clause",
  "topicalization" = "Topicalization",
  "wh_question" = "Matrix \nWh-Question"
)
# Apply the mapping where available
for (old_name in names(construction_mapping)) {
  plot_df$construction_label[plot_df$parent_construction_from == old_name] <- construction_mapping[old_name]
}

# Create the stacked plot
p <- ggplot(plot_df, aes(x = layer, y = factor(pos), fill = mean_odds_ratio)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "magenta4") +
  facet_grid(construction_label ~ animacy_condition, scales = "free_y", space = "free_y") +
  scale_y_discrete(
    breaks = c("2", "4", "5", "6", "7", "8", "9", "10"),
    labels = c("{filler}", "the", "{np}", "{verb}",  "that", "the", "{np}", "{verb}"),
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Palatino", face = "bold", size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
    axis.text.y = element_text(size = 6),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(color = "black", size = 1),
    strip.placement = "outside",
    strip.text.y = element_text(angle = 270, hjust = 0.5, size = 7)
  ) +
  labs(
    title = "Generalization Across Constructions",
    x = "Layer",
    y = "Position",
    fill = "Odds Ratio"
  )

# Display the plot
print(p)

# Save the stacked plot
ggsave(filename = "plots/stacked_constructions_sd.pdf", 
       plot = p, 
       width = 10, 
       device = 'pdf',
       height = 1 * length(unique(plot_df$construction_label)),
       dpi = 10000)

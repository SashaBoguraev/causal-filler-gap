library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(arrow) 
library(extrafont)
library(rstatix)  
library(tidyr)
library(igraph)
library(viridis)

generalization_df_classic <- read_parquet("../results/generalization/classic.parquet")
generalization_df_classic <- generalization_df_classic[generalization_df_classic$seed == 41 &
                                                       generalization_df_classic$model == "pythia-1.4b", ]

generalization_df_classic$`__index_level_0__`=NULL

gen_df <- generalization_df_classic
gen_df$to <- gsub("control_lexical_(animate|inanimate)", "control_lexical", gen_df$to)
gen_df$from <- gsub("control_lexical_(animate|inanimate)", "control_lexical", gen_df$from)

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


# Add animacy indicators
max_odds_df$animate_from <- !grepl("inanimate", max_odds_df$from)
max_odds_df$animate_to <- !grepl("inanimate", max_odds_df$to)
max_odds_df$parent_construction_from <- with(max_odds_df, 
                                             gsub("_animate", "", gsub("_inanimate", "", from))
)
max_odds_df$parent_construction_to <- with(max_odds_df, 
                                           gsub("_animate", "", gsub("_inanimate", "", to))
)

max_odds_single <- max_odds_df %>% 
  filter(!grepl("_embedded_clause", to)) %>% 
  filter(!grepl("_embedded_clause", from)) %>% 
  filter(!grepl("embedded_control", to)) %>%
  filter(pos==4)

g <- graph_from_data_frame(max_odds_single[, c("from", "to", "max_avg")], directed = TRUE)
g <- delete_edges(g, which(E(g)$max_avg < 1))

# Step 2: Clean node labels (remove _animate/_inanimate for display purposes)
V(g)$label <- gsub("_animate", "\nAnimate", V(g)$name)
V(g)$label <- gsub("_inanimate", "\nInanimate", V(g)$label)
V(g)$label <- gsub("embedded_wh_nonfinite", "Emb. \nWh-Q Wonder", V(g)$label)
V(g)$label <- gsub("embedded_wh_finite", "Emb. \nWh-Q Know", V(g)$label)
V(g)$label <- gsub("cleft", "Cleft", V(g)$label)
V(g)$label <- gsub("pseudo_Cleft", "Pseudocleft", V(g)$label)
V(g)$label <- gsub("wh_question", "Matrix\nWh-question", V(g)$label)
V(g)$label <- gsub("topicalization", "Topicalization", V(g)$label)
V(g)$label <- gsub("restricted_rc", "Restricted\nRelative Clause", V(g)$label)
V(g)$label <- gsub("control_lexical", "Lexical\nControl", V(g)$label)
V(g)$label <- gsub("control", "Control", V(g)$label)
V(g)$animacy <- gsub("\nAnimate|\nInanimate", "", V(g)$label)

# Step 3: Add animacy attribute to vertices
V(g)$animate <- !grepl("Inanimate", V(g)$label)

construction_types <- unique(V(g)$animacy)
palette <- RColorBrewer::brewer.pal(max(3, min(length(construction_types), 9)), "Set3")
construction_colors <- setNames(palette, construction_types)

V(g)$color <- construction_colors[V(g)$animacy]

# Step 5: In-strength-based sizing
V(g)$in_strength <- strength(g, mode = "in", weights = E(g)$max_avg)
vertex_sizes <- V(g)$in_strength

# Step 6: Edge width and color
edge_widths <- E(g)$max_avg
edge_palette_func <- colorRampPalette(c("grey", "magenta4"))
edge_colors <- edge_palette_func(100)
scaled_weights <- scales::rescale(E(g)$max_avg, to = c(1, 100))
edge_color_mapping <- edge_colors[round(scaled_weights)]

# Step 7: Loop angles
if (!"loop_angle" %in% edge_attr_names(g)) {
  E(g)$loop_angle <- 0
}

layout <- layout_in_circle(g)
edge_list <- as_edgelist(g)
is_loop <- which(edge_list[, 1] == edge_list[, 2])

if (length(is_loop) > 0) {
  for (i in is_loop) {
    node_name <- edge_list[i, 1]
    node_id <- which(V(g)$name == node_name)
    node_pos <- layout[node_id, ]
    angle <- atan2(node_pos[2], node_pos[1])
    E(g)$loop_angle[i] <- -angle 
  }
}

# Step 8: Plot with cleaned labels and color-coded animacy
arrow_scale_factor <- 0.5


edge_order <- order(E(g)$max_avg, decreasing = TRUE)
g <- set.edge.attribute(g, "plot_order", value = NA)
E(g)$plot_order <- seq_along(E(g))

legend_breaks <- pretty(range(E(g)$max_avg), n = 5)
legend_colors <- edge_palette_func(100)[round(scales::rescale(legend_breaks, to = c(1, 100)))]

pdf("plots/generalization_network.pdf", width = 8, height = 8)
par(mar = c(1, 0, 1, 0))

plot(
  g,
  layout = layout,
  edge.width = (edge_widths / max(edge_widths)) * 10,
  edge.arrow.width = (edge_widths / max(edge_widths)) * 2,
  edge.arrow.size = (edge_widths / max(edge_widths)),
  vertex.size = vertex_sizes ,
  vertex.color = V(g)$color,
  vertex.frame.color = "gray",
  vertex.label = V(g)$label,
  vertex.label.color = "black",
  vertex.label.font = 2,
  edge.loop.angle = E(g)$loop_angle,
  edge.color = edge_color_mapping,
  cex.main = 5,  
  # main = paste(pos_labels[pos_val]),
  margin = 0.2,
  edge.curved = 0.0
)

dev.off()

# List of POS values you want to plot
pos_values <- c(2, 4, 5, 6)
pos_labels = c("{prefix}", "{filler}", "{nc}", "{the}", "{np}", "{verb}")

# Start a PDF for plotting subplots
pdf("plots/generalization_network_subplots.pdf", width = 28, height = 7)
par(mfrow = c(1, 4), mar = c(1, 0, 2, 0))  # 2x2 grid of plots

for (pos_val in pos_values) {
  
  # Filter and create subgraph
  subset_df <- max_odds_df %>%
    filter(!grepl("_embedded_clause", to)) %>%
    filter(!grepl("_embedded_clause", from)) %>%
    filter(!grepl("embedded_control", to)) %>%
    filter(pos == pos_val)
  
  g <- graph_from_data_frame(subset_df[, c("from", "to", "max_avg")], directed = TRUE)
  g <- delete_edges(g, which(E(g)$max_avg < 10))
  
  # Clean labels
  V(g)$label <- gsub("_animate", "\nAnimate", V(g)$name)
  V(g)$label <- gsub("_inanimate", "\nInanimate", V(g)$label)
  V(g)$label <- gsub("embedded_wh_nonfinite", "Emb. \nWh-Q Wonder", V(g)$label)
  V(g)$label <- gsub("embedded_wh_finite", "Emb. \nWh-Q Know", V(g)$label)
  V(g)$label <- gsub("cleft", "Cleft", V(g)$label)
  V(g)$label <- gsub("pseudo_Cleft", "Pseudocleft", V(g)$label)
  V(g)$label <- gsub("wh_question", "Matrix\nWh-question", V(g)$label)
  V(g)$label <- gsub("topicalization", "Topicalization", V(g)$label)
  V(g)$label <- gsub("restricted_rc", "Restricted\nRelative Clause", V(g)$label)
  V(g)$label <- gsub("control_lexical", "Lexical\nControl", V(g)$label)
  V(g)$label <- gsub("control", "Control", V(g)$label)
  V(g)$animacy <- gsub("\nAnimate|\nInanimate", "", V(g)$label)
  
  V(g)$animate <- !grepl("Inanimate", V(g)$label)
  construction_types <- unique(V(g)$animacy)
  palette <- RColorBrewer::brewer.pal(max(3, min(length(construction_types), 9)), "Set3")
  construction_colors <- setNames(palette, construction_types)
  V(g)$color <- construction_colors[V(g)$animacy]
  
  # Vertex sizing and edge styling
  V(g)$in_strength <- strength(g, mode = "in", weights = E(g)$max_avg)
  vertex_sizes <- V(g)$in_strength
  
  edge_widths <- E(g)$max_avg
  edge_palette_func <- colorRampPalette(c("grey", "magenta4"))
  edge_colors <- edge_palette_func(100)
  scaled_weights <- scales::rescale(E(g)$max_avg, to = c(1, 100))
  edge_color_mapping <- edge_colors[round(scaled_weights)]
  
  if (!"loop_angle" %in% edge_attr_names(g)) {
    E(g)$loop_angle <- 0
  }
  
  layout <- layout_in_circle(g)
  edge_list <- as_edgelist(g)
  is_loop <- which(edge_list[, 1] == edge_list[, 2])
  if (length(is_loop) > 0) {
    for (i in is_loop) {
      node_name <- edge_list[i, 1]
      node_id <- which(V(g)$name == node_name)
      node_pos <- layout[node_id, ]
      angle <- atan2(node_pos[2], node_pos[1])
      E(g)$loop_angle[i] <- -angle 
    }
  }
  
  # Plot
  plot(
    g,
    layout = layout,
    edge.width = (edge_widths / max(edge_widths)) * 10,
    edge.arrow.width = (edge_widths / max(edge_widths)) * 2,
    edge.arrow.size = (edge_widths / max(edge_widths)),
    vertex.size = vertex_sizes ,
    vertex.color = V(g)$color,
    vertex.frame.color = "gray",
    vertex.label = V(g)$label,
    vertex.label.color = "black",
    vertex.label.font = 2,
    edge.loop.angle = E(g)$loop_angle,
    edge.color = edge_color_mapping,
    cex.main = 5,  
    # main = paste(pos_labels[pos_val]),
    margin = 0.2,
    edge.curved = 0.0
  )
  
  title(paste(pos_labels[pos_val]), cex.main = 2.5, font.main = 2)
}

dev.off()


library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(tidyr)
library(extrafont)
library(ggsignif)
library(reshape2)
library(arrow)

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

max_odds_df$animacy_condition <- with(max_odds_df, ifelse(
  to == "control",
  "control",
  ifelse(
    to == "control_lexical",
    "control_lexical",
    paste0(
      ifelse(parent_construction_from == parent_construction_to, "SameDataset", "DiffDataset"), "_",
      ifelse(animate_from == animate_to, "SameAnimacy", "DiffAnimacy")
    )
  )
)
)


# ========= NORMALIZED ==========
max_odds_df$normal <- NA


unique_pos <- unique(max_odds_df$pos)
unique_from <- unique(max_odds_df$from)


for (pos_i in unique_pos) {
  
  for (from_val in unique_from) {
    
    
    from_data <- max_odds_df[max_odds_df$from == from_val 
                                        & max_odds_df$pos == pos_i, ]
    
    row_index_from <- which(from_data$to == from_val)
    
    if(length(row_index_from) > 0) {
      normal <- from_data$max_avg[row_index_from]
      
      for (row_i in 1:nrow(from_data)) {
        
        current_val <- from_data$max_avg[row_i]
        max_odds_df$normal[max_odds_df$from == from_val 
                                      & max_odds_df$pos == pos_i 
                                      & max_odds_df$to == from_data$to[row_i]] <- current_val / normal
      }
    } else {
      warning(paste("No row found for 'from' value:", from_val, "and 'pos' value:", pos_i))
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
  "control",
  "control_lexical",
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

max_odds_df$from <- factor(max_odds_df$from, levels = from_order)
max_odds_df$to <- factor(max_odds_df$to, levels = to_order)

pos_labels <- c(
  "2" = "{filler}",
  "4" = "the",
  "5" = "{np}",
  "6" = "{verb}"
)

max_odds_df$pos <- factor(max_odds_df$pos, levels = c(2, 4, 5, 6), labels = pos_labels)
write_parquet(max_odds_df, paste0("../results/generalization/single_clause_single_construction_classic_", model_size,".parquet"))

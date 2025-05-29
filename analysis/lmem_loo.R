library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(arrow) 
library(extrafont)
library(rstatix)  
library(tidyr)
library(lme4)
library(optimx)

model_size = "1,4b"
model_name = paste0("pythia-", model_size)

generalization_df <- read_parquet("../results/generalization/leave_out.parquet")
generalization_df <- generalization_df[generalization_df$seed == 41 &
                                       generalization_df$model == model_name, ]

generalization_df_unique_single <- generalization_df %>% # GET JUST SINGLE CLAUSE
  filter(!grepl("embedded_clause", from))%>%
  distinct()

generalization_df_unique_single$`__index_level_0__`=NULL

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

max_odds_df = max_odds_df[max_odds_df$to != "control" & max_odds_df$to != "control_lexical",]

# Add animacy indicators
max_odds_df$animate_from <- !grepl("inanimate", max_odds_df$from)
max_odds_df$animate_to <- !grepl("inanimate", max_odds_df$to)
max_odds_df$parent_construction_from <- with(max_odds_df, 
                                             gsub("_animate", "", gsub("_inanimate", "", from))
)
max_odds_df$parent_construction_to <- with(max_odds_df, 
                                           gsub("_animate", "", gsub("_inanimate", "", to))
)

max_odds_df$in_train_set <- with(max_odds_df, 
                                 ifelse(parent_construction_from != parent_construction_to, 1, -1)
)

max_odds_df$same_animacy <- with(max_odds_df,
                                 ifelse(animate_from == animate_to, 1, -1)
                                 )


max_odds_df$to_filler_class <- with(max_odds_df,
                                    ifelse(parent_construction_to == "cleft",
                                           "null",
                                           ifelse(parent_construction_to == "topicalization",
                                                  "phrasal",
                                                  "wh")
                                    )
)

max_odds_df$to_embeds_cp <- with(max_odds_df,
                                    ifelse(parent_construction_to == "cleft",
                                           1, -1)
)

max_odds_df$to_inversion <- with(max_odds_df,
                                    ifelse(parent_construction_to == "cleft" | parent_construction_to == "wh_question",
                                           1, -1
                                    )
)

max_odds_df$to_embedded_under <- with(max_odds_df,
                                      ifelse(parent_construction_to == "wh_question",
                                             "na",
                                             ifelse(parent_construction_to == "cleft",
                                                    "aux",
                                                    ifelse(parent_construction_to == "restricted_rc",
                                                           "np",
                                                           ifelse(parent_construction_to == "embedded_wh_nonfinite" | parent_construction_to == "embedded_wh_finite",
                                                                  "comp",
                                                                  "verb")
                                                    )
                                             )
                                      )
)


max_odds_df$to_discourse_fronted <- with(max_odds_df,
                                           ifelse(parent_construction_to == "cleft" | parent_construction_to == "pseudo_cleft" | parent_construction_to =="topicalization",
                                                  1, -1
                                           )
)

max_odds_df$to_frequency <- with(max_odds_df, 
                                 ifelse(parent_construction_to == "restricted_rc", 
                                        504,
                                        ifelse(parent_construction_to == "embedded_wh_finite" | parent_construction_to == "embedded_wh_nonfinite", 
                                               308,
                                               ifelse(parent_construction_to == "wh_question", 
                                                      82,
                                                      ifelse(parent_construction_to == "cleft", 
                                                             20,
                                                             ifelse(parent_construction_to == "pseudo_cleft", 
                                                                    6,
                                                                    ifelse(parent_construction_to == "topicalization", 
                                                                           6,
                                                                           NA)
                                                             )
                                                      )
                                               )
                                        )
                                 )
)

max_odds_df$to_frequency_small <- with(max_odds_df, 
                                       ifelse(to_frequency<100, 1, -1))

max_odds_df$pos <- factor(max_odds_df$pos)


model_main <- lmerTest::lmer(max_avg ~ 
                              (1 + in_train_set * same_animacy | from) +
                              (1 + in_train_set * same_animacy | to ) +
                              in_train_set * same_animacy ,
                              data = filter(max_odds_df, pos == 6), 
                              REML=TRUE, control=lmerControl(optimizer="bobyqa"))
summary(model_main)

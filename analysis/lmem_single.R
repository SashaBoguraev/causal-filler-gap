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

model_size = "1.4b"
model_name = paste0("pythia-", model_size)

generalization_df_classic <- read_parquet("../results/generalization/classic.parquet")
generalization_df_classic <- generalization_df_classic[generalization_df_classic$seed == 41 &
                                                       generalization_df_classic$model == model_name, ]

gen_df <- generalization_df_classic
gen_df <- gen_df[
                   !grepl("control", gen_df$to, ignore.case = TRUE) &
                   !grepl("control", gen_df$from, ignore.case = TRUE) &
                   !grepl("_embedded_clause", gen_df$from, ignore.case = TRUE)& 
                   !grepl("_embedded_clause", gen_df$to, ignore.case = TRUE), 
                 ]

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

max_odds_df$in_train_set <- with(max_odds_df, 
                                 ifelse(parent_construction_from == parent_construction_to, 1, -1)
)

max_odds_df$same_animacy <- with(max_odds_df,
                                 ifelse(animate_from == animate_to, 1, -1)
                                 )

max_odds_df$to_filler_class <- with(max_odds_df,
                                    ifelse(grepl("control", parent_construction_to), "NA",
                                    ifelse(parent_construction_to == "cleft",
                                           "null",
                                           ifelse(parent_construction_to == "topicalization",
                                                  "phrasal",
                                                  "wh")
                                    )
))

# Create matching "from" filler class variables
max_odds_df$from_filler_class <- with(max_odds_df,
                                      ifelse(grepl("control", parent_construction_to), "NA",
                                      ifelse(parent_construction_from == "cleft",
                                             "null",
                                             ifelse(parent_construction_from == "topicalization",
                                                    "phrasal",
                                                    "wh")
                                      )
))

# Add match indicator for filler class
max_odds_df$match_filler_class <- with(max_odds_df,
                                       ifelse(from_filler_class == to_filler_class, 1, -1)
)

# "to" inversion variable
max_odds_df$to_is_fg <- with(max_odds_df,
                                 ifelse(!grepl("control", parent_construction_to),
                                        1, -1
                                 ))

# "to" inversion variable
max_odds_df$from_is_fg <- with(max_odds_df,
                             ifelse(!grepl("control", parent_construction_from),
                                    1, -1
                             ))

# Add match indicator for inversion
max_odds_df$match_inversion <- with(max_odds_df,
                                    ifelse(max_odds_df$from_is_fg == max_odds_df$to_is_fg, 
                                           1, -1))

# "to" inversion variable
max_odds_df$to_inversion <- with(max_odds_df,
                                 ifelse(parent_construction_to == "wh_question",
                                        1, -1
                                 ))

# Create matching "from" inversion variable
max_odds_df$from_inversion <- with(max_odds_df,
                                   ifelse(parent_construction_from == "wh_question",
                                          1, -1
                                   )
)

# Add match indicator for inversion
max_odds_df$match_inversion <- with(max_odds_df,
                                      ifelse(max_odds_df$from_inversion == max_odds_df$to_inversion, 
                                             1, -1))

# "to" embedded under variable
max_odds_df$to_embedded_under <- with(max_odds_df,
                                      ifelse(parent_construction_to == "restricted_rc",
                                                           "np", ifelse(parent_construction_to == "cleft" |parent_construction_to == "embedded_wh_nonfinite" | parent_construction_to == "embedded_wh_finite",
                                                                                      "vp", "na")))

# Create matching "from" embedded under variable
max_odds_df$from_embedded_under <- with(max_odds_df,
                                        ifelse(parent_construction_from == "restricted_rc",
                                               "np", ifelse(parent_construction_from == "cleft" | parent_construction_from == "embedded_wh_nonfinite" | parent_construction_from == "embedded_wh_finite",
                                                                          "vp", "na")))


# Add match indicator for embedded under
max_odds_df$match_embedded_under <- with(max_odds_df,
                                         ifelse(from_embedded_under == to_embedded_under,
                                                1, -1
                                         ))

max_odds_df$to_discourse_fronted <- with(max_odds_df,
                                         ifelse(parent_construction_to == "cleft" | parent_construction_to == "pseudo_cleft" | parent_construction_to == "topicalization",
                                                1, -1
                                         )
)

# Create matching "from" discourse fronted variable
max_odds_df$from_discourse_fronted <- with(max_odds_df,
                                           ifelse(parent_construction_from == "cleft" | parent_construction_from == "pseudo_cleft" | parent_construction_from == "topicalization",
                                                  1, -1
                                           )
)

# Add match indicator for discourse fronted
max_odds_df$match_discourse_fronted <- with(max_odds_df,
                                            ifelse(max_odds_df$from_discourse_fronted == max_odds_df$to_discourse_fronted, 
                                                   1, -1))
max_odds_df$pos <- factor(max_odds_df$pos)


model_with_matches <- lmerTest::lmer(max_avg ~ match_filler_class + match_inversion + match_discourse_fronted + match_embedded_under + 
                                       (1 + match_filler_class + match_inversion + match_discourse_fronted + match_embedded_under || from) + 
                                       (1 + match_filler_class + match_inversion + match_discourse_fronted + match_embedded_under || to),
                                     data = filter(max_odds_df, pos==2))

model_with_matches <- lmerTest::lmer(max_avg ~ match_filler_class + match_inversion + match_discourse_fronted + match_embedded_under + 
                                       (1 + match_filler_class + match_inversion + match_discourse_fronted + match_embedded_under || from) + 
                                       (1 + match_filler_class + match_inversion + match_discourse_fronted + match_embedded_under || to),
                                     data = filter(max_odds_df, pos==4))

model_with_matches <- lmerTest::lmer(max_avg ~ match_filler_class + match_inversion + match_discourse_fronted + match_embedded_under + 
                                       (1 + match_filler_class + match_inversion + match_discourse_fronted + match_embedded_under || from) + 
                                       (1 + match_filler_class + match_inversion + match_discourse_fronted + match_embedded_under || to),
                                     data = filter(max_odds_df, pos==5))

model_with_matches <- lmerTest::lmer(max_avg ~ match_filler_class + match_inversion + match_discourse_fronted + match_embedded_under + 
                                       (1 + match_filler_class + match_inversion + match_discourse_fronted + match_embedded_under || from) + 
                                       (1 + match_filler_class + match_inversion + match_discourse_fronted + match_embedded_under || to),
                                     data = filter(max_odds_df, pos==6))

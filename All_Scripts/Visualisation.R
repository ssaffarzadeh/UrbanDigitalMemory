# install.packages(c("gghighlight", "tidyverse", "dplyr", "ggplot2")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(gghighlight)

topics <- read.csv("output/Py_results/plot_topics_english.csv")
tweets <- read.csv("output/full_data_translated_keyprob_sentiments.csv")

# 2. Define our Spatial/Urban Topic IDs

spatial_ids <- c(0, 1, 2, 4, 5, 11, 12, 13, 16, 18, 19, 21, 22, 30,
                 31, 32, 37, 41, 46, 52, 55, 76, 82, 84, 86, 88, 93)

spatial_ids_explicit <- c(0, 2, 5, 11, 13, 16, 18, 19, 30, 31, 32, 37, 86)

spatialtopic_names <- c( "0" = "Tehran / Valiasr Street Life & Nostalgia",
                         "1" = "Neighborhood Hubs & Squares",
                         "2" = "Grand Bazaar & Market Shopping",
                         "4" = "Toopkhaneh Square Naming & War",
                         "5" = "City Theater Metro & Underpass",
                         "11" = "Metro Infrastructure & Tajrish Station",
                         "12" = "Avenues, Gardens & Boulevards",
                         "13" = "Performing Arts & Festivals (City Theater)",
                         "16" = "Valiasr Street Connectivity & Length",
                         "18" = "Valiasr / Tajrish Tree Preservation & Urban Ecology",
                         "19" = "Urban Management & Heritage Protection of City Theater",
                         "21" = "Nostalgic Atmospheric Streets & Alleys",
                         "22" = "Urban Identity and Perception",
                         "30" = "Public Monuments",
                         "31" = "Metro Station Entrances",
                         "32" = "Seasonal Walks (Autumn in Valiasr)",
                         "37" = "Emotional Attachment to Tajrish",
                         "41" = "Emotional Vibe of Toopkhaneh",
                         "46" = "Hassan Abad Heritage Houses",
                         "52" = "Historical Renaming of Squares",
                         "55" = "Historical Squares (Ashura)",
                         "76" = "Contemporary Urban History",
                         "82" = "Cafe Culture & Solitude",
                         "84" = "Homa Self-Immolation in Tajrish",
                         "86" = "Tajrish-Valiasr Walks",
                         "88" = "Tehran's Historical Gates",
                         "93" = "Tehran Cafes")


spatial_columns <- c('prob_topic_0', 'prob_topic_1', 'prob_topic_2', 'prob_topic_4',
                    'prob_topic_5', 'prob_topic_11', 'prob_topic_12', 'prob_topic_13',
                    'prob_topic_16', 'prob_topic_18', 'prob_topic_19', 'prob_topic_21',
                    'prob_topic_22', 'prob_topic_22', 'prob_topic_30',
                    'prob_topic_31','prob_topic_32', 'prob_topic_37', 'prob_topic_41',
                    'prob_topic_46', 'prob_topic_52', 'prob_topic_55', 'prob_topic_76',
                     'prob_topic_82', 'prob_topic_84', 'prob_topic_86', 'prob_topic_88',
                    'prob_topic_93')

# 3. Filter and Prepare
spatial_topics <- topics |>
  filter(Topic %in% spatial_ids)

# exporting the classified topics to create wordclouds
topics_for_cloud <- spatial_topics|>
  mutate(eng_name = spatialtopic_names[as.character(Topic)])|>
  mutate(explicitness = case_when(Topic %in% spatial_ids_explicit ~ "Explicit",
                               TRUE ~ "Non-explicit"))|>
  select(Topic, eng_name, explicitness, Score, Count, English_Word, 
         English_Representation)

write.csv(topics_for_cloud , "output/Py_results/plot_topics_english.csv")



counttopics <- topics_for_cloud |>
  select(eng_name, Count, Topic)|>
  unique()


spatial_tweets <- tweets |>
  select(-X)|>
  filter(final_topic %in% spatial_ids)|>
  mutate(Topic = final_topic)|>
  left_join(counttopics, by = "Topic")|>
  mutate(eng_name = spatialtopic_names[as.character(Topic)]) |>
  mutate(explicitness = case_when(Topic %in% spatial_ids_explicit ~ "Explicit",
                               TRUE ~ "Non-explicit"))|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year != "2021")|>
  mutate(timeline = if_else(year < 2024, "Before_24", "After_24"))|>
  select(doc_id, text, txtenglish, Topic, eng_name, Count, 
         sentiment_label, sentiment_score, explicitness, type, year, timeline, createdAt,
         all_of(spatial_columns))

sum_tweets <- spatial_tweets|>
  filter(explicitness == "Explicit")|>
  str()

write.csv(spatial_tweets,"output/Py_results/mostfinal_analysis_output.csv")

spatial_cols <- all_of(spatial_columns)|>
  as.data.frame()

topic_probs <- spatial_tweets|>
  select(spatial_cols, doc_id)|>
  as.data.frame()


plot_data_ct24 <- spatial_tweets |>
  filter(explicitness == "Explicit") |>
  filter(type == "ct" & timeline == "Before_24") |>
  # 1. Standardize: Handle NAs or empty strings by naming them "Neutral"
  # mutate(sentiment_label = case_when(
  #   is.na(sentiment_label) ~ "Neutral",
  #   sentiment_label == "" ~ "Neutral",
  #   TRUE ~ sentiment_label
  # )) |>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  
  # 2. THE UNIVERSAL FILL:
  # This creates a row for ANY sentiment label that exists in the data 
  # but is missing for a specific topic.
  complete(sentiment_label, eng_name, fill = list(topic_count = 0)) |>
  
  # 3. ROW-WISE PERCENTAGE (Topic-based)
  group_by(eng_name) |> 
  mutate(
    topic_total = sum(topic_count),
    # If the topic has at least one tweet, calculate %; otherwise 0
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  # Sort topics by total volume so the chart is organized
  mutate(eng_name = reorder(eng_name, topic_total))

# CT 24 dark
ggplot(plot_data_ct24, aes(x = eng_name, y = topic_count,
                           fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c(
    "City Theater Metro & Underpass",
    "Tehran / Valiasr Street Life & Nostalgia",
    "Urban Management & Heritage Protection of City Theater",
    # "Valiasr Street Connectivity & Length",
    "Performing Arts & Festivals (City Theater)"
  )),
  show.legend = FALSE) +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.3),
                     guide = "none") +
  geom_text(aes(label = label_text), 
            hjust = -0.1, 
            size = 4.0,
            color = "black") + 
  facet_wrap(~sentiment_label, scales = "fixed") + 
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c(
    "neutral" = "#969696",
    "negative" = "#d9534f",
    "positive" = "#5cb85c"
  )) + 
  labs(title = "Localised Spatial Topic Frequency by Sentiments for City Theater 2022 - 2024",
       subtitle = "Tweet distributions per topic | Tweet Count: 266, Total: 582",
       ## nrows ct 24: 266 // Total ct: 582
       x = "Spatial Topics",
       y = "Number of Tweets (Absolute Count)") +
  theme(
    strip.text = element_text(size = 11, face = "bold", color = "black"),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "white"),
    panel.grid.major  = element_line(color = "gray80"),   
    panel.grid.minor  = element_line(color = "gray90"),
    ## axis
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(size = 14, hjust = 1, color = "black"),
    axis.text.x = element_text(size = 11, hjust = 0.9, color = "black"),
    axis.title = element_text(size = 12, color = "black", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "black"),
    plot.subtitle = element_text(color = "black")
  ) +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.25)))



plot_data_ct26 <- spatial_tweets |>
  filter(explicitness == "Explicit") |>
  filter(type == "ct" & timeline == "After_24") |>
  # mutate(sentiment_label = case_when(
  #   is.na(sentiment_label) ~ "Neutral",
  #   sentiment_label == "" ~ "Neutral",
  #   TRUE ~ sentiment_label
  # )) |>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  complete(sentiment_label, eng_name, fill = list(topic_count = 0)) |>
  group_by(eng_name) |> 
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_total))


# CT 26
ggplot(plot_data_ct26, aes(x = eng_name, y = topic_count,
                           fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c(
    "City Theater Metro & Underpass",
    "Tehran / Valiasr Street Life & Nostalgia",
    "Urban Management & Heritage Protection of City Theater",
    # "Valiasr Street Connectivity & Length",
    "Performing Arts & Festivals (City Theater)"
  )),
  show.legend = FALSE) +
  geom_text(aes(label = label_text), 
            hjust = -0.1, 
            size = 4.0,
            color = "black") + 
  facet_wrap(~sentiment_label, scales = "fixed") + 
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c(
    "neutral" = "#969696",
    "negative" = "#d9534f",
    "positive" = "#5cb85c"
  )) + 
  theme(
    strip.text = element_text(size = 11, face = "bold", color = "black"),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "white"),
    panel.grid.major  = element_line(color = "gray80"),   
    panel.grid.minor  = element_line(color = "gray90"),
    ## axis
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(size = 14, hjust = 1, color = "black"),
    axis.text.x = element_text(size = 11, hjust = 0.9, color = "black"),
    axis.title = element_text(size = 12, color = "black", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "black"),
    plot.subtitle = element_text(color = "black")
  ) +
  labs(title = "Localised Spatial Topic Frequency by Sentiments for City Theater 2024 - 2026",
       subtitle = "Tweet distributions per topic | Tweet Count: 316 | Total: 582",
       # nrows ct 26: 316 // Total ct: 582
       x = "Spatial Topics",
       y = "Number of Tweets (Absolute Count)") +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.3)))




### Tehran

plot_data_teh24 <- spatial_tweets |>
  filter(explicitness == "Explicit") |>
  filter(type == "not_ct" & timeline == "Before_24") |>
  # mutate(sentiment_label = case_when(
  #   is.na(sentiment_label) ~ "Neutral",
  #   sentiment_label == "" ~ "Neutral",
  #   TRUE ~ sentiment_label
  # )) |>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  complete(sentiment_label, eng_name, fill = list(topic_count = 0)) |>
  group_by(eng_name) |> 
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_total))

plot_data_teh26 <- spatial_tweets |>
  filter(explicitness == "Explicit") |>
  filter(type == "not_ct" & timeline == "After_24") |>
  # mutate(sentiment_label = case_when(
  #   sentiment_label == "" ~ "Neutral",
  #   TRUE ~ sentiment_label
  # )) |>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  complete(sentiment_label, eng_name, fill = list(topic_count = 0)) |>
  group_by(eng_name) |> 
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_total))




# Teh 24 dark
ggplot(plot_data_teh24, aes(x = eng_name, y = topic_count,
                            fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c(
    # "City Theater Metro & Underpass",
    "Tehran / Valiasr Street Life & Nostalgia",
    # "Urban Management & Heritage Protection of City Theater",
    "Valiasr Street Connectivity & Length",
    # "Performing Arts & Festivals (City Theater)"
    "Tehran/Valiasr Street Life & Nostalgia",
    "Emotional Attachment to Tajrish",
    "Seasonal Walks (Autumn in Valiasr)",
    "Public Monuments",
    "Valiasr / Tajrish Tree Preservation & Urban Ecology",
    "Grand Bazaar & Market Shopping",
    "Metro Infrastructure & Tajrish Station"
  )),
  show.legend = FALSE) +
  geom_text(aes(label = label_text), 
            hjust = -0.1, 
            size = 3.5,
            color = "white") + 
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25), 
                     guide = "none") +
  facet_wrap(~sentiment_label, scales = "fixed") + 
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c(
    "neutral" = "#969696",
    "negative" = "#d9534f",
    "positive" = "#5cb85c"
  )) + 
  labs(title = "Localised Spatial Topic Frequency by Sentiments for Tehran 2022 - 2024",
       subtitle = "Tweet distributions per topic | Tweet Count: 604 | Total: 1212",
       # nrows teh 24: 604 // Total ct: 1212 // nrows: teh 26: 
       x = "Spatial Topics",
       y = "Number of Tweets (Absolute Count)") +
  theme(
    strip.text = element_text(size = 9, face = "bold", color = "white"),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray50"),   
    panel.grid.minor  = element_line(color = "gray40"),
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 11, hjust = 1, color = "white"),
    axis.text.x = element_text(hjust = 0.9, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
  ) +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.3)))



# Plot Teh 26 dark
ggplot(plot_data_teh26, aes(x = eng_name, y = topic_count,
                            fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c(
    # "City Theater Metro & Underpass",
    "Tehran / Valiasr Street Life & Nostalgia",
    # "Urban Management & Heritage Protection of City Theater",
    "Valiasr Street Connectivity & Length",
    # "Performing Arts & Festivals (City Theater)"
    "Tehran/Valiasr Street Life & Nostalgia",
    "Emotional Attachment to Tajrish",
    "Seasonal Walks (Autumn in Valiasr)",
    "Public Monuments",
    "Valiasr / Tajrish Tree Preservation & Urban Ecology",
    "Grand Bazaar & Market Shopping",
    "Metro Infrastructure & Tajrish Station"
  )),
  show.legend = FALSE) +
  geom_text(aes(label = label_text), 
            hjust = -0.1, 
            size = 3.5,
            color = "white") +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25), 
                     guide = "none") +
  facet_wrap(~sentiment_label, scales = "fixed") + 
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c(
    "neutral" = "#969696",
    "negative" = "#d9534f",
    "positive" = "#5cb85c"
  )) +
  theme(
    strip.text = element_text(size = 9, face = "bold", color = "white"),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray50"),   
    panel.grid.minor  = element_line(color = "gray40"),
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 11, hjust = 1, color = "white"),
    axis.text.x = element_text(hjust = 0.9, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
  ) + 
  labs(title = "Localised Spatial Topic Frequency by Sentiments for Tehran 2024 - 2026",
       subtitle = "Tweet distributions per topic | Tweet Count: 608 | Total: 1212",
       # nrows teh 26: 608 // Total ct: 1212
       x = "Spatial Topics",
       y = "Number of Tweets (Absolute Count)") +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.25)))



# 1. Prepare the master data (No timeline filter)
plot_data_scale_ct <- spatial_tweets |>
  filter(type == "ct" & explicitness == "Explicit")|>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  complete(sentiment_label, eng_name, fill = list(topic_count = 0)) |>
  group_by(eng_name) |> 
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_total))

#######
## Testing the number of rows
number_tweets_per_scale <- spatial_tweets|>
  filter(type == "not_ct" & explicitness == "Explicit")
nrow(number_tweets_per_scale)
#######

plot_data_scale_teh <- spatial_tweets |>
  filter(type == "not_ct" & explicitness == "Explicit")|>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  complete(sentiment_label, eng_name, fill = list(topic_count = 0)) |>
  group_by(eng_name) |> 
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_total))

# Combined timelines CT
ggplot(plot_data_scale_ct, aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c(
    "City Theater Metro & Underpass",
    "Tehran / Valiasr Street Life & Nostalgia",
    "Urban Management & Heritage Protection of City Theater",
    "Performing Arts & Festivals (City Theater)"
  )), 
  show.legend = FALSE) +
  geom_text(aes(label = label_text), 
            hjust = -0.1, 
            size = 3.5,
            color = "white") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.25),
                     guide = "none") +
  facet_grid(~ sentiment_label, scales = "fixed") + 
  coord_flip() +
  theme_minimal() +
  # Custom colors (Red for Negative, Gray for Neutral, Green for Positive)
  scale_fill_manual(values = c(
    "neutral" = "#969696",
    "negative" = "#d9534f",
    "positive" = "#5cb85c"
  )) + 
  theme(
    strip.text = element_text(size = 9, face = "bold", color = "white"),
    strip.background = element_blank(),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray50"),   
    panel.grid.minor  = element_line(color = "gray40"),
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 11, hjust = 1, color = "white"),
    axis.text.x = element_text(hjust = 0.9, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
  ) + 
  labs(title = "Overall Localised Spatial Topics: City Theater Scale",
       subtitle = "Tweet Count: 582, Total: 1794 | Combined Timelines",
       ## nrows ct : 582 // nrows not-ct: 1212
       x = "Spatial Topics",
       y = "Number of Tweets (Absolute Count)") +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  theme(strip.text = element_text(face = "bold", size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3)))


# Combined timelins Teh
ggplot(plot_data_scale_teh, aes(x = eng_name, y = topic_count,
                                fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c(
    "Tehran / Valiasr Street Life & Nostalgia",
    "Grand Bazaar & Market Shopping",
    "Metro Infrastructure & Tajrish Station",
    "City Theater Metro & Underpass")), 
  show.legend = FALSE) +
  geom_text(aes(label = label_text), 
            hjust = -0.1, 
            size = 3.5,
            color = "white") +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25),
                     guide = "none") +
  facet_grid(~ sentiment_label, scales = "fixed") + 
  coord_flip() +
  # Custom colors (Red for Negative, Gray for Neutral, Green for Positive)
  scale_fill_manual(values = c(
    "neutral" = "#969696",
    "negative" = "#d9534f",
    "positive" = "#5cb85c"
  )) +
  theme(
    strip.text = element_text(size = 9, face = "bold", color = "white"),
    strip.background = element_blank(),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray50"),   
    panel.grid.minor  = element_line(color = "gray40"),
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 11, hjust = 1, color = "white"),
    axis.text.x = element_text(hjust = 0.9, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
  ) + 
  labs(title = "Overall Localised Spatial Topics: Tehran Scale",
       subtitle = "Tweet Count: 1212, Total: 1794 | Combined Timelines",
       ## nrows ct : 582 // nrows not-ct: 1212
       x = "Spatial Topics",
       y = "Number of Tweets (Absolute Count)") +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)))



# 1. Prepare the master data (No timeline filter)
plot_data_timeline <- spatial_tweets |>
  mutate(timeline = factor(timeline, 
                           levels = c("Before_24", "After_24")))|>
  filter(explicitness == "Explicit") |>
  mutate(sentiment_label = case_when(
    sentiment_label == "" ~ "Neutral",
    TRUE ~ sentiment_label
  )) |>
  group_by(sentiment_label, eng_name, timeline) |>
  summarise(topic_count = n(), .groups = "drop") |>
  complete(sentiment_label, eng_name, fill = list(topic_count = 0)) |>
  group_by(eng_name, timeline) |> 
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_total, sum))


# 1. Prepare the master data (No timeline filter)
plot_data_timeline_before <- spatial_tweets |>
  filter(explicitness == "Explicit" & timeline == "Before_24") |>
  mutate(sentiment_label = case_when(
    sentiment_label == "" ~ "Neutral",
    TRUE ~ sentiment_label
  )) |>
  group_by(sentiment_label, eng_name, timeline) |>
  summarise(topic_count = n(), .groups = "drop") |>
  complete(sentiment_label, eng_name, fill = list(topic_count = 0)) |>
  group_by(eng_name) |> 
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_total, sum))


# 1. Prepare the master data (No timeline filter)
plot_data_timeline_after <- spatial_tweets |>
  filter(explicitness == "Explicit" & timeline == "After_24") |>
  mutate(sentiment_label = case_when(
    sentiment_label == "" ~ "Neutral",
    TRUE ~ sentiment_label
  )) |>
  group_by(sentiment_label, eng_name, timeline) |>
  summarise(topic_count = n(), .groups = "drop") |>
  complete(sentiment_label, eng_name, fill = list(topic_count = 0)) |>
  group_by(eng_name) |> 
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_total, sum))


## Testing the number of rows
number_tweets_per_timeline <- spatial_tweets|>
  filter(timeline == "After_24" & explicitness == "Explicit")

nrow(number_tweets_per_timeline)

# Combined scales Before 24
ggplot(plot_data_timeline_before, aes(x = eng_name, y = topic_count,
                                      fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c("Public Monuments",
                                       "Tehran / Valiasr Street Life & Nostalgia",
                                       "City Theater Metro & Underpass",
                                       "Valiasr Street Connectivity & Length",
                                       "Urban Management & Heritage Protection of City Theater",
                                       "Valiasr / Tajrish Tree Preservation & Urban Ecology",
                                       "Metro Infrastructure & Tajrish Station"
  )), show.legend = FALSE) +
  geom_text(aes(label = label_text), hjust = -0.2, size = 3.5, color = "black") +
  # Split by Scale (Type) and Sentiment
  facet_grid(~sentiment_label, scales = "fixed") +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25), guide = "none") +
  coord_flip() +
  scale_fill_manual(values = c("negative" = "#d9534f", "neutral" = "#999999", "positive" = "#5cb85c")) +
  theme_minimal() +
  labs(title = "Overal Localised Spatial Topics: Between 2022-2024",
       subtitle = "Tweet Count: 870, Total: 1794 | Combined Scales",
       ## nrow before: 870 // nrows after: 924
       x = "Spatial Topics", 
       y = "Tweet Count") +
  theme(strip.text = element_text(face = "bold", size = 10, color = "black"),
              strip.background = element_blank(),
              panel.background = element_rect(fill = NA, color = NA),
              plot.background  = element_rect(fill = "white"),
              panel.grid.major  = element_line(color = "gray80"),
              panel.grid.minor = element_line(color = "gray90"),
              ## axis
              axis.text = element_text(color = "black"),
              axis.text.y = element_text(size = 12, hjust = 1, color = "black"),
              axis.text.x = element_text(hjust = 0.9, color = "black"),
              axis.title = element_text(color = "black", face = "bold"),
              ## title and subtitle
              plot.title = element_text(color = "black"),
              plot.subtitle = element_text(color = "black")
        ) +
  scale_y_continuous(expand = expansion(mult = c(0, .3)))


# Combined Scales after24
ggplot(plot_data_timeline_after, aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c("Public Monuments", 
                                       "City Theater Metro & Underpass",
                                       "Tehran / Valiasr Street Life & Nostalgia",
                                       "Valiasr Street Connectivity & Length",
                                       "Urban Management & Heritage Protection of City Theater",
                                       "Valiasr / Tajrish Tree Preservation & Urban Ecology",
                                       "Metro Infrastructure & Tajrish Station")),
           show.legend = FALSE) +
  geom_text(aes(label = label_text), hjust = -0.2, size = 3.5, color = "black") +
  # Split by Scale (Type) and Sentiment
  facet_grid(~sentiment_label, scales = "fixed") +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25), guide = "none") +
  coord_flip() +
  scale_fill_manual(values = c("negative" = "#d9534f", "neutral" = "#999999", "positive" = "#5cb85c")) +
  labs(title = "Overal Localised Spatial Topics: Between 2024-2026",
       subtitle = "Tweet Count: 924, Total: 1794 | Combined Scales",
       ## nrow before: 870 // nrows after: 924
       x = "Spatial Topics", 
       y = "Tweet Count") +
  theme(strip.text = element_text(face = "bold", size = 10, color = "black"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = "white"),
        panel.grid.major  = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"),
        ## axis
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 12, hjust = 1, color = "black"),
        axis.text.x = element_text(hjust = 0.9, color = "black"),
        axis.title = element_text(color = "black", face = "bold"),
        ## title and subtitle
        plot.title = element_text(color = "black"),
        plot.subtitle = element_text(color = "black")
        ) +
  scale_y_continuous(expand = expansion(mult = c(0, .35)))

# 1. Prepare the master data (No timeline filter)
plot_data_all <- spatial_tweets |>
  filter(explicitness == "Explicit")|>
  group_by(timeline, sentiment_label, eng_name, type) |>
  summarise(topic_count = n(), .groups = "drop") |>
  mutate(topic_pct = (topic_count / sum(topic_count)) * 100)|>
  mutate(eng_name = reorder(eng_name, topic_count, sum),
         label_text = paste0(round(topic_pct, 1), "% (",(topic_count),")"))

# 2. Create the Master Plot
ggplot(plot_data_all, aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
  geom_col() +
  geom_text(aes(label = label_text), hjust = -0.2, size = 3) +
  # Split by Scale (Type) and Sentiment
  facet_grid(timeline ~ type, scales = "free_y") + 
  coord_flip() +
  scale_fill_manual(values = c("negative" = "#d9534f", "neutral" = "#999999", "positive" = "#5cb85c")) +
  theme_minimal() +
  labs(title = "Overal Spatial Tweets",
       subtitle = "Total Tweets: 1,794 | Combined Scales and Timelines",
       x = "Spatial Topics", 
       y = "Tweet Count") +
  theme(strip.text = element_text(face = "bold", size = 10))


### Topic Distribution
topic_distribution <- spatial_tweets|>
  group_by(eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  complete(eng_name, fill = list(topic_count = 0)) |>
  # group_by(eng_name) |> 
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_count, sum))


## General Topic Distribution
ggplot(topic_distribution, aes(x = eng_name, y = topic_count)) +
  geom_col(show.legend = FALSE, fill = "#5556a5") +
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 4,
            # angle = 45,
            color = "black",
            fontface = "italic") +
  # facet_grid(type ~ timeline, scales = "free_y", space = "free_y") + 
  coord_flip() +
  # Distinct Urban Sentiment Palette
  theme_minimal() +
  labs(title = "Tehran Spatial Topics Distribution",
       #Spatial or general spatial topics
       subtitle = "Labels: Tweet Frequency per Topic - Total Tweets: 1,794",
       # general total 2,948 / spatial total 1,794
       y = "Tweet Volume (Absolute Count)",
       x = "Spatial Topic") +
  # Increase the expansion on the Y-axis to prevent labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  theme(
    # strip.background = element_rect(fill = "whitesmoke", color = "gray80"),
    # strip.text = element_text(size = 9, face = "bold"),
    # strip.background = element_rect(fill = "whitesmoke", color = "gray80"),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = NA), #gray20 for dark
    panel.grid.major  = element_line(color = "gray80"),  #gray40 for dark 
    panel.grid.minor = element_line(color = "gray90"),
    ## axis
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(size = 11, hjust = 1, color = "black"),
    axis.text.x = element_text(size = 11, hjust = 0.9, color = "black"),
    axis.title = element_text(color = "black", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "black"),
    plot.subtitle = element_text(color = "black")
  )



topic_distribution_explicit <- spatial_tweets |>
  filter(explicitness == "Explicit") |>
  group_by(eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  complete(eng_name, fill = list(topic_count = 0)) |>
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_count, sum))

## Explicit topic distribution
ggplot(topic_distribution_explicit, aes(x = eng_name, y = topic_count)) +
  geom_col(show.legend = FALSE, fill = "#5556a5") +
  ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
  ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 4.0,
            # angle = 45,
            color = "white", #white #black for light theme
            fontface = "italic") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Tehran Localised Spatial Topics Distribution",
       subtitle = "Tweet Frequency per Topic | Total Tweets: 1,794",
       x = "Spatial Topics",
       y = "Tweet Volume") +
  # Increase the expansion on the Y-axis to prevent labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    # strip.background = element_rect(fill = "whitesmoke", color = "gray80"),
    strip.text = element_text(size = 12, face = "bold"),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"), #white for light theme, # gray20 for dark
    panel.grid.major  = element_line(color = "gray40"),  #gray40 for dark, gray80 for light
    panel.grid.minor  = element_line(color = "gray50"), #gray 50 for dark, gray90 for light
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 13, hjust = 1, color = "white"),
    axis.text.x = element_text(size = 11, hjust = 0.9, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
  )

# Localised Topic-Sentiment distribution
topic_distribution_explicit_sentiment <- spatial_tweets |>
  filter(explicitness == "Explicit") |>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  complete(sentiment_label, eng_name, fill = list(topic_count = 0)) |>
  group_by(eng_name)|>
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_total, sum))


## Topic Distribution per sentiments dark themed
ggplot(topic_distribution_explicit_sentiment, aes(x = eng_name, y = topic_count,
                                                  fill = sentiment_label)) +
  geom_col(show.legend = FALSE) +
  facet_grid(~sentiment_label, scales = "fixed") +
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 3.0,
            # angle = 45,
            color = "white",
            fontface = "italic") +
  coord_flip() +
  # Distinct Urban Sentiment Palette
  scale_fill_manual(values = c("negative" = "#d9534f", "neutral" = "#999999",
                               "positive" = "#5cb85c")) +
  ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
  ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
  theme_minimal() +
  labs(title = "Tehran Localised Spatial Topic-Sentiment Distribution",
       subtitle = "Tweet Frequency per Topic & Sentiments | Total Tweets: 1,794",
       x = "Spatial Topics",
       y = "Tweet Volume") +
  # Increase the expansion on the Y-axis to prevent labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))+
  theme(
    # strip.background = element_rect(fill = "whitesmoke", color = "gray80"),
    strip.text = element_text(size = 9, face = "bold"),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray50"),   
    panel.grid.minor  = element_line(color = "gray60"),
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 13, hjust = 1, color = "white"),
    axis.text.x = element_text(size = 11,hjust = 0.9, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
    )


## Topic Distribution per sentiments with fixed blocks light themed
ggplot(topic_distribution_explicit_sentiment, 
       aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
  # 1. THE FIX: position = "fill" makes all bars the same length (0 to 1)
  geom_col(position = "fill", show.legend = TRUE) +
  # 2. Adjust text to show proportions inside the blocks
  # Note: position_fill(vjust = 0.5) puts the text in the middle of each color block
  geom_text(aes(label = label_text), 
            position = position_fill(vjust = 0.5), 
            size = 4.0,
            color = "white", #white for dark
            fontface = "bold") +
  coord_flip() +
  # Distinct Urban Sentiment Palette
  scale_fill_manual(values = c("negative" = "#d9534f", 
                               "neutral" = "#999999",
                               "positive" = "#5cb85c"),
                    name = "Sentiment") +
  # 3. Format Y axis as percentages (0% to 100%)
  scale_y_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.background = element_rect(fill = "gray20", color = NA), #gray20 for dark
    panel.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_line(color = "gray40"), #gray40 for dark, gray80 for light
    panel.grid.minor = element_line(color = "gray50"), #gray50 for dark, gray90 for light
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 13),
    axis.text.x = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(fill = NA, color = NA), #gray20 for dark
    legend.text = element_text(size = 11, color = "white", face = "bold")
  ) +
  labs(title = "Tehran Localised Spatial Topic-Sentiment Distribution",
       subtitle = "Tweet Frequency per Topic & Sentiments | Total Tweets: 1,794",
       x = "Spatial Topics",
       y = "Tweet Volume")


## Integrated Matrix of scale

plot_data_combined_scales <- spatial_tweets |>
  filter(explicitness == "Explicit") |>
  # 1. Group by Timeline, Sentiment, and Name (Scale is ignored/merged)
  group_by(timeline, sentiment_label, eng_name) |>
  summarise(
    topic_count = n(),
    avg_score = mean(sentiment_score), 
    .groups = "drop") |>
  # 2. Percentage Logic: % of all explicit tweets within that timeline
  group_by(timeline) |>
  mutate(topic_pct = (topic_count / sum(topic_count)) * 100) |>
  ungroup() |>
  # 3. Formatting
  mutate(
    eng_name = reorder(eng_name, topic_count, sum),
    # Including both % and Score in the label
    label_text = paste0(round(topic_pct, 1), "% (", round(avg_score, 2), ")")
  )

# 2. Plotting with a focus on the 4-way comparison
ggplot(plot_data_combined_scales, aes(x = eng_name, y = topic_count,
                                      fill = sentiment_label)) +
  geom_col(show.legend = FALSE) +
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 2.5, 
            color = "black",
            fontface = "italic") +
  # Rows = Scale (Type), Columns = Timeline & Sentiment
  facet_grid(timeline ~ sentiment_label, scales = "free_y", space = "free_y") + 
  coord_flip() +
  # Distinct Urban Sentiment Palette
  scale_fill_manual(values = c("negative" = "#d9534f", "neutral" = "#999999", "positive" = "#5cb85c")) +
  theme_minimal() +
  labs(title = "Tehran Overall Spatial Topics and Sentiments",
       subtitle = "Labels: Tweet Frequency (Mean Sentiment Score) - Total Tweets: 2,948",
       x = "Spatial Topics",
       y = "Tweet Volume") +
  # Increase the expansion on the Y-axis to prevent labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
  theme(
    strip.background = element_rect(fill = "whitesmoke", color = "gray80"),
    strip.text = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8)
  )


plot_data_combined_scales24 <- spatial_tweets |>
  filter(timeline == "Before_24" & explicitness == "Explicit")|>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(),
            avg_score = mean(sentiment_score),
            sentiment_total = sum(sentiment_score),
            .groups = "drop") |>
  complete(sentiment_label, eng_name, 
           fill = list(sentiment_total = 0, topic_count = 0, avg_score = 0)) |>
  group_by(eng_name) |> 
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(sentiment_total, 1), " (", round(avg_score, 2),")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_total, sum))

plot_data_combined_scales26 <- spatial_tweets |>
  filter(timeline == "After_24" & explicitness == "Explicit" )|>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(),
            avg_score = mean(sentiment_score),
            sentiment_total = sum(sentiment_score),
            .groups = "drop") |>
  complete(sentiment_label, eng_name,
           fill = list(sentiment_total = 0, topic_count = 0, avg_score = 0)) |>
  group_by(eng_name) |> 
  mutate(
    topic_total = sum(topic_count),
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(sentiment_total, 1), " (", round(avg_score, 2),")")
  ) |>
  ungroup() |>
  mutate(eng_name = reorder(eng_name, topic_total, sum))


# Overall before mean score
ggplot(plot_data_combined_scales24, aes(x = eng_name, y = topic_count,
                                        fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c(
    "Public Monuments",
    "Tehran / Valiasr Street Life & Nostalgia",
    "City Theater Metro & Underpass",
    "Valiasr Street Connectivity & Length",
    "Urban Management & Heritage Protection of City Theater",
    "Valiasr / Tajrish Tree Preservation & Urban Ecology",
    "Metro Infrastructure & Tajrish Station")),
    show.legend = FALSE) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.25), guide = "none") +
  geom_text(aes(label = label_text), hjust = -0.2, size = 4, color = "black") +
  # Split by Scale (Type) and Sentiment
  facet_grid(~sentiment_label, scales = "fixed") + 
  coord_flip() +
  scale_fill_manual(values = c("negative" = "#d9534f",
                               "neutral" = "#999999",
                               "positive" = "#5cb85c")) +
  labs(title = "Overal Localised Spatial Tweets: Before 2024",
       subtitle = "Total Sentiment Score (Mean Sentiment Score) | Total Tweets: 870",
       # Tweet Frequency
       x = "Spatial Topics", 
       y = "Tweet Count") +
  theme(
    strip.text = element_text(size = 11, face = "bold", color = "white"),
    strip.background = element_blank(),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "white"),#gray20 for dark
    panel.grid.major  = element_line(color = "gray80"), #gray40 for dark #gray80 for light 
    panel.grid.minor  = element_line(color = "gray90"), #gray50 for dark #gray90 for light
    ## axis
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(size = 13, hjust = 1, color = "black"),
    axis.text.x = element_text(size = 11, hjust = 0.9, color = "black"),
    axis.title = element_text(size = 14, color = "black", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "black"),
    plot.subtitle = element_text(color = "black")
    ) +
  scale_y_continuous(expand = expansion(mult = c(0, .33)))


# Overall after mean score
ggplot(plot_data_combined_scales26, aes(x = eng_name, y = topic_count,
                                        fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c(
    "Public Monuments",
    "Tehran / Valiasr Street Life & Nostalgia",
    "City Theater Metro & Underpass",
    "Valiasr Street Connectivity & Length",
    "Urban Management & Heritage Protection of City Theater",
    "Valiasr / Tajrish Tree Preservation & Urban Ecology",
    "Metro Infrastructure & Tajrish Station")),
    show.legend = FALSE) +
  geom_text(aes(label = label_text), hjust = -0.2, size = 4, color = "black") +
  # Split by Scale (Type) and Sentiment
  facet_grid(~sentiment_label, scales = "fixed") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.25), guide = "none") +
  coord_flip() +
  scale_fill_manual(values = c("negative" = "#d9534f",
                               "neutral" = "#999999",
                               "positive" = "#5cb85c")) +
  theme_minimal() +
  labs(title = "Overal Localised Spatial Tweets: After 2024",
       subtitle = "Total Sentiment Score (Mean Sentiment Score) | Total Tweets: 924",
       # Tweet Frequency
       x = "Spatial Topics", 
       y = "Tweet Count") +
  theme(
    strip.text = element_text(size = 11, face = "bold", color = "black"),
    strip.background = element_blank(),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "white"),
    panel.grid.major  = element_line(color = "gray80"),   
    panel.grid.minor  = element_line(color = "gray90"),
    ## axis
    axis.text = element_text(color = "black"),
    axis.text.y = element_text(size = 13, hjust = 1, color = "black"),
    axis.text.x = element_text(size = 11, hjust = 0.9, color = "black"),
    axis.title = element_text(size = 14, color = "black", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "black"),
    plot.subtitle = element_text(color = "black")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, .33)))



### Chi-square assessment

# Filter and Table
before_24_matrix <- spatial_tweets |>
  filter(explicitness == "Explicit")|>
  filter(timeline == "Before_24") |>
  select(type, sentiment_label) |>
  table()

print("BEFORE 2024")
print(before_24_matrix)
# Chi-Square
chisq.test(before_24_matrix)


# Run this for the After 2024 data
after_24_matrix <- spatial_tweets |>
  filter(explicitness == "Explicit")|>
  filter(timeline == "After_24") |>
  select(type, sentiment_label) |>
  table()

print("AFTER 2024")
print(after_24_matrix)
chisq.test(after_24_matrix)


all_time_matrix <- spatial_tweets|>
  filter(explicitness == "Explicit")|>
  select(type, sentiment_label)|>
  table()

print("SCALES Comparison")
print(all_time_matrix)
chisq.test(all_time_matrix)


general_matrix <- spatial_tweets|>
  select(type, sentiment_label)|>
  table()

print("general Comparison")
print(general_matrix)
chisq.test(general_matrix)


general_before <- spatial_tweets|>
  filter(timeline == "Before_24")|>
  select(type, sentiment_label)|>
  table()

print("General Before24 Comparison")
print(general_before)
chisq.test(general_before)


general_after <- spatial_tweets|>
  filter(timeline == "After_24")|>
  select(type, sentiment_label)|>
  table()

print("General After24 Comparison")
print(general_after)
chisq.test(general_after)


## Topic / Sentiment Chi-square Assessment

# 1. Filter for the two periods
before_topics <- spatial_tweets |>
  filter(explicity == "Explicit")|>
  filter(timeline == "Before_24")

after_topics <- spatial_tweets|>
  filter(explicity == "Explicit")|>
  filter(timeline == "After_24")
# 
# # Including percentage for before 24
# before_stats <- before_topics |>
#   group_by(eng_name, type) |>
#   summarise(count = n(), .groups = "drop") |>
#   mutate(pct_of_period = (count / nrow(before_topics)) * 100) |>
#   arrange(desc(pct_of_period))
# 
# after_stats <- after_topics|>
#   group_by(sentiment_label, eng_name, type)|>
#   summarise(count = n(), .groups = "drop")|>
#   mutate(pct_of_period = (count / nrow(after_topics)) * 100)|>
#   arrange(desc(pct_of_period))
# 
# print(before_stats)
# print(after_stats)


# 2. Create the matrices (Topics vs. Sentiment)
matrix_before <- table(before_topics$type, before_topics$sentiment_label)
matrix_after <- table(after_topics$type, after_topics$sentiment_label)

# 3. Run the Chi-Square tests
chi_before_topics <- chisq.test(matrix_before)
chi_after_topics <- chisq.test(matrix_after)

# 4. View Results
print("Topic-Sentiment Association BEFORE 2024")
print(matrix_before)
print(chi_before_topics)

print("Topic-Sentiment Association AFTER 2024")
print(matrix_after)
print(chi_after_topics)

ct_matrix24 <- spatial_tweets|>
  filter(type == "ct" & timeline == "Before_24" & explicitness == "Explicit")|>
  select(timeline, sentiment_label)|>
  table()


ct_matrix26 <- spatial_tweets|>
  filter(type == "ct" & timeline == "After_24" & explicitness == "Explicit")|>
  select(timeline, sentiment_label)|>
  table()

print("CT timeline-sentiment comparison_24")
print(ct_matrix24)
chisq.test(ct_matrix24)

print("CT timeline-sentiment comparison_26")
print(ct_matrix26)
chisq.test(ct_matrix26)


ct_matrix <- spatial_tweets|>
  filter(type == "ct" & explicitness == "Explicit")|>
  select(type, sentiment_label)|>
  table()

ct_table <- spatial_tweets|>
  filter(explicitness == "Explicit" & type == "ct")|>
  select(timeline, sentiment_label)|>
  table()
  
print(ct_table)

print("CT timeline-sentiment comparison")
print(ct_table)
chisq.test(ct_table)


teh_table <- spatial_tweets|>
  filter(explicitness == "Explicit" & type == "not_ct")|>
  select(timeline, sentiment_label)|>
  table()

print("Tehran timeline-sentiment comparison")
print(teh_table)
chisq.test(teh_table)


not_ct_matrix24 <- spatial_tweets|>
  filter(type == "not_ct" & timeline == "Before_24" & explicitness == "Explicit")|>
  select(timeline, sentiment_label)|>
  table()

print("Not-CT timeline-sentiment comparison_24")
print(not_ct_matrix24)
chisq.test(not_ct_matrix24)


not_ct_matrix26 <- spatial_tweets|>
  filter(type == "not_ct" & timeline == "After_24" & explicitness == "Explicit")|>
  select(timeline, sentiment_label)|>
  table()

print("Not-CT timeline-sentiment comparison_26")
print(not_ct_matrix26)
chisq.test(not_ct_matrix26)
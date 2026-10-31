# In R, install and run the necessary libraries
# install.packages(c("gghighlight", "tidyverse", "dplyr", "ggplot2")
# remove the # sign to install the packages only for the first time
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gghighlight)

topics <- read.csv("output/Py_results/plot_topics_english.csv")
tweets <- read.csv("output/full_data_translated_keyprob_sentiments.csv")

# Define our Spatial General Topic IDs from processed final topic # numbers in combined vector
spatial_ids <- c(0, 1, 2, 4, 5, 11, 12, 13, 16, 18, 19, 21, 22, 30, 31, 32, 37, 41, 46, 52, 55, 76, 82, 86, 88, 93)

# Define the localised topics
spatial_ids_local <- c(0, 2, 5, 11, 13, 16, 18, 19, 30, 31, 32, 37, 86)
#  We’d better replace the topic names with the refined versions for clarity and
# consistency in the visualizations
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
                         "86" = "Tajrish-Valiasr Walks",
                         "88" = "Tehran's Historical Gates",
                         "93" = "Tehran Cafes")
# We can already prepare the topics to create the word clouds after refining the
# English topic titles
# filter and prepare spatial topics
spatial_topics <- topics |>
  filter(Topic %in% spatial_ids)

# exporting the classified topics to create wordclouds
topics_for_cloud <- spatial_topics|>
  mutate(eng_name = spatialtopic_names[as.character(Topic)])|>
  mutate(contextuality = case_when(Topic %in% spatial_ids_explicit ~ "Local",
                                  TRUE ~ "General"))|>
  select(Topic, eng_name, contextuality, Score, Count, English_Word,
         English_Representation)
# Write the output topic as .csv for wordcloud and further
# analytical steps
write.csv(topics_for_cloud , "output/Py_results/plot_topics_english.csv")

# Further processing with the analyzed tweets; temporal,
# categorical classifications and organization, topic name updates
# etc.
spatial_tweets <- tweets |>
  select(-X)|>
  filter(final_topic %in% spatial_ids)|>
  mutate(Topic = final_topic)|>
  left_join(counttopics, by = "Topic")|>
  mutate(eng_name = spatialtopic_names[as.character(Topic)]) |>
  mutate(contextuality = case_when(Topic %in% spatial_ids_local ~ "Local",
                                  TRUE ~ "General"))|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year != "2021")|>
  mutate(timeline = if_else(year < 2024, "Before_24", "After_24"))|>
  select(doc_id, text, txtenglish, Topic, eng_name, Count,
         sentiment_label, sentiment_score, contextuality, type, year, timeline, createdAt,
         all_of(spatial_columns))
# You can save the processed data right away at this stage.
# Remove the Hash sign, so you can run the line.
write.csv(spatial_tweets,"output/Py_results/mostfinal_analysis_output.csv")

# Prepare the City Theater data for 2022-24 topic-sentiments for
# the plot
plot_data_ct24 <- spatial_tweets |>
  filter(contextuality == "Local") |>
  filter(type == "ct" & timeline == "Before_24") |>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  complete(sentiment_label, eng_name, fill = list(topic_count = 0)) |>

  # Row-wise percentage (Topic-based)
  group_by(eng_name) |>
  mutate(
    topic_total = sum(topic_count),
    # If the topic has at least one tweet, calculate %; otherwise, 0
    topic_pct = ifelse(topic_total > 0, (topic_count / topic_total) * 100, 0),
    label_text = paste0(round(topic_pct, 1), "% (", topic_count, ")")
  ) |>
  ungroup() |>
  # Sort topics by total volume so the chart is organized
  mutate(eng_name = reorder(eng_name, topic_total))

# CT 24 plot in dark theme
ggplot(plot_data_ct24, aes(x = eng_name, y = topic_count,
                           fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c(
    "City Theater Metro & Underpass",
    "Tehran / Valiasr Street Life & Nostalgia",
    "Urban Management & Heritage Protection of City Theater",
    "Performing Arts & Festivals (City Theater)"
  )),
  show.legend = FALSE) +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.3),
                     guide = "none") +
  geom_text(aes(label = label_text),
            hjust = -0.1,
            size = 3.5,
            color = "white") +
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
    strip.text = element_text(size = 9, face = "bold", color = "white"),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray50"),
    panel.grid.minor  = element_line(color = "gray40"),
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 10, hjust = 1, color = "white"),
    axis.text.x = element_text(hjust = 0.9, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
  ) +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))

# The same preparation for CT 2024-26
plot_data_ct26 <- spatial_tweets |>
  filter(explicitness == "Explicit") |>
  filter(type == "ct" & timeline == "After_24") |>
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
    "Performing Arts & Festivals (City Theater)"
  )),
  show.legend = FALSE) +
  geom_text(aes(label = label_text),
            hjust = -0.1,
            size = 3.5,
            color = "white") +
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
    axis.text.y = element_text(size = 10, hjust = 1, color = "white"),
    axis.text.x = element_text(hjust = 0.9, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
  ) +
  labs(title = "Localised Spatial Topic Frequency by Sentiments for City Theater 2024 - 2026",
       subtitle = "Tweet distributions per topic | Tweet Count: 316 | Total: 582",
       # nrows ct 26: 316 // Total ct: 582
       x = "Spatial Topics",
       y = "Number of Tweets (Absolute Count)") +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.2)))


# Preparing data for Tehran scale between 2022 and 2026

plot_data_teh24 <- spatial_tweets |>
  filter(explicitness == "Explicit") |>
  filter(type == "not_ct" & timeline == "Before_24") |>
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

# Plotting the data in dark theme

# Plot Teh 26 dark
ggplot(plot_data_teh26, aes(x = eng_name, y = topic_count,
                            fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c(
    "Tehran / Valiasr Street Life & Nostalgia",
    "Valiasr Street Connectivity & Length",
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
    # panel & background
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


# # Overall before with mean sentiment score
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
  geom_text(aes(label = label_text), hjust = -0.2, size = 4) +
  # Split by Scale (Type) and Sentiment
  facet_wrap(~sentiment_label, scales = "fixed") +
  coord_flip() +
  scale_fill_manual(values = c("negative" = "#d9534f",
                               "neutral" = "#999999",
                               "positive" = "#5cb85c")) +
  theme_minimal() +
  labs(title = "Overal Localised Spatial Tweets: Before 2024",
       subtitle = "Tweet Frequency (Mean Sentiment Score) | Total Tweets: 870",
       x = "Spatial Topics",
       y = "Tweet Count") +
  theme(strip.text = element_text(face = "bold", size = 10),
        ## panel & background
        panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA), #gray20 for dark
        panel.grid.major  = element_line(color = "gray80"),  #gray40 for dark
        panel.grid.minor = element_line(color = "gray90"),
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


# # Overall after with mean sentiment score
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
  geom_text(aes(label = label_text), hjust = -0.2, size = 4) +
  # Split by Scale (Type) and Sentiment
  facet_wrap(~sentiment_label, scales = "fixed") +
  coord_flip() +
  scale_fill_manual(values = c("negative" = "#d9534f",
                               "neutral" = "#999999",
                               "positive" = "#5cb85c")) +
  theme_minimal() +
  labs(title = "Overal Localised Spatial Tweets: After 2024",
       subtitle = "Tweet Frequency (Mean Sentiment Score) | Total Tweets: 924",
       x = "Spatial Topics",
       y = "Tweet Count") +
  theme(strip.text = element_text(face = "bold", size = 10),
        ## panel & background
        panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = NA), #gray20 for dark
        panel.grid.major  = element_line(color = "gray80"),  #gray40 for dark
        panel.grid.minor = element_line(color = "gray90"),
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






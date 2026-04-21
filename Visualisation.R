# install.packages("umap")
library(tidyverse)
library(dplyr)
# library(umap)
library(ggplot2)

topics <- read.csv("output/Py_results/plot_topics_english.csv")
tweets <- read.csv("output/full_data_translated_keyprob_sentiments.csv")

# 2. Define our Spatial/Urban Topic IDs

spatial_ids <- c(0, 1, 2, 4, 5, 11, 12, 13, 16, 18, 19, 21, 22, 30,
                 31, 32, 37, 41, 46, 52, 55, 76, 82, 84, 86, 88, 93)

spatial_ids_explicit <- c(0, 1, 2, 5, 11, 13, 16, 18, 19,
                          31)



spatialtopic_names <- c( "0" = "Tehran Street Life & Nostalgia",
                         "1" = "Neighborhood Hubs & Squares",
                         "2" = "Grand Bazaar & Market Shopping",
                         "4" = "Toopkhaneh Square Naming & War",
                         "5" = "City Theater Metro & Underpass",
                         "11" = "Metro Infrastructure & Tajrish Station",
                         "12" = "Avenues, Gardens & Boulevards",
                         "13" = "Performing Arts & Festivals (City Theater)",
                         "16" = "Valiasr Street Connectivity & Length",
                         "18" = "Tree Preservation & Urban Ecology",
                         "19" = "Urban Management & Heritage Protection(CT)",
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
                         "93" = "Tehran Cafes"
                         )

spatial_columns <- c('prob_topic_0', 'prob_topic_1', 'prob_topic_2', 'prob_topic_4',
                    'prob_topic_5', 'prob_topic_11', 'prob_topic_12', 'prob_topic_13',
                    'prob_topic_16', 'prob_topic_18', 'prob_topic_19', 'prob_topic_21',
                    'prob_topic_22', 'prob_topic_22', 'prob_topic_29', 'prob_topic_30',
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
  mutate(explicity = case_when(Topic %in% spatial_ids_explicit ~ "Explicit",
                               TRUE ~ "Non-explicit"))|>
  select(Topic, eng_name, explicity, Score, Count, English_Word, 
         English_Representation)

write.csv(topics_for_cloud , "output/Py_results/plot_topics_english.csv")



counttopics <- spatial_topics |>
  select(English_Name, Count, Topic)|>
  unique()


spatial_tweets <- tweets |>
  select(-X)|>
  filter(final_topic %in% spatial_ids)|>
  mutate(Topic = final_topic)|>
  left_join(counttopics, by = "Topic")|>
  mutate(eng_name = spatialtopic_names[as.character(Topic)]) |>
  mutate(explicity = case_when(Topic %in% spatial_ids_explicit ~ "Explicit",
                               TRUE ~ "Non-explicit"))|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year != "2021")|>
  mutate(timeline = if_else(year < 2024, "Before_24", "After_24"))|>
  select(doc_id, text, txtenglish, Topic, eng_name, Count, 
         sentiment_label, sentiment_score, explicity, type, year, timeline, createdAt,
         all_of(spatial_columns))

sum_tweets <- spatial_tweets|>
  filter(explicity == "Explicit")|>
  str()

write.csv(spatial_tweets,"output/Py_results/mostfinal_analysis_output.csv")

spatial_cols <- all_of(spatial_columns)|>
  as.data.frame()

topic_probs <- spatial_tweets|>
  select(spatial_cols, doc_id)|>
  as.data.frame()

sum(is.na(spatial_tweets$type))
sum(is.na(spatial_tweets$timeline))
sum(is.na(spatial_tweets$sentiment_score))
table(spatial_tweets$sentiment_label, useNA = "always")

# 1. Prepare the data: Grouping by sentiment and topic
plot_data_ct24 <- spatial_tweets |>
  filter(explicity == "Explicit")|>
  filter(type == "ct" & timeline == "Before_24")|>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  mutate(topic_pct = (topic_count / sum(topic_count)) * 100)|>
  # Reordering topics within each sentiment block for better visual flow
  mutate(eng_name = reorder(eng_name, topic_count, sum),
         label_text = paste0(round(topic_pct, 1), "% (",(topic_count),")"))


plot_data_ct26 <- spatial_tweets |>
  filter(explicity == "Explicit")|>
  filter(type == "ct" & timeline == "After_24")|>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  mutate(topic_pct = (topic_count / sum(topic_count)) * 100)|>
  mutate(eng_name = reorder(eng_name, topic_count, sum),
         label_text = paste0(round(topic_pct, 1), "% (",(topic_count),")"))

plot_data_teh24 <- spatial_tweets |>
  filter(explicity == "Explicit")|>
  filter(type == "not_ct" & timeline == "Before_24")|>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  mutate(topic_pct = (topic_count / sum(topic_count)) * 100)|>
  mutate(eng_name = reorder(eng_name, topic_count, sum),
         label_text = paste0(round(topic_pct, 1), "% (",(topic_count),")"))

plot_data_teh26 <- spatial_tweets |>
  filter(explicity == "Explicit")|>
  filter(type == "not_ct" & timeline == "After_24")|>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  mutate(topic_pct = (topic_count / sum(topic_count)) * 100)|>
  mutate(eng_name = reorder(eng_name, topic_count, sum),
         label_text = paste0(round(topic_pct, 1), "% (",(topic_count),")"))

# 2. Create the plot
ggplot(plot_data_teh24, aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
  geom_col(show.legend = FALSE) +
  # The "Call-outs": Adding counts above the bars
  geom_text(aes(label = label_text), 
            hjust = -0.2, # Push text slightly to the right of the bar
            size = 3) +
  # Create the three "Blocks" based on sentiment
  facet_wrap(~sentiment_label, scales = "free_y") + 
  # Flip coordinates because topic names are long
  coord_flip() +
  # Aesthetics and Labels
  theme_minimal() +
  labs(title = "Spatial Topic Frequency by Sentiment",
       subtitle = "Tehran 2022 - 2024",
       x = "Spatial Topics",
       y = "Number of Tweets") +
  # Adjusting the Y-axis (which is now the top axis) to give room for labels
  scale_y_continuous(expand = expansion(mult = c(0, .45)))


# 1. Prepare the master data (No timeline filter)
plot_data_scale <- spatial_tweets |>
  filter(explicity == "Explicit")|>
  group_by(type, sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  mutate(topic_pct = (topic_count / sum(topic_count)) * 100)|>
  mutate(eng_name = reorder(eng_name, topic_count, sum),
         label_text = paste0(round(topic_pct, 1), "% (",(topic_count),")"))

# 1. Prepare the master data (No timeline filter)
plot_data_timeline <- spatial_tweets |>
  filter(explicity == "Explicit")|>
  group_by(timeline, sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  mutate(topic_pct = (topic_count / sum(topic_count)) * 100)|>
  mutate(eng_name = reorder(eng_name, topic_count, sum),
         label_text = paste0(round(topic_pct, 1), "% (",(topic_count),")"))

# 2. Create the Master Plot for combined timelines
ggplot(plot_data_scale, aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
  geom_col() +
  geom_text(aes(label = label_text), hjust = -0.2, size = 3) +
  # Split by Scale (Type) and Sentiment
  facet_grid(type ~ sentiment_label, scales = "free_y") + 
  coord_flip() +
  scale_fill_manual(values = c("negative" = "#d9534f", "neutral" = "#999999", "positive" = "#5cb85c")) +
  theme_minimal() +
  labs(title = "Overall Spatial Tweets: Tehran vs. City Theater",
       subtitle = "Total Tweets: 1,763 | Combined Timelines",
       x = "Spatial Topics", 
       y = "Tweet Count") +
  theme(strip.text = element_text(face = "bold", size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, .3)))
          
  


# 2. Create the Master Plot for combined scales
ggplot(plot_data_timeline, aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
  geom_col() +
  geom_text(aes(label = label_text), hjust = -0.2, size = 3) +
  # Split by Scale (Type) and Sentiment
  facet_grid(timeline ~ sentiment_label, scales = "free_y") + 
  coord_flip() +
  scale_fill_manual(values = c("negative" = "#d9534f", "neutral" = "#999999", "positive" = "#5cb85c")) +
  theme_minimal() +
  labs(title = "Overal Spatial Tweets: Before and After 2024",
       subtitle = "Total Tweets: 1,763 | Combined Scales",
       x = "Spatial Topics", 
       y = "Tweet Count") +
  theme(strip.text = element_text(face = "bold", size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, .3)))

# 1. Prepare the master data (No timeline filter)
plot_data_all <- spatial_tweets |>
  filter(explicity == "Explicit")|>
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
       subtitle = "Total Tweets: 1,763 | Combined Scales and Timelines",
       x = "Spatial Topics", 
       y = "Tweet Count") +
  theme(strip.text = element_text(face = "bold", size = 10))



## combined Plot including avg. sentiment scores

# 1. Prepare the final plotting data
plot_data_final <- spatial_tweets |>
  # Filter for your hand-picked topics
  filter(explicity == "Explicit") |> 
  # Group by all your categorical dimensions
  group_by(type, timeline, sentiment_label, eng_name) |>
  summarise(
    topic_count = n(),
    avg_score = mean(sentiment_score), 
    .groups = "drop") |>
  # The Proportional Logic:
  # We group by 'type' (scale) and 'timeline' (era)
  group_by(type, timeline) |> 
  # This makes 'sum(topic_count)' the total number of tweets in that specific era and scale
  mutate(topic_pct = (topic_count / sum(topic_count)) * 100) |>
  ungroup() |>
  # Final Visualisation
  mutate(
    eng_name = reorder(eng_name, topic_count, sum),
    label_text = paste0(round(topic_pct, 1), "% (", round(avg_score, 2), ")")
  )


# 2. Plotting with a focus on the 4-way comparison
ggplot(plot_data_final, aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
  geom_col(show.legend = FALSE) +
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 2.5, 
            color = "black",
            fontface = "italic") +
  # Rows = Scale (Type), Columns = Timeline & Sentiment
  facet_grid(type ~ timeline + sentiment_label, scales = "free_y", space = "free_y") + 
  coord_flip() +
  # Distinct Urban Sentiment Palette
  scale_fill_manual(values = c("negative" = "#e31a1c", "neutral" = "#feb24c", "positive" = "#33a02c")) +
  theme_minimal() +
  labs(title = "Tehran Spatial Topics and Sentiments",
       subtitle = "Labels: Tweet Frequency (Mean Sentiment Score)",
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

## Integrated Matrix of scale

plot_data_combined_scales <- spatial_tweets |>
  filter(explicity == "Explicit") |>
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
  scale_fill_manual(values = c("negative" = "#e31a1c", "neutral" = "#feb24c", "positive" = "#33a02c")) +
  theme_minimal() +
  labs(title = "Tehran Overall Spatial Topics and Sentiments",
       subtitle = "Labels: Tweet Frequency (Mean Sentiment Score)",
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


### Chi-square assessment

# 1. Filter and Table
before_24_matrix <- spatial_tweets |>
  filter(explicity == "Explicit")|>
  filter(timeline == "Before_24") |>
  select(type, sentiment_label) |>
  table()

print("BEFORE 2024")
print(before_24_matrix)

# 2. Chi-Square
chisq.test(before_24_matrix)


# Run this for the After 2024 data
after_24_matrix <- spatial_tweets |>
  filter(explicity == "Explicit")|>
  filter(timeline == "After_24") |>
  select(type, sentiment_label) |>
  table()

print("AFTER 2024")
print(after_24_matrix)
chisq.test(after_24_matrix)

## Topic / Sentiment Chi-square Assessment

# 1. Filter for the two periods
before_topics <- spatial_tweets |>
  filter(explicity == "Explicit")|>
  filter(timeline == "Before_24")

after_topics <- spatial_tweets|>
  filter(explicity == "Explicit")|>
  filter(timeline == "After_24")

# Including percentage for before 24
before_stats <- before_topics |>
  group_by(eng_name, type) |>
  summarise(count = n(), .groups = "drop") |>
  mutate(pct_of_period = (count / nrow(before_topics)) * 100) |>
  arrange(desc(pct_of_period))

after_stats <- after_topics|>
  group_by(sentiment_label, eng_name, type)|>
  summarise(count = n(), .groups = "drop")|>
  mutate(pct_of_period = (count / nrow(after_topics)) * 100)|>
  arrange(desc(pct_of_period))

print(before_stats)
print(after_stats)


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





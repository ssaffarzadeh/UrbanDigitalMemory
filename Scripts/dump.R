

oldspatialtopic_names <- c( "0" = "General Street Life & Nostalgia",
                            "1" = "Neighborhood Hubs & Squares (Nazi Abad)",
                            "2" = "Grand Bazaar & Market Shopping",
                            "4" = "Toopkhaneh Square Naming & War",
                            "5" = "City Theater Metro & Underpass",
                            "11" = "Metro Infrastructure & Tajrish Station",
                            "12" = "Avenues, Gardens & Boulevards",
                            "13" = "Performing Arts & Festivals (City Theater)",
                            "16" = "Valiasr Street Connectivity & Length",
                            "18" = "Tree Preservation & Urban Ecology",
                            "19" = "Urban Management & Heritage Protection(CT)",
                            "21" = "Atmospheric Streets & Alleys",
                            "29" = "Architectural Heritage (Hassan Abad)",
                            "31" = "Metro Station Entrances",
                            "32" = "Seasonal Walks (Autumn in Valiasr)",
                            "43" = "Urban Decay & Street Dogs (Vanak)",
                            "46" = "Hassan Abad Heritage Houses",
                            "52" = "Historical Renaming of Squares",
                            "82" = "Cafe Culture & Solitude",
                            "88" = "Historical Gates (Bab Homayoun)")

oldspatial_ids <- c(0, 1, 2, 4, 5, 11, 12, 13, 16, 18, 19, 21,
                    31, 32, 46, 52, 82, 88)

old_spatial_tweets <- tweets |>
  select(-X)|>
  filter(final_topic %in% spatial_ids)|>
  mutate(Topic = final_topic)|>
  mutate(eng_name = spatialtopic_names[as.character(Topic)]) |>
  # filter(!is.na(eng_name))|>
  left_join(topic_count, by = "Topic")|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year != "2021")|>
  mutate(timeline = if_else(year < 2024, "Before_24", "After_24"))|>
  select(doc_id, text, txtenglish, year, Topic, eng_name, Count, 
         sentiment_label, sentiment_score, type, timeline, createdAt, 
         all_of(spatial_columns))

alltopic_for24 <- tweets|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year < 2024 & type == "ct")|>
  select(doc_id, text, year, type, final_topic, txtenglish, sentiment_label)|>
  view()

notct_alltopic_for24 <- tweets|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year < 2024 & type == "not_ct")|>
  select(doc_id, text, year, type, final_topic, txtenglish, sentiment_label)|>
  view()

notct_alltopic_after24 <- tweets|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year >= 2024 & type == "not_ct")|>
  select(doc_ic, text, year, type, final_topic, txtenglish, sentiment_label)|>
  view()

alltopic_after24 <- tweets|>
  mutate(year = year(parse_date_time(createdAt, "a b d H:M:S z Y")))|>
  filter(year >= 2024 & type == "ct")|>
  select(doc_id, text, year,type, final_topic, txtenglish, sentiment_label)|>
  view()



### First edition of data ready for visualization

# 1. Prepare the data: Grouping by sentiment and topic
plot_data_ct24 <- spatial_tweets |>
  filter(explicitness == "Explicit")|>
  filter(type == "ct" & timeline == "Before_24")|>
  group_by(sentiment_label, eng_name) |>
  summarise(topic_count = n(), .groups = "drop") |>
  mutate(topic_pct = (topic_count / sum(topic_count)) * 100)|>
  # Reordering topics within each sentiment block for better visual flow
  mutate(eng_name = reorder(eng_name, topic_count, sum),
         label_text = paste0(round(topic_pct, 1), "% (",(topic_count),")"))


# 2. Create the plot
ggplot(plot_data_ct24, aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
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
       subtitle = "Tehran 2024 - 2026",
       x = "Spatial Topics",
       y = "Number of Tweets") +
  # Adjusting the Y-axis (which is now the top axis) to give room for labels
  scale_y_continuous(expand = expansion(mult = c(0, .65)))

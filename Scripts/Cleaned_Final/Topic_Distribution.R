# Topic Distribution
topic_distribution <- spatial_tweets|>
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

# General Topic Distribution
ggplot(topic_distribution, aes(x = eng_name, y = topic_count)) +
  geom_col(show.legend = FALSE, fill = "#5556a5") +
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 3.5,
            # angle = 45,
            color = "white",
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
    panel.grid.minor = element_blank(),
    # strip.background = element_rect(fill = "whitesmoke", color = "gray80"),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray40"),   
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 8, hjust = 1, color = "white"),
    axis.text.x = element_text(hjust = 0.9, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
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

# Explicit topic distribution
ggplot(topic_distribution_explicit, aes(x = eng_name, y = topic_count)) +
  geom_col(show.legend = FALSE, fill = "#5556a5") +
  ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
  ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 3.5,
            # angle = 45,
            color = "white",
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
    strip.text = element_text(size = 9, face = "bold"),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray40"),   
    panel.grid.minor  = element_line(color = "gray50"),
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 8, hjust = 1, color = "white"),
    axis.text.x = element_text(hjust = 0.9, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
  )

# Localised topic distribution
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

# Plot the localised topic distribution in dark theme
ggplot(topic_distribution_explicit, aes(x = eng_name, y = topic_count)) +
  geom_col(show.legend = FALSE, fill = "#5556a5") +
  ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
  ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 3.5,
            # angle = 45,
            color = "white",
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
    strip.text = element_text(size = 9, face = "bold"),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray40"),   
    panel.grid.minor  = element_line(color = "gray50"),
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 8, hjust = 1, color = "white"),
    axis.text.x = element_text(hjust = 0.9, color = "white"),
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

# Topic Distribution per sentiments with fixed blocks dark themed
ggplot(topic_distribution_explicit_sentiment, 
       aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
  # Position = "fill" makes all bars the same length (0 to 1)
  geom_col(position = "fill", show.legend = TRUE) +
  # Adjust text to show proportions inside the blocks
  # Note: position_fill(vjust = 0.5) puts the text in the middle
  # of each color block
  geom_text(aes(label = label_text), 
            position = position_fill(vjust = 0.5), 
            size = 2.8,
            color = "white", 
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
    plot.background = element_rect(fill = "gray20", color = NA),
    panel.background = element_rect(fill = NA, color = NA),
    panel.grid.major = element_line(color = "gray40"),   
    panel.grid.minor = element_line(color = "gray50"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom",
    legend.background = element_rect(fill = "gray20", color = NA),
    legend.text = element_text(color = "white")
  ) +
  labs(title = "Tehran Localised Spatial Topic-Sentiment Distribution",
       subtitle = "Tweet Frequency per Topic & Sentiments | Total Tweets: 1,794",
       x = "Spatial Topics",
       y = "Tweet Volume")

## Combined scales between 2022-2026
# Combined scales before 2024
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

# Plot combined scales before ‘24
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
  geom_text(aes(label = label_text), hjust = -0.2, size = 3.5, color = "white") +
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
  theme(strip.text = element_text(face = "bold", size = 10, color = "white"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = "gray20"),
        panel.grid.major  = element_line(color = "gray40"),
        panel.grid.minor = element_line(color = "gray30"),
        ## axis
        axis.text = element_text(color = "white"),
        axis.text.y = element_text(size = 11, hjust = 1, color = "white"),
        axis.text.x = element_text(hjust = 0.9, color = "white"),
        axis.title = element_text(color = "white", face = "bold"),
        ## title and subtitle
        plot.title = element_text(color = "white"),
        plot.subtitle = element_text(color = "white")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, .2)))

# Combined scales after 2024
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

# Plot the combined scales after ‘24
ggplot(plot_data_timeline_after, aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c("Public Monuments", 
                                       "City Theater Metro & Underpass",
                                       "Tehran / Valiasr Street Life & Nostalgia",
                                       "Valiasr Street Connectivity & Length",
                                       "Urban Management & Heritage Protection of City Theater",
                                       "Valiasr / Tajrish Tree Preservation & Urban Ecology",
                                       "Metro Infrastructure & Tajrish Station")),
           show.legend = FALSE) +
  geom_text(aes(label = label_text), hjust = -0.2, size = 3.5, color = "white") +
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
  theme(strip.text = element_text(face = "bold", size = 10, color = "white"),
        strip.background = element_blank(),
        panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = "gray20"),
        panel.grid.major  = element_line(color = "gray40"),
        panel.grid.minor = element_line(color = "gray30"),
        ## axis
        axis.text = element_text(color = "white"),
        axis.text.y = element_text(size = 11, hjust = 1, color = "white"),
        axis.text.x = element_text(hjust = 0.9, color = "white"),
        axis.title = element_text(color = "white", face = "bold"),
        ## title and subtitle
        plot.title = element_text(color = "white"),
        plot.subtitle = element_text(color = "white")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, .3)))

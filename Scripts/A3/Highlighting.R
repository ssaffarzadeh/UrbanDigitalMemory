library(tidyverse)
library(dplyr)
library(ggplot2)
library(gghighlight)


ggplot(topic_distribution, aes(x = eng_name, y = topic_count)) +
  geom_col(show.legend = FALSE) +
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 2.5,
            # angle = 45,
            color = "black",
            fontface = "italic") +
  # facet_grid(type ~ timeline, scales = "free_y", space = "free_y") + 
  coord_flip() +
  # Distinct Urban Sentiment Palette
  theme_minimal() +
  labs(title = "Tehran General Spatial Topics Distribution",
       #Spatial or general spatial topics
       subtitle = "Labels: Tweet Frequency per Topic | Total Tweets: 2,948",
       # general total 2,948 / spatial total 1,794
       y = "Tweet Volume (Absolute Count)",
       x = "Spatial Topic") +
  # Increase the expansion on the Y-axis to prevent labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))+
  theme(
    strip.background = element_rect(fill = "whitesmoke", color = "gray80"),
    strip.text = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.y  = element_text(size = 8),
    axis.text.x = element_text(hjust = 1)
  )

ggplot(topic_distribution_explicit, aes(x = eng_name, y = topic_count)) +
  geom_col(show.legend = FALSE) +
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 2.5,
            # angle = 45,
            color = "black",
            fontface = "italic") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Tehran Localised Spatial Topics Distribution",
       subtitle = "Tweet Frequency per Topic | Total Tweets: 1,794",
       x = "Spatial Topics",
       y = "Tweet Volume") +
  # Increase the expansion on the Y-axis to prevent labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.5)))+
  theme(
    strip.background = element_rect(fill = "whitesmoke", color = "gray80"),
    strip.text = element_text(size = 9, face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.y  = element_text(size = 8),
    axis.text.x = element_text(hjust = 1)
  )


## Topic Distribution per sentiments
ggplot(topic_distribution_explicit_sentiment, aes(x = eng_name, y = topic_count,
                                        fill = sentiment_label)) +
  geom_col(show.legend = FALSE) +
  facet_grid(~sentiment_label, scales = "fixed") +
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 2.8,
            # angle = 45,
            color = "black",
            fontface = "italic") +
  coord_flip() +
  # Distinct Urban Sentiment Palette
  scale_fill_manual(values = c("negative" = "#d9534f", "neutral" = "#999999",
                               "positive" = "#5cb85c")) +
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
    panel.grid.minor = element_blank(),
    axis.text.y  = element_text(size = 8),
    axis.text.x = element_text(hjust = 1)
  )

## Testing the number of rows
number_tweets_per_scale_time <- spatial_tweets|>
  filter(type == "ct" & explicitness == "Explicit" & timeline == "After_24")
nrow(number_tweets_per_scale_time)

# CT 24
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
  geom_text(aes(label = label_text), 
            hjust = -0.1, 
            size = 3,
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
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)))


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
            size = 3,
            color = "black") + 
  facet_wrap(~sentiment_label, scales = "fixed") + 
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c(
    "neutral" = "#969696",
    "negative" = "#d9534f",
    "positive" = "#5cb85c"
  )) + 
  labs(title = "Localised Spatial Topic Frequency by Sentiments for City Theater 2024 - 2026",
       subtitle = "Tweet distributions per topic | Tweet Count: 316 | Total: 582",
       # nrows ct 26: 316 // Total ct: 582
       x = "Spatial Topics",
       y = "Number of Tweets (Absolute Count)") +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)))


# Teh 24
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
            size = 2.8,
            color = "black") + 
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
       # nrows teh 24: 604 // Total ct: 1212
       x = "Spatial Topics",
       y = "Number of Tweets (Absolute Count)") +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)))


# Teh 26
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
            size = 2.8,
            color = "black") + 
  facet_wrap(~sentiment_label, scales = "fixed") + 
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c(
    "neutral" = "#969696",
    "negative" = "#d9534f",
    "positive" = "#5cb85c"
  )) + 
  labs(title = "Localised Spatial Topic Frequency by Sentiments for Tehran 2024 - 2026",
       subtitle = "Tweet distributions per topic | Tweet Count: 608 | Total: 1212",
       # nrows teh 26: 608 // Total ct: 1212
       x = "Spatial Topics",
       y = "Number of Tweets (Absolute Count)") +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)))



# Create the Master Plot for combined timelines
ggplot(plot_data_scale_teh, aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c(
                                       "Tehran / Valiasr Street Life & Nostalgia",
                                       "Grand Bazaar & Market Shopping",
                                       "Metro Infrastructure & Tajrish Station",
                                       "City Theater Metro & Underpass"
                                       )), 
               show.legend = FALSE) +
  geom_text(aes(label = label_text), 
            hjust = -0.1, 
            size = 2.8,
            color = "black") + 
  facet_grid(~ sentiment_label, scales = "fixed") + 
  coord_flip() +
  theme_minimal() +
  # Custom colors (Red for Negative, Gray for Neutral, Green for Positive)
  scale_fill_manual(values = c(
    "neutral" = "#969696",
    "negative" = "#d9534f",
    "positive" = "#5cb85c"
  )) + 
  labs(title = "Overall Localised Spatial Topics: Tehran Scale",
       subtitle = "Tweet Count: 1212, Total: 1794 | Combined Timelines",
       ## nrows ct : 582 // nrows not-ct: 1212
       x = "Spatial Topics",
       y = "Number of Tweets (Absolute Count)") +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  theme(strip.text = element_text(face = "bold", size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)))


# Combined timelines
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
            size = 2.8,
            color = "black") + 
  facet_grid(~ sentiment_label, scales = "fixed") + 
  coord_flip() +
  theme_minimal() +
  # Custom colors (Red for Negative, Gray for Neutral, Green for Positive)
  scale_fill_manual(values = c(
    "neutral" = "#969696",
    "negative" = "#d9534f",
    "positive" = "#5cb85c"
  )) + 
  labs(title = "Overall Localised Spatial Topics: City Theater Scale",
       subtitle = "Tweet Count: 582, Total: 1794 | Combined Timelines",
       ## nrows ct : 582 // nrows not-ct: 1212
       x = "Spatial Topics",
       y = "Number of Tweets (Absolute Count)") +
  # Expand the horizontal axis to prevent the percentage labels from being cut off
  theme(strip.text = element_text(face = "bold", size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.7)))



# Combined Scales After
ggplot(plot_data_timeline_after, aes(x = eng_name, y = topic_count,
                                     fill = sentiment_label)) +
  geom_col(aes(alpha = eng_name %in% c("Public Monuments", 
                                       "City Theater Metro & Underpass",
                                       "Tehran / Valiasr Street Life & Nostalgia",
                                       "Valiasr Street Connectivity & Length",
                                       "Urban Management & Heritage Protection of City Theater",
                                       "Valiasr / Tajrish Tree Preservation & Urban Ecology",
                                       "Metro Infrastructure & Tajrish Station"
                                       )), show.legend = FALSE) +
  geom_text(aes(label = label_text), hjust = -0.2, size = 3) +
  # Split by Scale (Type) and Sentiment
  facet_grid(~sentiment_label, scales = "fixed") + 
  coord_flip() +
  scale_fill_manual(values = c("negative" = "#d9534f", "neutral" = "#999999", "positive" = "#5cb85c")) +
  theme_minimal() +
  labs(title = "Overal Localised Spatial Topics: Between 2024-2026",
       subtitle = "Tweet Count: 924, Total: 1794 | Combined Scales",
       ## nrow before: 870 // nrows after: 924
       x = "Spatial Topics", 
       y = "Tweet Count") +
  theme(strip.text = element_text(face = "bold", size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, .3)))


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
  geom_text(aes(label = label_text), hjust = -0.2, size = 3) +
  # Split by Scale (Type) and Sentiment
  facet_grid(~sentiment_label, scales = "fixed") + 
  coord_flip() +
  scale_fill_manual(values = c("negative" = "#d9534f", "neutral" = "#999999", "positive" = "#5cb85c")) +
  theme_minimal() +
  labs(title = "Overal Localised Spatial Topics: Between 2022-2024",
       subtitle = "Tweet Count: 870, Total: 1794 | Combined Scales",
       ## nrow before: 870 // nrows after: 924
       x = "Spatial Topics", 
       y = "Tweet Count") +
  theme(strip.text = element_text(face = "bold", size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, .3)))




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
  geom_text(aes(label = label_text), hjust = -0.2, size = 3) +
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
  theme(strip.text = element_text(face = "bold", size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, .3)))


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
  geom_text(aes(label = label_text), hjust = -0.2, size = 3) +
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
  theme(strip.text = element_text(face = "bold", size = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, .3)))

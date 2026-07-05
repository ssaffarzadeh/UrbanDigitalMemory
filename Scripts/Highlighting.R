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
  geom_text(aes(label = label_text), hjust = -0.2, size = 4) +
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
  geom_text(aes(label = label_text), hjust = -0.2, size = 4) +
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




# # Overall after with mean score
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



# # Overall before with mean score
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



#======================================================

#New highlight

### Topic Distribution light themed
## General topic distribution

### General Topic Distribution
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
  geom_col(aes(alpha = eng_name %in% c(
      # "Public Monuments",
      "Tehran / Valiasr Street Life & Nostalgia",
      "City Theater Metro & Underpass",
      # "Valiasr Street Connectivity & Length",
      "Urban Management & Heritage Protection of City Theater",
      "Valiasr / Tajrish Tree Preservation & Urban Ecology"
      # "Metro Infrastructure & Tajrish Station"
      )),
      fill = "#5556a5", show.legend = FALSE) +
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 4,
            # angle = 45,
            color = "white",
            fontface = "italic") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.25), guide = "none") +
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
    plot.background  = element_rect(color = NA, fill = "gray20"), #gray20 for dark
    panel.grid.major  = element_line(color = "gray40"),  #gray40 for dark gray80 for light
    panel.grid.minor = element_line(color = "gray50"), #gray50 for dark gray90 for light
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 13, hjust = 1, color = "white"),
    axis.text.x = element_text(hjust = 0.9, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
  )


## Localised topic distribution
ggplot(topic_distribution_explicit, aes(x = eng_name, y = topic_count)) +
  geom_col(aes(alpha = eng_name %in% c(
    # "Public Monuments",
    "Tehran / Valiasr Street Life & Nostalgia",
    "City Theater Metro & Underpass",
    # "Valiasr Street Connectivity & Length",
    "Urban Management & Heritage Protection of City Theater",
    "Valiasr / Tajrish Tree Preservation & Urban Ecology"
    # "Metro Infrastructure & Tajrish Station"
    )),
    show.legend = FALSE, fill = "#5556a5") + #
  ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
  ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
  # Adding the labels as "call-outs"
  geom_text(aes(label = label_text), 
            hjust = -0.15, 
            size = 4,
            # angle = 45,
            color = "white", #white #black for light theme
            fontface = "italic") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.35), guide = "none") +
  # scale_fill_manual(values = c("TRUE" = "#faa819", "FALSE" = "#5556a5")) +
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
    panel.grid.major  = element_line(color = "gray40"),  #gray40 for dark #gray80 for light
    panel.grid.minor  = element_line(color = "gray50"), #gray 50 for dark # gray90 for light
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 13, hjust = 1, color = "white"),
    axis.text.x = element_text(hjust = 0.9, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
  )

### Highlight Localised Topic-Sentiment Distribution light
## Localised Topic Distribution per sentiments with fixed blocks dark themed
ggplot(topic_distribution_explicit_sentiment, 
       aes(x = eng_name, y = topic_count, fill = sentiment_label)) +
  # 1. THE FIX: position = "fill" makes all bars the same length (0 to 1)
  geom_col(
    aes(alpha = eng_name %in% c(
      # "Public Monuments",
      "Tehran / Valiasr Street Life & Nostalgia",
      "City Theater Metro & Underpass",
      # "Valiasr Street Connectivity & Length",
      "Urban Management & Heritage Protection of City Theater",
      "Valiasr / Tajrish Tree Preservation & Urban Ecology"
      # "Metro Infrastructure & Tajrish Station"
    )),
    position = "fill", show.legend = TRUE) +
  # 2. Adjust text to show proportions inside the blocks
  # Note: position_fill(vjust = 0.5) puts the text in the middle of each color block
  geom_text(aes(label = label_text), 
            position = position_fill(vjust = 0.5), 
            size = 3,
            color = "white", #white for dark
            fontface = "bold") +
  coord_flip() +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.35), guide = "none") +
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
    panel.grid.major = element_line(color = "gray40"), #gray40 for dark #gray80 for light
    panel.grid.minor = element_line(color = "gray50"), #gray50 for dark #gray90 for light
    text = element_text(color = "white", size = 13),
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 13),
    legend.position = "bottom",
    legend.background = element_rect(fill = "gray20", color = NA), #gray20 for dark #NA for light
    legend.text = element_text(color = "white", face = "bold")
  ) +
  labs(title = "Tehran Localised Spatial Topic-Sentiment Distribution",
       subtitle = "Tweet Frequency per Topic & Sentiments | Total Tweets: 1,794",
       x = "Spatial Topics",
       y = "Tweet Volume")

# install.packages("corrr")
# install.packages("igraph")
# install.packages("ggraph")
library(tidyverse)
library(dplyr)
library(ggplot2)
# library(igraph)
# library(ggraph)
# library(corrr)
# library(tidygraph)

spatial_columns <- c('prob_topic_0', 'prob_topic_1', 'prob_topic_2', 'prob_topic_4',
                     'prob_topic_5', 'prob_topic_11', 'prob_topic_12', 'prob_topic_13',
                     'prob_topic_16', 'prob_topic_18', 'prob_topic_19', 'prob_topic_21',
                     'prob_topic_22', 'prob_topic_22', 'prob_topic_30',
                     'prob_topic_31','prob_topic_32', 'prob_topic_37', 'prob_topic_41',
                     'prob_topic_46', 'prob_topic_52', 'prob_topic_55', 'prob_topic_76',
                     'prob_topic_82', 'prob_topic_84', 'prob_topic_86', 'prob_topic_88',
                     'prob_topic_93')

topic_to_place <- tribble(
  ~Topic, ~place_name,
  # --- City Theater Specifics ---
  "5",    "City Theater", 
  "13",   "City Theater", 
  "19",   "City Theater",
  
  # --- Tajrish Specifics ---
  "11",   "Tajrish Sqr.",
  "37",   "Tajrish Sqr.",
  "84",   "Tajrish Sqr.",
  
  # --- Valiasr Specifics ---
  "16",   "Valiasr St.",
  "18",   "Valiasr St.",
  "18",   "Tajrish Sqr.", # Ecology link
  "32",   "Valiasr St.",
  "86",   "Valiasr St.",
  "86",   "Tajrish Sqr.", # Connectivity link
  
  # --- Toopkhaneh Specifics ---
  "4",    "Toopkhaneh Sqr.",
  "41",   "Toopkhaneh Sqr.",
  
  # --- Hassan Abad ---
  "46",   "Hassan Abad Sqr.",
  
  # --- Vanak ---
  "1",    "Vanak Sqr.", # Based on 'Neighborhood Hubs' logic
  "86",   "Vanak Sqr.",
  # --- THE BRIDGES (Topics that go to multiple rows) ---
  "0",    "Valiasr St.",
  "0",    "Tehran Urban Spaces",
  "31",   "City Theater",     # Metro Station Entrances
  "31",   "Tajrish Sqr.",
  "31",   "Toopkhaneh Sqr.",
  "31",   "Hassan Abad Sqr.",
  
  "30",   "City Theater",     # Public Monuments
  "30",   "Toopkhaneh Sqr.",
  "30",   "Vanak Sqr.",
  
  "52",   "Toopkhaneh Sqr.",  # Historical Renaming
  "52",   "Hassan Abad Sqr.",
  "55",   "Toopkhaneh Sqr.",  # Historical Squares
  "55",   "Hassan Abad Sqr."
)


cleaned_tweets <- read.csv("output/Py_results/mostfinal_analysis_output.csv")
topics <- read.csv("output/Py_results/plot_topics_english.csv")

topic_labels <- topics|>
  # filter(explicity == "Explicit")|>
  select(Topic, eng_name, explicitness)|>
  mutate(Topic = as.character(Topic))|>
  unique()

place_links <- topic_labels|>
  left_join(topic_to_place, by = "Topic")|>
  mutate(place_name = case_when(!is.na(place_name) ~ place_name,
                                TRUE ~ "Tehran Urban Spaces"))

head(place_links)

place_links_explicit <- place_links|>
  filter(explicitness == "Explicit")


probs_df <- cleaned_tweets|>
  # filter(explicity == "Explicit")|>
  select(all_of(spatial_columns), Topic, explicitness, timeline)



### Place-Topic matrix for explicit topics

## Pivot probs for the overall structure
probs_long <- probs_df|>
  pivot_longer(starts_with("prob_topic_"), 
               names_to = "Topic_ID", 
               values_to = "score") |>
  mutate(Topic_ID = gsub("prob_topic_", "", Topic_ID))


head(probs_long)

spatial_matrix_data <- probs_long|>
  inner_join(place_links, by = c("Topic_ID" = "Topic"))|>
  group_by(place_name, eng_name, timeline)|>
  summarise(mean_prob = mean(score, na.rm = FALSE), .groups = "drop")

## it include Vanak in Tajrish-Valiasr walks
spatial_matrix_data24 <- probs_long|>
  inner_join(place_links, by = c("Topic_ID" = "Topic"))|>
  group_by(place_name, eng_name, timeline)|>
  summarise(mean_prob = mean(score, na.rm = FALSE), .groups = "drop")|>
  filter(timeline == "Before_24")

spatial_matrix_data26 <- probs_long|>
  inner_join(place_links, by = c("Topic_ID" = "Topic"))|>
  group_by(place_name, eng_name, timeline)|>
  summarise(mean_prob = mean(score, na.rm = FALSE), .groups = "drop")|>
  filter(timeline == "After_24")

# Build the Spatial DataFrame using an inner_join
# An inner_join will automatically drop any Topic_ID that isn't in your explicit map
spatial_matrix_explicit_data <- probs_long |>
  inner_join(place_links_explicit, by = c("Topic_ID" = "Topic")) |>
  # 3. Summarize
  group_by(place_name, eng_name, timeline) |>
  summarise(mean_prob = mean(score, na.rm = FALSE), .groups = "drop")|>
  group_by(eng_name, place_name)|>
  # To check if the avg scores are completely equal in both timelines
  mutate(
    is_equal =
      max(mean_prob) - min(mean_prob) < 0.001)|>
  ungroup()


spatial_matrix_explicit_data24 <- probs_long |>
  inner_join(place_links_explicit, by = c("Topic_ID" = "Topic")) |>
  group_by(place_name, eng_name, timeline) |>
  summarise(mean_prob = mean(score, na.rm = FALSE), .groups = "drop")|>
  filter(timeline == "Before_24")

spatial_matrix_explicit_data26 <- probs_long |>
  inner_join(place_links_explicit, by = c("Topic_ID" = "Topic")) |>
  group_by(place_name, eng_name, timeline) |>
  summarise(mean_prob = mean(score, na.rm = FALSE), .groups = "drop")|>
  filter(timeline == "After_24")

# For the avg score and localised topic shifts matrix
spatial_tweet_local <- cleaned_tweets|>
  filter(explicitness == "Explicit")|>
  # Group ONLY by topic and timeline to get the true average sentiment
  group_by(eng_name, sentiment_label, timeline) |> 
  summarise(
    topic_count = n(),
    avg_score = mean(sentiment_score, na.rm = TRUE), # True overall sentiment score
    .groups = "drop"
  ) |>
  group_by(timeline)|>
  mutate(
    timeline_total = sum(topic_count),
    topic_share = topic_count/timeline_total,
    normalized_score = avg_score * topic_share
  )|>
  ungroup()|>
  group_by(eng_name, sentiment_label)|>
  # To check if the avg scores are completely equal in both timelines
  mutate(
    is_equal =
      max(normalized_score) - min(normalized_score) < 0.01,
    is_equal_avg = 
      max(avg_score) - min(avg_score) < 0.05
    )|>
  ungroup()

###############
## Sentiment shifts based on weighted average scores!

ggplot(spatial_tweet_local, 
       aes(x = eng_name, y = sentiment_label, color = timeline)) +
  geom_point(aes(size = normalized_score, alpha = is_equal),
             position = position_dodge(width = 0.0)) +
  scale_alpha_manual(name = "Sentiment Stability",
                     values = c("FALSE" = 1.0, "TRUE" = 0.2),
                     labels = c("FALSE" = "Shifted / Changed", "TRUE" = "Stagnant / Equal"),
                     guide = "legend") +
  scale_x_discrete(
    name = "Sentiment Labels", # Setting the Axis Title
    labels = c(
      "negative" = "Negative",
      "neutral"  = "Neutral",
      "positive" = "Positive"
    )
  ) +
  # Making the differences look much more dramatic
  scale_size_continuous(
    # range = c(2, 15), name = "Mean Sentiment Score"
    trans = "exp", range = c(0, 25),
                        name = "Weighted Mean Sentiment Score", 
    ) + # Giving exponential size
  scale_color_manual(values = c("Before_24" = "#662d91", "After_24" = "#ffde17"),
                     ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
                     ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
                     labels = c("2024-2026", "2022-2024")) +
  # scale_fill_manual(values = NA) +
  theme_minimal() +
  theme(
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"), #gray20 for dark 
    panel.grid.major  = element_line(color = "gray40"),  #gray40 for dark #gray80 for light 
    panel.grid.minor  = element_line(color = "gray50"), #gray50 for dark #gray90 for light
    ## legend configurations
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA), # Box behind the dots
    legend.text = element_text(size = 12, color = "white"),
    legend.title = element_text(color = "white", face = "bold"),
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1, 
                               color = "white"),
    axis.text.y = element_text(size = 13, hjust = 2.5, color = "white", face = "bold"),
    axis.title = element_text(size = 14, color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white"),
    
    # legend.box.spacing = unit(0.1, "cm"),
    legend.margin = margin(r = 15, l = 10)) + # adjusting distances in legend box space
  guides(
    size = guide_legend(override.aes = list(color = "gray70")),
    color = guide_legend(override.aes = list(size = 10)),
    alpha = guide_legend(
      override.aes = list(
        # We use shape 21 (a circle that has both a FILL and a border color)
        shape = 21,
        size = 6,
        stroke = 1.5,                 # Thicker border line
        color = "#ffde17",          # The Border color (Blue)
        fill = "#662d91"            # The Center fill color (Orange)
        )) #blue 2d2e8b #orange faa819
    ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
  ) +
  labs(title = "Localised Discourse Shifts in Weighted Average Sentiment Scores",
       subtitle = "Between 2022-2026",
       x = "Spatial Topics",
       y = "Sentiment Labels",
       color = "Timeline")

ggsave(filename = "images/Place-Topic Matrix/Localised_Discourse_SentimentShift_Dark_Horizontal.jpg",
       last_plot(), 
       units = c("px"),
       dpi = 300)

#=====================================================================

### Sentiment Shifts based on Average Sentiments score
ggplot(spatial_tweet_local, 
       aes(x = eng_name, y = sentiment_label, color = timeline)) +
  geom_point(aes(size = avg_score, alpha = is_equal_avg),
             position = position_dodge(width = 0.0)) +
  scale_alpha_manual(name = "Sentiment Stability",
                     values = c("FALSE" = 1.0, "TRUE" = 0.2),
                     labels = c("FALSE" = "Shifted / Changed", "TRUE" = "Stagnant / Equal"),
                     guide = "legend") +
  scale_x_discrete(
    name = "Sentiment Labels", # Setting the Axis Title
    labels = c(
      "negative" = "Negative",
      "neutral"  = "Neutral",
      "positive" = "Positive"
    )
  ) +
  # Making the differences look much more dramatic
  scale_size_continuous(
    # range = c(2, 15), name = "Mean Sentiment Score"
    trans = "exp", range = c(0, 25),
    name = "Mean Sentiment Score", 
  ) + # Giving exponential size
  scale_color_manual(values = c("Before_24" = "#2d2e8b", "After_24" = "#faa819"),
                     ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
                     ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
                     labels = c("2024-2026", "2022-2024")) +
  # scale_fill_manual(values = NA) +
  theme_minimal() +
  theme(
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "white"), #gray20 for dark 
    panel.grid.major  = element_line(color = "gray80"),  #gray40 for dark #gray80 for light 
    panel.grid.minor  = element_line(color = "gray90"), #gray50 for dark #gray90 for light
    ## legend configurations
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA), # Box behind the dots
    legend.text = element_text(size = 13, color = "black"),
    legend.title = element_text(color = "black", face = "bold"),
    ## axis
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(size = 13, angle = 45, hjust = 1, 
                               color = "black"),
    axis.text.y = element_text(size = 13, hjust = 1.0, color = "black", face = "bold"),
    axis.title = element_text(size = 14, color = "black", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "black"),
    plot.subtitle = element_text(color = "black"),
    
    # legend.box.spacing = unit(0.1, "cm"),
    legend.margin = margin(r = 15, l = 10)) + # adjusting distances in legend box space
  guides(
    size = guide_legend(override.aes = list(color = "gray70")),
    color = guide_legend(override.aes = list(size = 10)),
    alpha = guide_legend(
      override.aes = list(
        # We use shape 21 (a circle that has both a FILL and a border color)
        shape = 21,
        size = 6,
        stroke = 1.5,                 # Thicker border line
        color = "#faa819",          # The Border color (Blue)
        fill = "#2d2e8b"            # The Center fill color (Orange)
      )) #blue 2d2e8b #orange faa819
    ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
    ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
  ) +
  labs(title = "Localised Discourse Shifts in Average Sentiment Scores",
       subtitle = "Between 2022-2026",
       x = "Spatial Topics",
       y = "Sentiment Labels",
       color = "Timeline")



spatial_matrix_data |> 
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_point(aes(size = mean_prob,
                 alpha = eng_name %in% c("Public Monuments",
                                         "Tehran / Valiasr Street Life & Nostalgia",
                                         "Seasonal Walks (Autumn in Valiasr)",
                                         # "City Theater Metro & Underpass",
                                         "Valiasr Street Connectivity & Length",
                                         # "Urban Management & Heritage Protection of City Theater",
                                         "Valiasr / Tajrish Tree Preservation & Urban Ecology",
                                         "Metro Station Entrances",
                                         "Tajrish-Valiasr Walks",
                                         "Performing Arts & Festivals (City Theater)",
                                         "Metro Infrastructure & Tajrish Station"),
                 show.legend = TRUE),
             color = "steelblue") +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.4),
                     guide = "none") +
  # facet_wrap(~factor(timeline, levels = c("Before_24", "After_24"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "General Spatial Discourse Between 2022-2026")


spatial_matrix_explicit_data |> 
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_point(aes(size = mean_prob,
                 alpha = eng_name %in% c("Public Monuments",
                                     "Tehran / Valiasr Street Life & Nostalgia",
                                     "Seasonal Walks (Autumn in Valiasr)",
                                     # "City Theater Metro & Underpass",
                                     "Valiasr Street Connectivity & Length",
                                     # "Urban Management & Heritage Protection of City Theater",
                                     "Valiasr / Tajrish Tree Preservation & Urban Ecology",
                                     "Metro Station Entrances",
                                     "Tajrish-Valiasr Walks",
                                     "Performing Arts & Festivals (City Theater)",
                                     "Metro Infrastructure & Tajrish Station"),
                 show.legend = TRUE),
                 color = "steelblue"
                 ) +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.4),
                     guide = "none") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Localised Spatial Discourse Between 2022-2026")



### highlighting the key places
spatial_matrix_data |> 
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_point(aes(size = mean_prob,
                 alpha = place_name %in% c("Tajrish Sqr.", "Valiasr St.",
                                           "City Theater", "Hassan Abad Sqr.",
                                           "Toopkhaneh Sqr."),
                 show.legend = TRUE),
             color = "steelblue"
  ) +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.2),
                     guide = "none") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "General Spatial Discourse Between 2022-2026")




spatial_matrix_explicit_data |> 
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_point(aes(size = mean_prob,
                 alpha = place_name %in% c("Tajrish Sqr.", "Valiasr St.",
                                           "City Theater"),
                 show.legend = TRUE),
             color = "steelblue"
  ) +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.2),
                     guide = "none") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Localised Spatial Discourse Between 2022-2026")




spatial_matrix_data26|>
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_point(aes(size = mean_prob, 
                 alpha = eng_name %in% c("Public Monuments",
                                            "Tehran / Valiasr Street Life & Nostalgia",
                                            "Seasonal Walks (Autumn in Valiasr)",
                                            # "City Theater Metro & Underpass",
                                            "Valiasr Street Connectivity & Length",
                                            # "Urban Management & Heritage Protection of City Theater",
                                            "Valiasr / Tajrish Tree Preservation & Urban Ecology",
                                            "Metro Station Entrances",
                                            "Tajrish-Valiasr Walks",
                                            "Performing Arts & Festivals (City Theater)",
                                            "Historical Renaming of Squares",
                                            "Historical Squares (Ashura)",
                                            "Metro Infrastructure & Tajrish Station"),
                 show.legend = TRUE),
             color = "steelblue") +
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25), 
                     guide = "none") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "General Spatial Discourse Between 2024-2026")
  # 22-24 / 24-26


spatial_matrix_data|>
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_point(aes(size = mean_prob, 
                 alpha = eng_name %in% c("Public Monuments",
                                         "Tehran / Valiasr Street Life & Nostalgia",
                                         "Seasonal Walks (Autumn in Valiasr)",
                                         # "City Theater Metro & Underpass",
                                         "Valiasr Street Connectivity & Length",
                                         # "Urban Management & Heritage Protection of City Theater",
                                         "Valiasr / Tajrish Tree Preservation & Urban Ecology",
                                         "Metro Station Entrances",
                                         "Tajrish-Valiasr Walks",
                                         "Performing Arts & Festivals (City Theater)",
                                         "Historical Renaming of Squares",
                                         "Historical Squares (Ashura)",
                                         "Metro Infrastructure & Tajrish Station"),
                 show.legend = TRUE),
             color = "#2d2e8b") +
  ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
  ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25), 
                     guide = "none") +
  scale_size_continuous(trans = "exp", range = c(0, 20),
                        name = "Probability Weight", ) +
  theme_minimal() +
  theme(
        ## panel & background
        panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = "white"), #gray20 for dark
        panel.grid.major  = element_line(color = "gray80"),  #gray40 for dark #gray80 for light 
        panel.grid.minor  = element_line(color = "gray90"), #gray50 for dark #gray90 for light
        ## legend configurations
        legend.background = element_rect(fill = NA, color = NA),
        legend.key = element_rect(fill = NA, color = NA), # Box behind the dots
        legend.text = element_text(size = 11, color = "black"),
        legend.title = element_text(size = 12, face = "bold",color = "black"),
        legend.key.size = unit(0.05, "cm"),
        ## axis
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14, hjust = 1.1, color = "black"),
        axis.title = element_text(size = 14, color = "black", face = "bold"),
        ## title and subtitle
        plot.title = element_text(color = "black"),
        plot.subtitle = element_text(color = "black"),
        ) +
  labs(title = "General Spatial Discourse Between 2022-2026",
       x = "Spatial Topics",
       y = "Key Urban Spaces"
       )


spatial_matrix_explicit_data|> 
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_point(aes(size = mean_prob,
                 alpha = eng_name %in% c("Public Monuments",
 "Tehran / Valiasr Street Life & Nostalgia",
 "Seasonal Walks (Autumn in Valiasr)",
  # "City Theater Metro & Underpass",
 "Valiasr Street Connectivity & Length",
  # "Urban Management & Heritage Protection of City Theater",
 "Valiasr / Tajrish Tree Preservation & Urban Ecology",
 "Metro Station Entrances",
 "Tajrish-Valiasr Walks",
 "Performing Arts & Festivals (City Theater)",
 "Metro Infrastructure & Tajrish Station"), show.legend = FALSE),
 color = "#faa819") +
  ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
  ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25), 
                     guide = "none") +
  
  scale_size_continuous(trans = "exp", range = c(0, 20),
                        name = "Probability Weight", ) +
 theme_minimal() +
  theme(
        ## panel & background
        panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = "white"), #gray20 for dark
        panel.grid.major  = element_line(color = "gray80"), #gray40 for dark #gray80 for light 
        panel.grid.minor  = element_line(color = "gray90"), #gray50 for dark #gray90 for light
        ## legend configurations
        legend.background = element_rect(fill = NA, color = NA),
        legend.key = element_rect(fill = NA, color = NA), # Box behind the dots
        legend.text = element_text(size = 11, color = "black"), 
        legend.title = element_text(size = 12, face = "bold", color = "black"), 
        ## axis
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14, hjust = 1.1, color = "black"),
        axis.title = element_text(size = 14, color = "black", face = "bold"),
        ## title and subtitle
        plot.title = element_text(color = "black"),
        plot.subtitle = element_text(color = "black"),
        ) +
  labs(title = "Localised Spatial Discourse Between 2022-2026",
       x = "Spatial Topics",
       y = "Key Urban Spaces")
  ## 22-24 / 24-26


## making the overlaying transformations for localised and general data in dark theme

ggplot(spatial_matrix_explicit_data, 
aes(x = eng_name, y = place_name, color = timeline)) +
  geom_point(
    aes(size = mean_prob, alpha = is_equal),
    position = position_dodge(width = 0.0)) +
  scale_alpha_manual(name = "Discourse Stability",
                     values = c("FALSE" = 1.0, "TRUE" = 0.2),
                     labels = c("FALSE" = "Shifted / Changed", "TRUE" = "Stagnant / Equal"),
                     guide = "legend") +
  # Making the differences look much more dramatic
  # scale_size_area(max_size = 15, name = "Probability Weight") +
  scale_size_continuous(trans = "exp", range = c(0, 20),
                        name = "Probability Weight") + # Giving exponential size
  scale_color_manual(values = c("After_24" = "#ffde17", "Before_24" = "#662d91"),
                     ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
                     ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
                     labels = c("2024-2026","2022-2024")) +
  theme_minimal() +
  theme(
        ## panel & background
        panel.background = element_rect(fill = NA, color = NA),
        plot.background  = element_rect(fill = "gray20"), #gray20 for dark 
        panel.grid.major  = element_line(color = "gray40"),   #gray40 for dark #gray80 for light
        panel.grid.minor  = element_line(color = "gray50"), #gray50 for dark #gray90 for light
        ## legend configurations
        legend.background = element_rect(fill = NA, color = NA),
        legend.key = element_rect(fill = NA, color = NA), # Box behind the dots
        legend.text = element_text(size = 11, color = "white"),
        legend.title = element_text(face = "bold", color = "white"),
        ## axis
        axis.text = element_text(color = "white"),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 1, color = "white"),
        axis.text.y = element_text(size = 14, hjust = 1.1, color = "white"),
        axis.title = element_text(size = 14, color = "white", face = "bold"),
        ## title and subtitle
        plot.title = element_text(color = "white"),
        plot.subtitle = element_text(color = "white"),
        
        # legend.box.spacing = unit(0.1, "cm"),
        legend.margin = margin(r = 15, l = 10)) + # adjusting distances in legend box space
  guides(
    size = guide_legend(override.aes = list(color = "gray70")), 
    color = guide_legend(override.aes = list(size = 5)),
    alpha = guide_legend(
      override.aes = list(
        # We use shape 21 (a circle that has both a FILL and a border color)
        shape = 21,
        size = 6,
        stroke = 1.5,                 # Thicker border line
        color = "#ffde17",          # The Border color (Blue)
        fill = "#662d91"            # The Center fill color (Orange)
      )) #blue 2d2e8b #orange faa819
    ) +
  labs(title = "Localised Discourse Shifts in Probability Weights",
       subtitle = "Between 2022-2026",
       x = "Spatial Topics",
       y = "Key Urban Spaces",
       color = "Timeline")



###################

# Under/Over Representation 
# Create the Global Baseline (The "Expected" frequency)
topic_baseline <- cleaned_tweets |>
  filter(explicitness == "Explicit") |>
  group_by(eng_name) |>
  summarise(global_count = n(), .groups = "drop") |>
  # Calculate the percentage of each topic relative to the WHOLE city
  mutate(
    global_total = sum(global_count),
    global_pct = (global_count / global_total) * 100) |>
  select(eng_name, global_pct) # We only need these two for the comparison


write.csv(topic_baseline, "output/Py_results/topic_baseline_reference.csv")
write.csv(representation_analysis, "output/Py_results/representation_analysis.csv")


# Compare against the City Theater (The "Actual" frequency)
representation_analysis <- cleaned_tweets |>
  filter(explicitness == "Explicit" & type == "ct") |> # Target the City Theater
  group_by(eng_name) |>
  summarise(ct_count = n(), .groups = "drop") |>
  mutate(
    ct_total = sum(ct_count),
    ct_pct = (ct_count / ct_total) * 100) |>
  # Join the baseline to calculate over/under representation
  left_join(topic_baseline, by = "eng_name") |>
  mutate(
    # The Indicator: Positive = Over-represented, Negative = Under-represented
    representation_index = ct_pct - global_pct,
    label_text = paste0(round(representation_index, 1), "%")
    ) |>
  mutate(rep_label = ifelse(representation_index > 0, "Over-represented",
                            "Under-represented"))|>
  mutate(eng_name = reorder(eng_name, representation_index))


ggplot(representation_analysis, aes(x = eng_name, y = representation_index,
                                    fill = representation_index > 0)) +
  # Map alpha to the logic (TRUE/FALSE)
  geom_col(aes(alpha = eng_name %in% c(
    "City Theater Metro & Underpass",
    "Tehran / Valiasr Street Life & Nostalgia",
    "Urban Management & Heritage Protection of City Theater",
    "Performing Arts & Festivals (City Theater)")),
    show.legend = TRUE) + # This stays TRUE so the 'fill' legend can appear
  
  geom_text(aes(label = label_text), 
            hjust = ifelse(representation_analysis$representation_index > 0, -0.1, 1.1), 
            size = 4, color = "white") +
  coord_flip() +
  
  # highlight the baseline
  geom_hline(yintercept = 0, color = "white", linewidth = 0.8) +
  
  # Manually define the fill legend
  scale_fill_manual(values = c("TRUE" = "#662d91", "FALSE" = "#ffde17"), 
                    ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
                    ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
                    labels = c("Under-represented", "Over-represented"),
                    name = "Representation Status") +
  # Manually define alpha and remove its legend specifically
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25), 
                     guide = "none") +
  labs(title = "Topic Representation: City Theater vs. Tehran Average",
       y = "Percentage Point Deviation from Baseline",
       x = "Spatial Topics") +
  theme(# Change the size of the title (e.g., "Representation Status")
    legend.title = element_text(size = 12, face = "bold", color = "white"), 
    # Change the size of the labels (e.g., "Over-represented")
    legend.text = element_text(size = 11, color = "white"),
    legend.background = element_blank(),
    strip.text = element_text(size = 11, face = "bold", color = "white"),
    strip.background = element_blank(),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20", color = NA),
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
    )

###############################################

topic_baseline26 <- cleaned_tweets |>
  filter(explicitness == "Explicit" & timeline == "After_24") |>
  group_by(eng_name) |>
  summarise(global_count = n(), .groups = "drop") |>
  mutate(
    global_total = sum(global_count),
    global_pct = (global_count / global_total) * 100) |>
  select(eng_name, global_pct) 



# Representation for 24
representation_analysis24 <- cleaned_tweets |>
  filter(explicitness == "Explicit" & type == "ct" & timeline == "Before_24") |>
  group_by(eng_name) |>
  summarise(ct_count = n(), .groups = "drop") |>
  mutate(
    ct_total = sum(ct_count),
    ct_pct = (ct_count / ct_total) * 100) |>
  # 3. JOIN the baseline to calculate over/under representation
  left_join(topic_baseline24, by = "eng_name") |>
  mutate(
    # The Indicator: Positive = Over-represented, Negative = Under-represented
    representation_index = ct_pct - global_pct,
    label_text = paste0(round(representation_index, 1), "%")
  ) |>
  mutate(rep_label = ifelse(representation_index > 0, "Over-represented",
                            "Under-represented"))|>
  mutate(eng_name = reorder(eng_name, representation_index))


# Plot representation for '24
ggplot(representation_analysis24, aes(x = eng_name, y = representation_index,
                                      fill = representation_index > 0)) +
  # Map alpha to the logic (TRUE/FALSE)
  geom_col(aes(alpha = eng_name %in% c(
    "City Theater Metro & Underpass",
    "Tehran / Valiasr Street Life & Nostalgia",
    "Urban Management & Heritage Protection of City Theater",
    "Public Monuments",
    "Performing Arts & Festivals (City Theater)")),
    show.legend = TRUE) + # This stays TRUE so the 'fill' legend can appear
  
  geom_text(aes(label = label_text), 
            hjust = ifelse(representation_analysis24$representation_index > 0, -0.1, 1.1), 
            size = 4, color = "white") +
  coord_flip() +
  
  # Manually define the fill legend
  scale_fill_manual(values = c("TRUE" = "#662d91", "FALSE" = "#ffde17"),
                    ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
                    ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
                    labels = c("Under-represented", "Over-represented"),
                    name = "Representation Status") +

  # highlight the baseline
  geom_hline(yintercept = 0, color = "white", linewidth = 0.8) +
  
  # Manually define alpha and remove its legend specifically
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25), 
                     guide = "none") +
  
  labs(title = "Topic Representation: City Theater vs. Tehran Average | 2022-2024",
       y = "Percentage Point Deviation from Baseline",
       x = "Spatial Topics") +
  theme_minimal() + 
  theme(# Change the size of the title (e.g., "Representation Status")
    legend.title = element_text(size = 12, face = "bold", color = "white"), 
    # Change the size of the labels (e.g., "Over-represented")
    legend.text = element_text(size = 11, color = "white"),
    legend.background = element_blank(),
    strip.text = element_text(size = 11, face = "bold", color = "white"),
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
    )


# Representation for 26
representation_analysis26 <- cleaned_tweets |>
  filter(explicitness == "Explicit" & type == "ct" & timeline == "After_24") |>
  group_by(eng_name) |>
  summarise(ct_count = n(), .groups = "drop") |>
  mutate(
    ct_total = sum(ct_count),
    ct_pct = (ct_count / ct_total) * 100) |>
  # Join the baseline to calculate over/under representation
  left_join(topic_baseline26, by = "eng_name") |>
  mutate(
    # The Indicator: Positive = Over-represented, Negative = Under-represented
    representation_index = ct_pct - global_pct,
    label_text = paste0(round(representation_index, 1), "%")
  ) |>
  mutate(rep_label = ifelse(representation_index > 0, "Over-represented",
                            "Under-represented"))|>
  mutate(eng_name = reorder(eng_name, representation_index))


ggplot(representation_analysis26, aes(x = eng_name, y = representation_index,
                                    fill = representation_index > 0)) +
  # Map alpha to the logic (TRUE/FALSE)
  geom_col(aes(alpha = eng_name %in% c(
    "City Theater Metro & Underpass",
    "Tehran / Valiasr Street Life & Nostalgia",
    "Urban Management & Heritage Protection of City Theater",
    "Public Monuments",
    "Performing Arts & Festivals (City Theater)")),
    show.legend = TRUE) + # This stays TRUE so the 'fill' legend can appear
  
  geom_text(aes(label = label_text), 
            hjust = ifelse(representation_analysis26$representation_index > 0, -0.1, 1.1), 
            size = 4, color = "white") +
  coord_flip() +
  
  # Manually define the fill legend
  scale_fill_manual(values = c("TRUE" = "#662d91", "FALSE" = "#ffde17"),
                    ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
                    ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
                    labels = c("Under-represented", "Over-represented"),
                    name = "Representation Status") +
  
  # Manually define alpha and remove its legend specifically
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25), 
                     guide = "none") +
  # Highlight the baseline
  geom_hline(yintercept = 0, color = "white", linewidth = 0.8) +
  
  labs(title = "Topic Representation: City Theater vs. Tehran Average | 2024-2026",
       y = "Percentage Point Deviation from Baseline",
       x = "Spatial Topics") +
  theme_minimal() + 
  theme(# Change the size of the title (e.g., "Representation Status")
    legend.title = element_text(size = 12, face = "bold", color = "white"), 
    # Change the size of the labels (e.g., "Over-represented")
    legend.text = element_text(size = 11, color = "white"),
    legend.background = element_blank(),
    strip.text = element_text(size = 11, face = "bold", color = "white"),
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
  )


# Overall representation with location quotient
representation_analysis_lq <- cleaned_tweets |>
  filter(explicitness == "Explicit" & type == "ct") |>
  group_by(eng_name) |>
  summarise(ct_count = n(), .groups = "drop") |>
  mutate(
    ct_total = sum(ct_count),
    ct_pct = (ct_count / ct_total) * 100) |>
  # Join the baseline to calculate over/under representation
  left_join(topic_baseline26, by = "eng_name") |>
  mutate(
    # The Indicator: Positive = Over-represented, Negative = Under-represented
    representation_index = ct_pct - global_pct,
    label_text = paste0(round(representation_index, 1), "%")
  ) |>
  mutate(lq = ct_pct / global_pct,
         label_lq = paste0(round(lq, 2)))|>
  mutate(rep_label = ifelse(representation_index > 0, "Over-represented",
                            "Under-represented"))|>
  mutate(lq_label = ifelse(lq > 1, "Specialized", "Not-specialized"))|>
  mutate(eng_name = reorder(eng_name, representation_index))

write.csv(representation_analysis_lq, "output/Py_results/under_over_lq.csv")

## Plot the lq
ggplot(representation_analysis_lq, aes(x = eng_name, y = lq - 1,
                                      fill = lq > 1)) +
  # 1. Map alpha to the logic (TRUE/FALSE)
  geom_col(aes(alpha = eng_name %in% c(
    "City Theater Metro & Underpass",
    "Tehran / Valiasr Street Life & Nostalgia",
    "Urban Management & Heritage Protection of City Theater",
    "Public Monuments",
    "Tajrish-Valiasr Walks",
    "Metro Station Entrances",
    "Performing Arts & Festivals (City Theater)")),
    show.legend = TRUE) + # This stays TRUE so the 'fill' legend can appear
  #Specify the baseline with value 1
  geom_hline(yintercept = 0, color = "white", linewidth = 0.8) + #white for dark
  geom_text(aes(label = label_lq), 
            hjust = ifelse(representation_analysis_lq$lq > 1, -0.1, 1.1), 
            size = 4, color = "white") +
  coord_flip() +
  # 2. MANUALLY DEFINE THE FILL LEGEND
  scale_fill_manual(values = c("TRUE" = "#662d91", "FALSE" = "#ffde17"),
                    ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
                    ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
                    labels = c("Not-specialized", "Specialized"),
                    name = "LQ Status") +
  # shifting the lq values to their original number
  scale_y_continuous(labels = function(x) x + 1) +
  # 3. MANUALLY DEFINE ALPHA AND KILL ITS LEGEND SPECIFICALLY
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25), 
                     guide = "none") +
  labs(title = "Topic Representation: City Theater vs. Tehran Average | 2022-2026",
       subtitle = "Based on Location Quotient value",
       y = "Percentage Point Deviation from Baseline",
       x = "Spatial Topics") +
  theme(# Change the size of the title (e.g., "Representation Status")
    legend.title = element_text(size = 12, face = "bold", color = "white"), #white for dark
    # Change the size of the labels (e.g., "Over-represented")
    legend.text = element_text(size = 12, color = "white"),
    legend.background = element_blank(),
    strip.text = element_text(size = 11, face = "bold", color = "white"),
    strip.background = element_blank(),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20", color = NA), #gray20 for dark
    panel.grid.major  = element_line(color = "gray40"),   #gray50 for dark #gray90 for light
    panel.grid.minor  = element_line(color = "gray50"), #gray40 for dark #gray80 for lights
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.y = element_text(size = 13, hjust = 1, color = "white"),
    axis.text.x = element_text(size = 13, hjust = 0.9, color = "white"),
    axis.title = element_text(size = 14, color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white")
  )

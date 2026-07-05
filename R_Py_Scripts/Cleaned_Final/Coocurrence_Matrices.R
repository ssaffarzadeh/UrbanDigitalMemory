# In R, install and run the libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

# Create a dataframe based on spatial topics and their 
# corresponding key places
topic_to_place <- tribble(
  ~Topic, ~place_name,
  #  City Theater Specifics 
  "5",    "City Theater", 
  "13",   "City Theater", 
  "19",   "City Theater",
  # Tajrish Specifics
  "11",   "Tajrish Sqr.",
  "37",   "Tajrish Sqr.",
  "84",   "Tajrish Sqr.",
  # Valiasr Specifics 
  "16",   "Valiasr St.",
  "18",   "Valiasr St.",
  "18",   "Tajrish Sqr.", 
  "32",   "Valiasr St.",
  "86",   "Valiasr St.",
  "86",   "Tajrish Sqr.", 
  # Toopkhaneh Specifics 
  "4",    "Toopkhaneh Sqr.",
  "41",   "Toopkhaneh Sqr.",
  # Hassan Abad 
  "46",   "Hassan Abad Sqr.",
  # Vanak 
  "1",    "Vanak Sqr.", # Based on 'Neighborhood Hubs' logic
  "86",   "Vanak Sqr.",
  # Topics that go to multiple tweets
  "0",    "Valiasr St.",
  "0",    "Tehran Urban Spaces",
  "31",   "City Theater",     
  "31",   "Tajrish Sqr.",
  "31",   "Toopkhaneh Sqr.",
  "31",   "Hassan Abad Sqr.",
  "30",   "City Theater",     
  "30",   "Toopkhaneh Sqr.",
  "30",   "Vanak Sqr.",
  "52",   "Toopkhaneh Sqr.",  
  "52",   "Hassan Abad Sqr.",
  "55",   "Toopkhaneh Sqr.",  
  "55",   "Hassan Abad Sqr."
)

# Load the data
cleaned_tweets <- read.csv("output/Py_results/mostfinal_analysis_output.csv")
topics <- read.csv("output/Py_results/plot_topics_english.csv")

# Convert the topic names into string(text) format directly
topic_labels <- topics|>
  select(Topic, eng_name, explicitness)|>
  mutate(Topic = as.character(Topic))|>
  unique()

# Join the place names to topic names
place_links <- topic_labels|>
  left_join(topic_to_place, by = "Topic")|>
  mutate(place_name = case_when(!is.na(place_name) ~ place_name,
                                TRUE ~ "Tehran Urban Spaces"))
probs_df <- cleaned_tweets|>
  select(all_of(spatial_columns), Topic, explicitness, timeline)

### Place-Topic matrix for localised topics
## Pivot probs for the overall structure
probs_long <- probs_df|>
  pivot_longer(starts_with("prob_topic_"), 
               names_to = "Topic_ID", 
               values_to = "score") |>
  mutate(Topic_ID = gsub("prob_topic_", "", Topic_ID))

# Creating the spatial matrix with explicit(localised) data for 
# both timelines
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
  # Summarize
  group_by(place_name, eng_name, timeline) |>
  summarise(mean_prob = mean(score, na.rm = FALSE), .groups = "drop")

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

# Create cooccurrence matrix plots
# General discourse matrix
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
             color = "#faa819") +
  ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
  ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
  scale_alpha_manual(values = c("TRUE" = 1.0, "FALSE" = 0.25), 
                     guide = "none") +
  scale_size_continuous(trans = "exp", range = c(0, 15),
                        name = "Probability Weight", ) +
  theme_minimal() +
  theme(
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray50"),   
    panel.grid.minor  = element_line(color = "gray60"),
    ## legend configurations
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA), # Box behind the dots
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.key.size = unit(0.05, "cm"),
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11, hjust = 1.1, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white"),
  ) +
  labs(title = "General Spatial Discourse Between 2022-2026",
       x = "Spatial Topics",
       y = "Key Urban Spaces"
  )
# Making the overlaying shifts for localised and general data in 
# dark theme. You can switch the data in the first item in the 
# bracket
ggplot(spatial_matrix_explicit_data, 
       aes(x = eng_name, y = place_name, color = timeline)) +
  geom_point(aes(size = mean_prob), 
             alpha = 1,
             position = position_dodge(width = 0.0)) +
  # Making the differences look much more dramatic
  # scale_size_area(max_size = 15, name = "Probability Weight") +
  scale_size_continuous(trans = "exp", range = c(0, 20),
                        name = "Probability Weight", ) + # Giving exponential size
  scale_color_manual(values = c("After_24" = "#ffde17", "Before_24" = "#662d91"),
                     ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
                     ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
                     labels = c("2024-2026","2022-2024")) +
  theme_minimal() +
  theme(
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray50"),   
    panel.grid.minor  = element_line(color = "gray60"),
    ## legend configurations
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA), # Box behind the dots
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1, color = "white"),
    axis.text.y = element_text(size = 11, hjust = 1.1, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white"),
    
    # legend.box.spacing = unit(0.1, "cm"),
    legend.margin = margin(r = 15, l = 10)) + # adjusting distances in legend box space
  guides(
    size = guide_legend(override.aes = list(color = "gray70")),
    color = guide_legend(override.aes = list(size = 5))
  ) +
  labs(title = "Localised Discourse Shifts in Probability Weights",
       subtitle = "Between 2022-2026",
       x = "Spatial Topics",
       y = "Key Urban Spaces",
       color = "Timeline")


### Sentiment shifts per topic between 2022-2026 ###
# For the average sentiment score and localised topic shifts 
# matrix 
spatial_tweet_local <- cleaned_tweets|>
  filter(explicitness == "Explicit")|>
  # Group only by topic and timeline to get the true average
  # sentiment
  group_by(eng_name, sentiment_label, timeline) |> 
  summarise(
    topic_count = n(),
    avg_score = mean(sentiment_score, na.rm = TRUE), 
    # True overall sentiment score
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
      max(normalized_score) - min(normalized_score) < 0.01)|>
  ungroup()

# Plot the shift matrix in dark theme
ggplot(spatial_tweet_local, 
       aes(x = eng_name, y = sentiment_label, color = timeline)) +
  geom_point(aes(size = normalized_score, alpha = is_equal),
             position = position_dodge(width = 0.0)) +
  scale_alpha_manual(values = c("FALSE" = 1.0, "TRUE" = 0.2),
                     guide = "none") +
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
    trans = "exp", range = c(0, 20),
    name = "Weighted Mean Sentiment Score", 
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
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray50"),   
    panel.grid.minor  = element_line(color = "gray60"),
    ## legend configurations
    legend.background = element_rect(fill = NA, color = NA),
    legend.key = element_rect(fill = NA, color = NA), # Box behind the dots
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    ## axis
    axis.text = element_text(color = "white"),
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1, 
                               face = "bold", color = "white"),
    axis.text.y = element_text(size = 11, hjust = 1.0, color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    ## title and subtitle
    plot.title = element_text(color = "white"),
    plot.subtitle = element_text(color = "white"),
    
    # legend.box.spacing = unit(0.1, "cm"),
    legend.margin = margin(r = 15, l = 10)) + # adjusting distances in legend box space
  guides(
    size = guide_legend(override.aes = list(color = "gray70")),
    color = guide_legend(override.aes = list(size = 5))
  ) +
  labs(title = "Localised Discourse Shifts in Weighted Average Sentiment Scores",
       subtitle = "Between 2022-2026",
       x = "Spatial Topics",
       y = "Sentiment Labels",
       color = "Timeline")



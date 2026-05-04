# install.packages("corrr")
# install.packages("igraph")
# install.packages("ggraph")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(corrr)
library(tidygraph)

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
  summarise(mean_prob = mean(score, na.rm = FALSE), .groups = "drop")


spatial_matrix_explicit_data24 <- probs_long |>
  inner_join(place_links_explicit, by = c("Topic_ID" = "Topic")) |>
  # 3. Summarize
  group_by(place_name, eng_name, timeline) |>
  summarise(mean_prob = mean(score, na.rm = FALSE), .groups = "drop")|>
  filter(timeline == "Before_24")

spatial_matrix_explicit_data26 <- probs_long |>
  inner_join(place_links_explicit, by = c("Topic_ID" = "Topic")) |>
  # 3. Summarize
  group_by(place_name, eng_name, timeline) |>
  summarise(mean_prob = mean(score, na.rm = FALSE), .groups = "drop")|>
  filter(timeline == "After_24")

head(spatial_matrix_data)

spatial_matrix_data |> 
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_point(aes(size = mean_prob), color = "steelblue", alpha = 0.6) +
  facet_wrap(~factor(timeline, levels = c("Before_24", "After_24"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "General Spatial Discourse Between 2022-2026")

spatial_matrix_data26|>
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_point(aes(size = mean_prob), color = "steelblue", alpha = 0.6) +
  # facet_wrap(~factor(timeline, levels = c("Before_24", "After_24"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "General Spatial Discourse Between 2024-2026")

spatial_matrix_explicit_data26|> 
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_point(aes(size = mean_prob), color = "steelblue", alpha = 0.6) +
  # facet_wrap(~factor(timeline, levels = c("Before_24", "After_24"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Explicit Spatial Discourse Between 2024-2026")



# 1. Create the Global Baseline (The "Expected" frequency)
topic_baseline <- cleaned_tweets |>
  filter(explicitness == "Explicit") |>
  group_by(eng_name) |>
  summarise(global_count = n(), .groups = "drop") |>
  # Calculate the percentage of each topic relative to the WHOLE city
  mutate(
    global_total = sum(global_count),
    global_pct = (global_count / global_total) * 100) |>
  select(eng_name, global_pct) # We only need these two for the comparison

# 2. Compare against the City Theater (The "Actual" frequency)
representation_analysis <- cleaned_tweets |>
  filter(explicitness == "Explicit" & type == "ct") |> # Target the City Theater
  group_by(eng_name) |>
  summarise(ct_count = n(), .groups = "drop") |>
  mutate(
    ct_total = sum(ct_count),
    ct_pct = (ct_count / ct_total) * 100) |>
  # 3. JOIN the baseline to calculate over/under representation
  left_join(topic_baseline, by = "eng_name") |>
  mutate(
    # The Indicator: Positive = Over-represented, Negative = Under-represented
    representation_index = ct_pct - global_pct,
    label_text = paste0(round(representation_index, 1), "%")
    ) |>
  mutate(eng_name = reorder(eng_name, representation_index))

ggplot(representation_analysis, aes(x = eng_name, y = representation_index,
                                    fill = representation_index > 0)) +
  geom_col(show.legend = TRUE) +
  geom_text(aes(label = label_text)) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#2ca25f", "FALSE" = "#de2d26"), 
                    labels = c("Under-represented", "Over-represented")) +
  labs(title = "Topic Representation: City Theater vs. Tehran Average",
       y = "Percentage Point Deviation from Baseline",
       x = "Spatial Topics") +
  theme_minimal()

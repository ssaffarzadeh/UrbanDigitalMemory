# install.packages("corrr")
# install.packages("igraph")
# install.packages("ggraph")
library(tidyverse)
library(dplyr)
library(ggplot2)
# library(igraph)
# library(ggraph)
# library(corrr)
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

write.csv(place_links, "output/Py_results/topic_links&spacenames.csv")
write.csv(topic_labels, "output/Py_results/topic_names_explicity.csv")


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



## it include Vanak in Tajrish-Valiasr walks
spatial_matrix_data2 <- probs_long|>
  inner_join(place_links, by = c("Topic_ID" = "Topic"))|>
  group_by(place_name, eng_name, timeline)|>
  summarise(mean_prob = mean(score, na.rm = TRUE), .groups = "drop")

# Build the Spatial DataFrame using an inner_join
# An inner_join will automatically drop any Topic_ID that isn't in your explicit map
spatial_matrix_explicit_data <- probs_long |>
  inner_join(place_links_explicit, by = c("Topic_ID" = "Topic")) |>
  # 3. Summarize
  group_by(place_name, eng_name, timeline) |>
  summarise(mean_prob = mean(score, na.rm = TRUE), .groups = "drop")


# Explicit heatmap

spatial_matrix_data |> 
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_point(aes(size = mean_prob, color = mean_prob)) +
  scale_size_continuous(range = c(2, 12)) +
  scale_color_gradient(low = "lightgray", high = "darkred") +
  facet_wrap(~timeline) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "General Spatial Discourse Intensity",
       subtitle = "Weights per place between 2022-2026",
       x = "Topic Name", y = "Location", size = "Intensity", color = "Intensity")

spatial_matrix_data2 |> 
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_col(aes(alpha = eng_name %in% c()))
  geom_point(aes(size = mean_prob), color = "steelblue", alpha = 0.6) +
  facet_wrap(~factor(timeline, levels = c("Before_24", "After_24"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "General Spatial Discourse Between 2022-2026")

spatial_matrix_explicit_data|> 
  ggplot(aes(x = eng_name, y = place_name)) +
  geom_point(aes(size = mean_prob), color = "steelblue", alpha = 0.6) +
  facet_wrap(~factor(timeline, levels = c("Before_24", "After_24"))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Explicit Spatial Discourse Between 2022-2026")




### Heatmap and plotting chunks

# Use the long-format data before we pivoted it wider
heatmap_data <- spatial_matrix_data |>
  inner_join(
    probs_df |> 
      pivot_longer(starts_with("prob_topic_"), names_to = "Topic_ID",
                   values_to = "score") |>
      mutate(Topic_ID = gsub("prob_topic_", "", Topic_ID)),
    by = c("Topic_ID" = "Topic_ID")
  ) |>
  group_by(place_name, eng_name) |>
  summarise(mean_score = mean(score), .groups = "drop")

# Plot

ggplot(heatmap_data, aes(x = eng_name, y = place_name, fill = mean_score)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Spatial Discourse Intensity",
       x = "Explicit Spatial Topics", y = "Place Name",
       fill = "Relation Strength")


ggplot(heatmap_data, aes(x = eng_name, y = place_name)) +
  # Use geom_point instead of geom_tile
  geom_point(aes(size = mean_score, color = mean_score)) + 
  # Control the size of the circles so they are visible
  scale_size_continuous(range = c(1, 15)) + 
  # Use a nice color gradient (e.g., from light gray to your theme color)
  scale_color_gradient(low = "lightgray", high = "#2c3e50") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "white") # Makes it look like a clean grid
  ) +
  labs(
    title = "Spatial Discourse Intensity",
    subtitle = "Circle size represents mean probability weight of the topic",
    x = "Explicit Topic", 
    y = "Place Name",
    size = "Intensity",
    color = "Intensity"
  )






### Preparing the data for co-occurence matrix

# # 3. Join this back to your probability data
# place_network_data <- probs_df |>
#   pivot_longer(starts_with("prob_topic_"), names_to = "Topic_ID", values_to = "prob") |>
#   filter(prob > 0.1) |> # Only keep meaningful mentions
#   mutate(Topic_ID = gsub("prob_topic_", "", Topic_ID)) |>
#   left_join(place_links, by = c("Topic_ID" = "Topic"))
# 
# # 4. Create the Graph: Topic <-> Place
# # This shows how different topics "anchor" to the same physical location
# place_graph <- as_tbl_graph(place_network_data |> select(eng_name, place_name) |> distinct())



### Testing the data for correlations

# # Calculate correlations but DO NOT filter yet
# topic_correlations_test <- probs_df |>
#   select(starts_with("prob_topic_")) |>
#   correlate() |>
#   stretch()
# 
# # Check the maximum correlation in your entire dataset
# # This will tell us if 0.05 was simply too high
# max_correlation <- max(topic_correlations_test$r, na.rm = TRUE)
# print(paste("Your highest correlation is:", max_correlation))
# 
# # Create the graph using ONLY the pairs that actually exist
# # (Ignoring NAs and self-correlations)
# topic_correlations_clean <- topic_correlations_test |>
#   filter(!is.na(r)) |>
#   filter(x != y) |> 
#   # Sort to see the strongest links
#   arrange(desc(r))
# 
# # Create the graph
# topic_graph <- as_tbl_graph(topic_correlations_clean, directed = FALSE)


# Filter for your timeline
before_df <- probs_df |> filter(timeline == "Before_24")
after_df <- probs_df |> filter(timeline == "After_24")

###Create the Binary Matrix (Topic is 'present' if weight > 0.05)

# # Using a lower threshold like 0.05 captures 'secondary' topics
binary_mtx <- after_df |>
  select(starts_with("prob_topic_")) |>
  mutate(across(everything(), ~ ifelse(. > 0.05, 1, 0))) |>
  as.matrix()

# Calculate Co-occurrence (How many tweets share Topic A and B)
co_matrix <- t(binary_mtx) %*% binary_mtx
diag(co_matrix) <- 0 # Remove self-links
colnames(co_matrix) <- gsub("prob_topic_", "", colnames(co_matrix))
rownames(co_matrix) <- colnames(co_matrix)

# Check how many connections we found
print(paste("Number of pairs that co-occur:", sum(co_matrix > 0) / 2))
# 
# ###
# ###
# 
# # Convert to graph
after_graph <- graph_from_adjacency_matrix(
  co_matrix, weighted = TRUE, mode = "undirected") |>
  as_tbl_graph()

# CRITICAL CHECK: Look at the 'name' column here
# It should show "0", "16", "29", etc.
print(after_graph)

# Join the labels (Ensuring types match)
topic_labels$Topic <- as.character(topic_labels$Topic)

before_graph <- after_graph |>
  activate(nodes) |>
  left_join(topic_labels, by = c("name" = "Topic"))
# 
# # 5. Check again - eng_name should now be populated
# print(before_graph)
# 
# # Plot
ggraph(before_graph, layout = "stress") +
  geom_edge_link(aes(width = weight, alpha = weight), color = "steelblue") +
  geom_node_point(aes(color = explicity), size = 6) +
  geom_node_text(aes(label = eng_name), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.5, 3)) + # Makes strong links thicker
  theme_void() +
  labs(title = "Topic Co-occurrence: Before 2024",
       subtitle = "Edges represent the number of tweets sharing topics")
# 
# 
# ### Filter the irrelevant matches
# 
# # Filter to keep only nodes with at least one connection
# before_graph_core <- before_graph |>
#   activate(nodes) |>
#   filter(!node_is_isolated())
# 
# ggraph(before_graph_core, layout = "nicely") +
#   geom_edge_link(aes(width = weight), alpha = 0.5, color = "steelblue") +
#   geom_node_point(aes(color = eng_name), size = 8) +
#   geom_node_text(aes(label = eng_name), 
#                  repel = TRUE, 
#                  size = 4, 
#                  fontface = "bold",
#                  max.overlaps = Inf) + # Forces all labels to show
#   theme_void() +
#   theme(legend.position = "none") +
#   labs(title = "Structural Core of Discourse (Before 2024)",
#        subtitle = "Displaying only co-occurring Farsi spatial topics")



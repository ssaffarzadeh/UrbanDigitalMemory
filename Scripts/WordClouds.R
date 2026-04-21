# 1. Load Libraries
# install.packages("ggwordcloud")
library(tidyverse)
library(ggwordcloud)

# 2. Load the Tidy Weights
# This file should have columns: Topic, Word, Score
# df_weights <- read.csv("output/Py_results/topic_weights_for_R.csv", encoding = "UTF-8")


# 1. Load data
df_eng <- read.csv("output/Py_results/plot_topics_english.csv")


# 3. Filter and Prepare
spatial_plot_data <- df_eng |>
  filter(explicity == "Explicit")|>
  mutate(eng_name = str_wrap(eng_name, width = 30)) # Wrap long names for better fit


# 4. Create the Plot
p_spatial <- ggplot(spatial_plot_data, aes(label = English_Word, size = Score, color = factor(Topic))) +
  geom_text_wordcloud_area(rm_outside = FALSE, shape = "circle") +
  scale_size_area(max_size = 25) + 
  facet_wrap(~eng_name, ncol = 5) + 
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold", color = "darkslategrey"),
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "Spatial and Urban Topics of Tehran",
    subtitle = "Analysis of explicit urban spaces, heritage, and daily street life"
  )

p_spatial
# 5. Save with high resolution
ggsave("Tehran's_Spatial_Analysis.png", plot = p_spatial, 
       width = 20, height = 18, units = "in", dpi = 300)

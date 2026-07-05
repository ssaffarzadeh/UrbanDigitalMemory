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
  filter(explicitness == "Explicit")|>
  mutate(eng_name = str_wrap(eng_name, width = 30)) # Wrap long names for better fit


# 4. Create the Plot
p_spatial <- ggplot(spatial_plot_data, aes(label = English_Word, size = Score, color = factor(Topic))) +
  geom_text_wordcloud_area(rm_outside = FALSE, shape = "circle") +
  scale_size_area(max_size = 25) + 
  facet_wrap(~eng_name, ncol = 4) + 
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold", color = "white"),
    ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
    ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
    #darkslategrey for white background
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "gray80"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray80"),
    strip.background = element_blank(),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "gray20"),
    panel.grid.major  = element_line(color = "gray50"),   
    panel.grid.minor  = element_line(color = "gray40"),
  ) +
  labs(
    title = "Spatial Topics of Tehran",
    subtitle = "Analysis of localised spatial discourse between 2022-2026"
  )

p_spatial

## Plot light theme
ggplot(spatial_plot_data, aes(label = English_Word, size = Score, color = factor(Topic))) +
  geom_text_wordcloud_area(rm_outside = FALSE, shape = "circle") +
  scale_size_area(max_size = 25) + 
  facet_wrap(~eng_name, ncol = 4) + 
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold", color = "black"),
    ## faa819 orange / ffde17 yellow / 2d2e8b blue / 662d91 purple
    ## 5556a5 light blue / 8c5d25 brown / d9534f red / 999999 grey / 5cb85c green
    #darkslategrey for white background
    legend.position = "none",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "black"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "black"),
    strip.background = element_blank(),
    ## panel & background
    panel.background = element_rect(fill = NA, color = NA),
    plot.background  = element_rect(fill = "white"),
    panel.grid.major  = element_line(color = "gray50"),   
    panel.grid.minor  = element_line(color = "gray40"),
  ) +
  labs(
    title = "Spatial Topics of Tehran",
    subtitle = "Analysis of localised spatial discourse between 2022-2026"
  )


# 5. Save with high resolution
ggsave("Tehran's_Spatial_Analysis.png", plot = p_spatial, 
       width = 20, height = 18, units = "in", dpi = 300)

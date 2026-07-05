# In R, install and run the libraries
library(tidyverse)
library(ggwordcloud)

# Load data
df_eng <- read.csv("output/Py_results/plot_topics_english.csv")

# Filter and Prepare
spatial_plot_data <- df_eng |>
  filter(explicitness == "Explicit")|>
  mutate(eng_name = str_wrap(eng_name, width = 30)) # Wrap long names for better fit
# Create the Plot
p_spatial <- ggplot(spatial_plot_data, aes(label = English_Word, size = Score, color = factor(Topic))) +
  geom_text_wordcloud_area(rm_outside = FALSE, shape = "circle") +
  scale_size_area(max_size = 25) + 
  facet_wrap(~eng_name, ncol = 4) + 
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
# Generate the plot by running this line
p_spatial

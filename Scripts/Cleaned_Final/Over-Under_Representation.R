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

# Save the baseline and representation layers
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

# Plot overall representation between ’22-26
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

# Plot the representation for ‘26
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

# Overall representation with Location Quotient (LQ)
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

# Plot the LQ
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
  geom_hline(yintercept = 0, color = "white", linewidth = 0.8) +
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

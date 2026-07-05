# In R, install and run the necessary libraries
library(tidyverse)
library(dplyr)

# Chi-square assessment

# Filter and table
before_24_matrix <- spatial_tweets |>
  filter(explicitness == "Explicit")|>
  filter(timeline == "Before_24") |>
  select(type, sentiment_label) |>
  table()

print("BEFORE 2024")
print(before_24_matrix)
# Chi-Square
chisq.test(before_24_matrix)

# Run for the After 2024 data
after_24_matrix <- spatial_tweets |>
  filter(explicitness == "Explicit")|>
  filter(timeline == "After_24") |>
  select(type, sentiment_label) |>
  table()

print("AFTER 2024")
print(after_24_matrix)
chisq.test(after_24_matrix)

# Combined timeline with localised topics
all_time_matrix <- spatial_tweets|>
  filter(explicitness == "Explicit")|>
  select(type, sentiment_label)|>
  table()

print("SCALES Comparison")
print(all_time_matrix)
chisq.test(all_time_matrix)

#############################################################
### Chi-square test only on City Theater scale
ct_matrix24 <- spatial_tweets|>
  filter(type == "ct" & timeline == "Before_24" & explicitness == "Explicit")|>
  select(timeline, sentiment_label)|>
  table()


ct_matrix26 <- spatial_tweets|>
  filter(type == "ct" & timeline == "After_24" & explicitness == "Explicit")|>
  select(timeline, sentiment_label)|>
  table()

print("CT timeline-sentiment comparison_24")
print(ct_matrix24)
chisq.test(ct_matrix24)

print("CT timeline-sentiment comparison_26")
print(ct_matrix26)
chisq.test(ct_matrix26)


### Chi-square test only on Not City Theater scale

not_ct_matrix24 <- spatial_tweets|>
  filter(type == "not_ct" & timeline == "Before_24" & explicitness == "Explicit")|>
  select(timeline, sentiment_label)|>
  table()

print("Not-CT timeline-sentiment comparison_24")
print(not_ct_matrix24)
chisq.test(not_ct_matrix24)


not_ct_matrix26 <- spatial_tweets|>
  filter(type == "not_ct" & timeline == "After_24" & explicitness == "Explicit")|>
  select(timeline, sentiment_label)|>
  table()

print("Not-CT timeline-sentiment comparison_26")
print(not_ct_matrix26)
chisq.test(not_ct_matrix26)


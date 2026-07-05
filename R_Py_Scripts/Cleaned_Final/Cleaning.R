# In R, install and run the necessary libraries

library(tidyverse)
library(tidytext)
library(dplyr)

# Load your data
raw_data <- read.csv("data/your_data_file.csv", stringsAsFactors = FALSE)

# Select the Text column and rename it to 'text'
raw_data <- raw_data |>
  mutate(copytext = fullText)

# Customize and modify the column names
colnames(raw_data)[2] <- "textshrt"
colnames(raw_data)[1] <- "text"

# Initial Clean: Remove noise but keep Farsi text intact; Removing # URLs, Hashtags, and tagging

clean1 <- raw_data |>
  mutate(
    # Remove URLs, Mentions, and Hashtags
    text = str_replace_all(text, "https?://\\S+|www\\.\\S+", ""),
    text = str_replace_all(text, "@\\w+", ""),
    text = str_replace_all(text, "#\\w+", ""),
    # Remove extra whitespace
    text = str_squish(text)
  )

# Remove rows(tweets) where the text is the same
deduplicated <- clean1 |>
  distinct(text, .keep_all = TRUE)

# Check how many tweets you lost
original_count <- nrow(clean1)
new_count <- nrow(deduplicated)
cat("Removed", original_count - new_count, "duplicate tweets.")

# Detect any foreign language in the dataframe
deduplicated <- deduplicated |>
  mutate(
    lang = detect_language(text)
  )

# Check how many Arabic/foreign language tweets you have
arabic_tweets <- deduplicated %>% filter(lang == "ar")
farsi_tweets <- deduplicated %>% filter(lang == "fa")
cat("Found", nrow(arabic_tweets), "Arabic tweets.")
unique(arabic_tweets$text)

# You may face some tweets that are actually Farsi, but some of
# them are detected as Arabic, you can identify the exact value
missed_tweets <- arabic_tweets|>
  filter(arabic_tweets$text == "من هروقت میرم مترو تئاترشهر:")
missed_tweets$lang <- "fa"

# Adding the missed fa tweet
farsi_tweets_filtered <- farsi_tweets|>
  add_row(missed_tweets)

# Organizing and choosing the desired columns to keep
cleaned_data_withkey <- farsi_tweets_filtered|>
  mutate(text2 = text, doc_id = row_number())|>
  select(text, text2, type, createdAt, doc_id)

# Export to Parquet (The "Memory-Safe" bridge) better for 
# processing data in Python
write_parquet(cleaned_data_withkey, "output/tweets_ready_withkey.parquet")

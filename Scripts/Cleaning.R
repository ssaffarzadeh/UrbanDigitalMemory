# install.packages("cld2")
# library(broom)
# library(reshape2)
library(tidyverse)
library(tidytext)
library(dplyr)
# library(topicmodels)
# library(stringr)
# library(stringi)
# library(wordcloud)
# library(RColorBrewer)
library(arrow)
library(cld2)

# Load your data
raw_data <- read.csv("data/tables/aggregated.csv", stringsAsFactors = FALSE)

# Select the Text column and rename it to 'text'
raw_data <- raw_data |>
  mutate(copytext = fullText)
 
colnames(raw_data)[2] <- "textshrt"
colnames(raw_data)[1] <- "text"


keywords <- c(
  "تئاتر شهر", 
  "تئاترشهر" ,
  "تئاتر_شهر", 
  "تئاتر شهر تهران",
  "تیاتر شهر",
  "تیاترشهر تهران", 
  "تئاترشهر تهران",
  "تئاترشهر تهران",
  "تئاترشهر_تهران",
  "تئاتر_شهر_تهران",
  "تیاترشهر_تهران",
  "تئاترشهرتهران",
  "تئاترشهر‌تهران",
  "تیاترشهرتهران",
  "تیاترشهر‌تهران",
  "میدان ونک",
  "میدون ونک",
  "میدان ونک تهران",
  "میدون ونک تهران",
  "ونک",
  "میدان_ونک_تهران",
  "ونک_تهران",
  "میدان_ونک",
  "تجریش",
  "میدان تجریش",
  "میدون تجریش",
  "مترو تجریش",
  "بازار تجریش",
  "میدان تجریش تهران",
  "میدون تجریش تهران",
  "بازار تجریش تهران",
  "میدان_تجریش_تهران",
  "میدان_تجریش",
  "مترو_تجریش",
  "بازار_تجریش",
  "بازار_تجریش_تهران",
  "خیابان ولیعصر",
  "ولیعصر تهران",
  "خیابون ولیعصر تهران",
  "خیابان ولیعصر تهران",
  "ولیعصر_تهران",
  "خیابان_ولیعصر_تهران",
  "میدان ولیعصر",
  "میدون ولیعصر",
  "خیابان_ولیعصر",
  "چهارراه ولیعصر",
  "تقاطع ولیعصر",
  "مترو تئاترشهر" ,
  "متروی تئاترشهر",
  "ایستگاه تئاترشهر",
  "میدان توپخانه",
  "میدان توپخونه",
  "میدون توپخونه",
  "میدون توپخانه",
  "میدان توپ‌خانه",
  "میدون توپ‌خانه",
  "میدون توپ‌خونه",
  "میدان توپ‌خونه",
  "میدان توپ خانه",
  "میدان توپ خونه",
  "میدون توپ خانه",
  "میدون توپ خونه",
  "میدان امام خمینی",
  "میدون امام خمینی",
  "میدان توپخانه تهران",
  "میدون توپخانه تهران",
  "میدان توپخونه تهران",
  "میدون توپخونه تهران",
  "میدان توپ خانه تهران",
  "میدون توپ خانه تهران",
  "میدون توپ خونه تهران",
  "میدان توپ خونه تهران",
  "میدان_توپخانه_تهران",
  "بازار بزرگ تهران",
  "بازار تهران",
  "بازار بزرگ",
  "سبزه میدان",
  "سبزه میدان تهران",
  "سبزه میدون تهران",
  "سبزه میدون",
  "بازار_تهران",
  "سبزه_میدان_تهران" ,
  "میدان حسن آباد تهران",
  "میدون حسن آباد تهران",
  "میدان حسن‌آباد تهران",
  "میدون حسن‌آباد تهران",
  "میدان حسنآباد تهران",
  "میدون حسنآباد تهران",
  "حسن آباد تهران",
  "حسن‌آباد تهران",
  "حسنآباد تهران",
  "حسن‌آباد_تهران",
  "میدان_حسن‌آباد_تهران",
  "تهران",
  "انقلاب",
  "خیابان انقلاب",
  "خیابون انقلاب",
  "میدان انقلاب",
  "میدون انقلاب"
  # "میدان",
  # "میدون"
)


# 2. Initial Clean: Remove noise but keep Farsi text intact: Removing URLs, Hashtags,

clean1 <- raw_data |>
  mutate(
    # Remove URLs, Mentions, and Hashtags
    text = str_replace_all(text, "https?://\\S+|www\\.\\S+", ""),
    text = str_replace_all(text, "@\\w+", ""),
    text = str_replace_all(text, "#\\w+", ""),
    # Remove extra whitespace
    text = str_squish(text)
  )

deduplicated <- clean1 |>
  # Remove rows where the text is exactly the same
  distinct(text, .keep_all = TRUE)
  # Optional: Also remove tweets that are identical after basic cleaning
  # (e.g., ignoring case or extra whitespace)
  # mutate(temp_check = str_squish(text)) |>
  # distinct(temp_check, .keep_all = TRUE) |>
  # select(-temp_check)

# 3. Check how many you lost
original_count <- nrow(clean1)
new_count <- nrow(deduplicated)
cat("Removed", original_count - new_count, "duplicate tweets.")

# Detec any foreign language in the dataframe
deduplicated <- deduplicated |>
  mutate(
    lang = detect_language(text)
  )

# 2. See how many Arabic tweets you have
arabic_tweets <- deduplicated %>% filter(lang == "ar")
farsi_tweets <- deduplicated %>% filter(lang == "fa")

cat("Found", nrow(arabic_tweets), "Arabic tweets.")

unique(arabic_tweets$text)

missed_tweets <- arabic_tweets|>
  filter(arabic_tweets$text == "من هروقت میرم مترو تئاترشهر:")

missed_tweets$lang <- "fa"

# Adding the missed fa tweet and making a column for tweets with no keywords called
# as "nokeywrds"
farsi_tweets_filtered <- farsi_tweets|>
  add_row(missed_tweets)


# Define function to remove keywords
remove_keywords <- function(text, keywords) {
  # Loop through each keyword and remove it from the text
  for (keyword in keywords) {
    text <- str_replace_all(text, fixed(keyword), " ")
  }
  # Collapse multiple spaces into a single space
  text <- str_replace_all(text, "\\s+", " ")
  # Trim leading/trailing whitespace
  text <- str_trim(text)
  return(text)
}

# Remove keywords from the 'text' column
farsi_tweets_filtered_nokeywrds <- farsi_tweets_filtered|>
  mutate(nokeywrds = text)
# 1. Apply the keyword removal within the dataframe
tweets_nokeywrds_final <- farsi_tweets_filtered_nokeywrds %>%
  mutate(
    # Use 'map_chr' or just a vectorized function to keep it in the df
    nokeywrds = map_chr(nokeywrds, ~ remove_keywords(.x, keywords = keywords))
  ) %>%
  
  # 2. Handling the "Empty String" problem
  mutate(
    # str_squish cleans up the extra spaces left behind by removed keywords
    nokeywrds = str_squish(nokeywrds)
  ) %>%
  
  # 3. Filter out empty or near-empty tweets
  # This ensures we don't send "empty" rows to ParsBERT
  filter(nokeywrds != "" & nchar(nokeywrds) > 2)

# Check the result
head(tweets_nokeywrds_final$nokeywrds)


head(farsi_tweets_filtered)

cleaned_data_withkey <- farsi_tweets_filtered|>
  mutate(text2 = text, doc_id = row_number())|>
  select(text, text2, type, createdAt, doc_id)

cleaned_data_nokey <- tweets_nokeywrds_final|>
  mutate(text = nokeywrds, doc_id = row_number())|>
  select(text, nokeywrds, doc_id, type, createdAt)



# 3. Export to Parquet (The "Memory-Safe" bridge)
write_parquet(cleaned_data_nokey, "output/tweets_ready_nokey.parquet")
write_parquet(cleaned_data_withkey, "output/tweets_ready_withkey.parquet")

write.csv(cleaned_data_withkey, "output/tweets_ready_withkey.csv")
write.csv(cleaned_data_nokey, "output/tweets_ready_nokey.csv")



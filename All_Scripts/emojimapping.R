# Emoji sentiment values
emoji_sentiment <- read.csv("data/Emoji_Sentiment_Data.csv", stringsAsFactors = FALSE)

emoji_sentiment <- emoji_sentiment |>
  mutate(Sentiment_score = (Positive - Negative) / (Positive + Neutral + Negative))

# Function to map emojis to emotional categories based on sentiment scores
map_emojis_to_categories <- function(emoji_sentiment) {
  emoji_map <- emoji_sentiment |>
    mutate(category =
             case_when(
               Sentiment_score > 0.5 ~ "شاد",
               Sentiment_score < -0.5 ~ "ناراحت",
               Sentiment_score > 0 ~ "مثبت",
               Sentiment_score < 0 ~ "منفی",
               TRUE ~ "neutral"
             )) |>
    select(Emoji, category) |>
    deframe()
  
  return(emoji_map)
}

# Create emoji map from sentiment dataset
emoji_map_from_sentiment <- map_emojis_to_categories(emoji_sentiment)

# Local emoticon dictionary
emoticon_map <- c(
  # Happy
  ":)" = "شاد",
  ":-)" = "شاد",
  ":))" = "شاد",
  ":)))" = "خیلی شاد",
  ":D" = "شاد",
  
  # Sad
  ":(" = "ناراحت",
  ":((" = "ناراحت",
  ":(((" = "بسیار ناراحت",
  ":((((" = "خیلی ناراحت",
  ":-(" = "ناراحت",
  ":-((" = "ناراحت",
  ":-(((" = "خیلی ناراحت",
  ":-((((" = "خیلی ناراحت",
  
  # Awkward
  ":\\" = "عجیب",
  ":-/" = "عجیب",
  ":/" = "عجیب",
  ":\\" = "عجیب",
  "://" = "عجیب",
  ":\\\\" = "خیلی عجیب",
  ":-//" = "عجیب",
  ":-\\" = "عجیب",
  ":|" = "عجیب",
  ":-|" = "عجیب",
  ":||" = "عجیب",
  ":-||" = "عجیب",
  ":|||" = "خیلی عجیب",
  ":-|||" = "عجیب"
)

# Combined emojis and emoticon map
combined_map <- c(emoji_map_from_sentiment, emoticon_map)


# Emoji replacement function
# replace_emoticons_emojis <- function(text) {
#   for (symbol in names(combined_map)) {
#     text <- str_replace_all(text, fixed(symbol), combined_map[[symbol]])
#   }
#   return(text)
# }

# Apply emoji and emoticon replacement
# clean_data$text <- sapply(clean_data$text, replace_emoticons_emojis)

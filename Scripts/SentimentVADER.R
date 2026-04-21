#install.packages("tidytext")
#install.packages("topicmodels")
# install.packages("textdata")
#install.packages("reshape2")
#install.packages("broom")
#install.packages(c("udpipe", "text2vec", "stm", "reticulate"))
#install.packages("reticulate")

#VADER
library(reticulate)
library(glue)

#wordembedding_included
library(udpipe)
library(text2vec)
library(stm)
library(reticulate)

#base_topicmodeling&sentiment
library(broom)
library(reshape2)
library(tidyverse)
library(tidytext)
library(dplyr)
library(topicmodels)
library(stringr)


# Install NLTK and VADER in Python
py_install("nltk")

py_run_string("
import nltk
nltk.download('vader_lexicon')
from nltk.sentiment.vader import SentimentIntensityAnalyzer
")


# Load your data
synth <- read.csv("data/Newest_tehran_city_theater_square_dataset.csv", stringsAsFactors = FALSE)

# Select the Text column and rename it to 'text'
synth <- select(synth, Text)
colnames(synth)[1] <- "text"

synth <- synth |>
  mutate(doc_id = row_number())|>
  mutate(document = remove())

view(synth)

# Define the emoji-to-text map
emoji_map <- c(
  "😊" = "happy",
  "😢" = "sad",
  "😠" = "angry",
  "😡" = "super angry",
  "😍" = "love",
  "🌿" = "nature",
  "😂" = "laughing",
  "😞" = "unhappy",
  "😎" = "cool",
  "😨" = "scared",
  "😤" = "frustrated",
  "💔" = "sad",      # Cracked heart emoji for sadness
  "❤️" = "love",    # Normal heart emoji for love
  "💖" = "love",     # Heart with sparks emoji for love
  "😐" = "neutral",
  "😕" = "confused",
  "😌" = "content",
  "😔" = "sorrowful",
  "😏" = "smirking",
  "😒" = "unamused",
  "😣" = "worried",
  "😥" = "disappointed",
  "😇" = "innocent",
  "😈" = "devilish",
  "🤔" = "uncertain",
  "🙄" = "awkward"
)


# Function to replace emojis with text
replace_emojis <- function(text) {
  for (emoji in names(emoji_map)) {
    text <- stringr::str_replace_all(text, fixed(emoji), emoji_map[[emoji]])
  }
  return(text)
}

# Apply emoji replacement
synth$text <- sapply(synth$text, replace_emojis)

synth

# Function to remove URLs
remove_urls <- function(text) {
  text <- stringr::str_replace_all(text, "https?://\\S+|www\\.\\S+", "")
  return(text)
}

# Function to remove @mentions (taggings)
remove_mentions <- function(text) {
  text <- stringr::str_replace_all(text, "@\\w+", "")
  return(text)
}

# Function to remove hashtags
remove_hashtags <- function(text) {
  text <- stringr::str_replace_all(text, "#\\w+", "")
  return(text)
}

# Apply all cleaning functions to the 'text' column
synth <- synth %>%
  mutate(
    text = remove_urls(text),     # Remove URLs
    text = remove_mentions(text), # Remove @mentions
    text = remove_hashtags(text), # Remove hashtags
    text = stringr::str_trim(stringr::str_replace_all(text, "\\s+", " ")) # Clean up whitespace
  )

view(synth)


# Tokenize the text
it <- itoken(synth$text, progressbar = FALSE)


# Define a function to run VADER sentiment analysis
vader_sentiment <- function(text) {
  py_run_string(glue::glue("
from nltk.sentiment.vader import SentimentIntensityAnalyzer
analyzer = SentimentIntensityAnalyzer()
text_var = '{text}'
scores = analyzer.polarity_scores(text_var)
"))
  sentiment_scores <- py$scores
  return(sentiment_scores$compound)
}


# Apply VADER sentiment analysis to each text
synth$vader_sentiment <- sapply(synth$text, vader_sentiment)

head(synth)
view(synth)


view(synth)

topic_text <- read.csv("output/topic_distribution_per_document_nokeywords.csv",
                       stringsAsFactors = FALSE)
view(topic_text)


###Change the col name
colnames(topic_text)[1] <- "doc_id"

###OR
topic_text <- topic_text |>
  rename(doc_id = document)

  

view(topic_text)

integrated <- synth |>
  left_join(topic_text, by = "doc_id")

write.csv(integrated, "output/topsent_integrated.csv", row.names = FALSE)
write.csv(integrated, "output/topsent_integrated_noid.csv", row.names = FALSE)
write.csv(synth, "vader_sentiment.csv", row.names = FALSE)

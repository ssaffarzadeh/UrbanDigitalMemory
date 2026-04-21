#install.packages("tidytext")
#install.packages("topicmodels")
# install.packages("textdata")
#install.packages("reshape2")
#install.packages("broom")
#install.packages(c("udpipe", "text2vec", "stm", "reticulate"))
#install.packages("word2vec")

#Install the BERtopic package from Py
# py_install("bertopic")
# py_install("pandas")
# py_install("scikit-learn")


# Load required libraries
library(tidyverse)
library(text2vec)
library(topicmodels)
library(tidytext)
library(stringr)
library(reticulate)
library(word2vec)



# Load your data
synth <- read.csv("data/Newest_tehran_city_theater_square_dataset.csv",
                  stringsAsFactors = FALSE)

# Select the Text column and rename it to 'text'
synth <- synth %>%
  select(Text) %>%
  rename(text = Text)

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
  "💔" = "sad",
  "❤️" = "love",
  "💖" = "love",
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
    text <- str_replace_all(text, fixed(emoji), emoji_map[[emoji]])
  }
  return(text)
}

# Function to remove URLs
remove_urls <- function(text) {
  str_replace_all(text, "https?://\\S+|www\\.\\S+", "")
}

# Function to remove @mentions (taggings)
remove_mentions <- function(text) {
  str_replace_all(text, "@\\w+", "")
}

# Function to remove hashtags
remove_hashtags <- function(text) {
  str_replace_all(text, "#\\w+", "")
}

# Remove the keyword
remove_keyword <- function(text) {
  text <- stringr::str_replace_all(text, "City Theater Square", "")
  return(text)
}

# Apply all cleaning functions to the 'text' column
synth <- synth %>%
  mutate(
    text = remove_urls(text),     # Remove URLs
    text = remove_mentions(text), # Remove @mentions
    text = remove_hashtags(text), # Remove hashtags
    text = remove_keyword(text),  # Remove keyword
    text = stringr::str_trim(stringr::str_replace_all(text, "\\s+", " ")) # Clean up whitespace
  ) 

|>
  mutate(doc_id = row_number())

tidy_text <- synth |>
  unnest_tokens(words, text)

# Create an iterator
it <- itoken(synth$text, preprocessor = tolower,
             progressbar = FALSE)



# Set up Python environment and import BERTopic
py_run_string("
from bertopic import BERTopic
from sklearn.feature_extraction.text import CountVectorizer
import pandas as pd
")

# Convert your R data to a Python list
py_synth <- r_to_py(synth$text)

# Use BERTopic for topic modeling
py_run_string("
vectorizer_model = CountVectorizer(stop_words='english')
topic_model = BERTopic()
topics, probs = topic_model.fit_transform(py_synth)
")

# Retrieve the topics and probabilities
topics <- py$topics
probs <- py$probs



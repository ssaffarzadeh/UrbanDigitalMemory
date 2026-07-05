#install.packages("tidytext")
#install.packages("topicmodels")
# install.packages("textdata")
#install.packages("reshape2")
#install.packages("broom")
#install.packages("wordcloud")
# install.packages("arrow")

library(broom)
library(reshape2)
library(tidyverse)
library(tidytext)
library(dplyr)
library(topicmodels)
library(stringr)
library(stringi)
library(reticulate)
library(wordcloud)
library(RColorBrewer)
library(arrow)

py_install("argostranslate")
py_install(c("transformers", "torch", "umap-learn", "hdbscan", "numpy"))
py_install("parsivar")
# py_install("hazm")


# Argos translator for Farsi
py_run_string("
import argostranslate.package
argostranslate.package.update_package_index()
available_packages = argostranslate.package.get_available_packages()
package_to_install = next(
    filter(
        lambda x: x.from_code == 'fa' and x.to_code == 'en',
        available_packages
    )
)
argostranslate.package.install_from_path(package_to_install.download())
")

# Parsivar as a pre-processor
py_run_string("
from parsivar import Normalizer, Tokenizer

# Initialize Parsivar tools
normalizer = Normalizer()
tokenizer = Tokenizer()

def preprocess_text(text):
    # Normalize text (convert Arabic characters to Farsi)
    normalized_text = normalizer.normalize(text)

    # Tokenize text into words
    tokens = tokenizer.tokenize_words(normalized_text)

    # Join tokens back into a single string (for further processing)
    cleaned_text = ' '.join(tokens)

    return cleaned_text
")


# Load ParsBERT and define embedding function
py_run_string("
from transformers import AutoTokenizer, AutoModel
import torch

model_name = 'HooshvareLab/bert-fa-base-uncased'
tokenizer = AutoTokenizer.from_pretrained(model_name)
model = AutoModel.from_pretrained(model_name)

def get_embeddings(texts):
    inputs = tokenizer(texts, return_tensors='pt', padding=True, truncation=True, max_length=512)
    with torch.no_grad():
        outputs = model(**inputs)
    embeddings = outputs.last_hidden_state.mean(dim=1).numpy()
    return embeddings
")

# Load your data
synth <- read.csv("data/tables/aggregated.csv", stringsAsFactors = FALSE)
head(synth)
# Select the Text column and rename it to 'text'
synth <- synth |>
  mutate(copytext = fullText)

colnames(synth)[2] <- "textshrt"

#synth <- select(synth, fullText)
colnames(synth)[1] <- "text"

# Emoji sentiment values
emoji_sentiment <- read.csv("data/Emoji_Sentiment_Data.csv", stringsAsFactors = FALSE)

head(emoji_sentiment)

emoji_sentiment <- emoji_sentiment |>
  mutate(Sentiment_score = (Positive - Negative) / (Positive + Neutral + Negative))
         
head(emoji_sentiment)

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
  "میدون انقلاب",
  "میدان",
  "میدون"
)
  

# Combined emojis and emoticon map
combined_map <- c(emoji_map_from_sentiment, emoticon_map)

head(synth)


# Function to remove multiple specific characters
remove_chars <- function(text, chars_to_remove) {
  for (char in strsplit(chars_to_remove, "")[[1]]) {
    text <- str_replace_all(text, fixed(char), " ")
  }
  return(text)
}

# Function to preprocess text (preserves Farsi text)
preprocess_text <- function(text) {
  # Define specific problematic characters to remove
  chars_to_remove <- "،؟؛«»\"'!()[]{}<>*"
  # Remove specific characters
  text <- remove_chars(text, chars_to_remove)
  # Collapse multiple spaces
  text <- str_replace_all(text, "\\s+", " ")
  # Trim leading/trailing whitespace
  text <- str_trim(text)
  return(text)
}


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

# Updated function
replace_emoticons_emojis <- function(text) {
  for (symbol in names(combined_map)) {
    text <- str_replace_all(text, fixed(symbol), combined_map[[symbol]])
  }
  return(text)
}


# Apply all cleaning functions to the 'text' column
synth <- synth |>
  mutate(
    text = remove_urls(text),     # Remove URLs
    text = remove_mentions(text), # Remove @mentions
    text = remove_hashtags(text), # Remove hashtags
    text = stringr::str_trim(stringr::str_replace_all(text, "\\s+", " ")) # Clean up whitespace
  )

# Remove keywords from the 'text' column
synth$text <- sapply(synth$text, remove_keywords, keywords = keywords)

# Apply emoji and emoticon replacement
synth$text <- sapply(synth$text, replace_emoticons_emojis)

# preprocess function for translation purposes
# synth$text <- sapply(synth$text, preprocess_text)

# Apply preprocessing for Farsi data
synth$text <- sapply(synth$text, function(text) {
  # Escape special characters in text
  escaped_text <- gsub("'", "\\\\'", text)
  escaped_text <- gsub('"', '\\"', escaped_text)
  
  py_run_string(paste0("cleaned_text = preprocess_text('", escaped_text, "')"))
  return(py$cleaned_text)
})

# Set the R vector as a Python variable
texts <- synth$text

py_run_string("
py_texts = list(r.texts)  # Convert R vector to Python list
")

# Utilizing word embedding in Py
py_run_string("
embeddings = get_embeddings(py_texts)
")


embeddings <- py$embeddings



# # Function to translate text from Farsi to English
# translate_text <- function(text) {
#   py_run_string(glue::glue('
#   import argostranslate.translate
#   from_code = "fa"
#   to_code = "en"
#   translated_text = argostranslate.translate.translate("{text}", from_code, to_code)
#   '))
#   return(py$translated_text)
# }

# Apply translation to each text
# synth$text <- sapply(synth$text, translate_text)



#Topic Modeling
dtm <- synth |>
  mutate(doc_id = row_number()) |> # Assign a unique document ID to each row
  unnest_tokens(word, text) |>      # Tokenize the text
  anti_join(stop_words) |>          # Remove stopwords
  count(doc_id,word) |>             # Count word occurrences per document
  cast_dtm(doc_id, word, n)        # Convert to a document-term matrix

calculate_coherence <- function(dtm, k, texts) {
  # Fit the LDA model
  lda_model <- LDA(dtm, k = k, control = list(seed = 1234))
  
  # Get the top terms for each topic
  top_terms <- tidy(lda_model, matrix = "beta") %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  # Calculate coherence (simplified example)
  # Note: This is a placeholder for a more sophisticated coherence calculation
  coherence <- top_terms %>%
    group_by(topic) %>%
    summarise(coherence = mean(beta)) %>%  # Simplified coherence metric
    pull(coherence) %>%
    mean()
  
  return(coherence)
}


k_values <- 2:15
coherence_scores <- sapply(k_values, function(k) calculate_coherence(dtm, k, synth$text))

# Plot coherence scores
data.frame(k = k_values, coherence = coherence_scores) %>%
  ggplot(aes(x = k, y = coherence)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Topics (k)", y = "Coherence Score",
       title = "Coherence Scores for Different k Values")

# Select the k with the highest coherence
best_k <- k_values[which.max(coherence_scores)]
print(paste("Optimal number of topics (k):", best_k))


# Fit the LDA model with the optimal k value
lda_model <- LDA(dtm, k = best_k, control = list(seed = 1234))


# Fit an LDA model with 3 topics
#lda_model <- LDA(tidy_text, k = 20, control = list(seed = 1234))

# View the top terms for each topic
terms(lda_model, 15)

# Extract top terms per topic
top_terms <- tidy(lda_model, matrix = "beta") |>
  group_by(topic) |>
  top_n(15, beta) |>
  ungroup() |>
  arrange(topic, -beta)


# Loop through each topic and create a word cloud
for (i in 1:best_k) {
  topic_terms <- top_terms %>%
    filter(topic == i) %>%
    select(term, beta)
  
  dev.new()  # Open a new plot window for each word cloud
  wordcloud(
    words = topic_terms$term,
    freq = topic_terms$beta,
    min.freq = 1,
    max.words = 100,
    random.order = FALSE,
    rot.per = 0.35,
    colors = brewer.pal(8, "Dark2"),
    main = paste("Topic", i)
  )
}

# Extract topic distribution per document
topic_distribution <- tidy(lda_model, matrix = "gamma") |>
  pivot_wider(names_from = topic, values_from = gamma, values_fill = 0)

topic_distribution <- tidy(lda_model, matrix = "gamma") %>%
  separate(topic, into = c("topic"), sep = "\\d+", remove = FALSE) %>%
  spread(topic, value, fill = 0)



str(top_terms)
view(top_terms)
view(topic_distribution)

topic_distribution <- topic_distribution |>
  mutate(doc_id = row_number())

view(topic_distribution)

view(top_terms)
# Save top terms to a CSV file
write.csv(top_terms, "output/top_terms_per_topic_nokeywords_aggregated_koptimal.csv",
          row.names = FALSE)

# Save topic distribution to a CSV file
write.csv(topic_distribution, "output/topic_distribution_per_document_nokeywords_
          aggregated_koptimal.csv",
          row.names = FALSE)



# ''''Sentiment Analysis''''

# Add document IDs
synth <- synth %>%
  mutate(doc_id = row_number())

# Tokenize and join with AFINN sentiment lexicon
tidy_data <- synth %>%
  unnest_tokens(word, text)

view(tidy_data)

#Write the cleaned text into txt file
# write.table(tidy_data |>
#               select("text"),
#             file = "synth.txt",
#             row.names = FALSE)

tidy_data %>%
  group_by(doc_id) %>%
  summarise(word_count = n()) %>%
  filter(word_count == 0)

sentiment_scores <- tidy_data %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(doc_id) %>%
  summarise(sentiment = sum(value))

# Merge sentiment scores back to original data
synth_with_sentiment <- synth %>%
  left_join(sentiment_scores, by = "doc_id")


# View the result
head(synth_with_sentiment)

synth_with_sentiment_filtered <- synth_with_sentiment |>
  filter(!is.na(sentiment))

view(synth_with_sentiment_filtered)

final <- synth_with_sentiment_filtered |>
  left_join(topic_distribution, by = "doc_id")

view(final)

final <- final |>
  select(doc_id,document,text, sentiment)

head(final)
view(final)

write.csv(final, "output/aggregated_final_koptimal.csv", row.names = FALSE)

# Export the result to a CSV file
write.csv(synth_with_sentiment, "sentiment_results.csv", row.names = FALSE)


Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

str(data)
head(data)

str(lda_model)

dput(head(data, 5))

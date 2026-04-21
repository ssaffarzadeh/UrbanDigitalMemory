library(tidyverse)
library(tidytext)
library(dplyr)
# library(topicmodels)
# library(stringr)
# library(stringi)
library(wordcloud)
library(RColorBrewer)
library(reticulate)
library(stringr)
library(stringi)

py_install("argostranslate")

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

# Function to translate text from Farsi to English
translate_text <- function(text) {
  py_run_string(glue::glue('
  import argostranslate.translate
  from_code = "fa"
  to_code = "en"
  translated_text = argostranslate.translate.translate("{text}", from_code, to_code)
  '))
  return(py$translated_text)
}

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

# Function for replacing keywords
replace_keywords <- function(text) {
  for (keyword in names(keywords_replace)) {
    text <- str_replace_all(text, fixed(keyword), keywords_replace[keyword])
  }
  return(text)
}

processed_data <- read.csv("output/Py_results/data_with_sentiment_key1_prob.csv")
# embedded_data2 <- read.csv("data/final_clean_results.csv")
# raw_data <- read.csv("data/tables/aggregated.csv")
topic_definitions <- read.csv("output/Py_results/topic_definitions_withkey_prob.csv")

keywords_replace <- c(
  "تئاتر شهر" = "City_theater", 
  "تئاترشهر" = "City_theater",
  "تئاتر_شهر" = "City_theater", 
  "تئاتر شهر تهران" = "City_theater",
  "تیاتر شهر" = "City_theater",
  "تیاترشهر تهران" = "City_theater", 
  "تئاترشهر تهران" = "City_theater",
  "تئاترشهر تهران" = "City_theater",
  "تئاترشهر_تهران" = "City_theater",
  "تئاتر_شهر_تهران" = "City_theater",
  "تیاترشهر_تهران" = "City_theater",
  "تئاترشهرتهران" = "City_theater",
  "تئاترشهر‌تهران" = "City_theater",
  "تیاترشهرتهران" = "City_theater",
  "تیاترشهر‌تهران" = "City_theater",
  "میدان ونک" = "Vanak",
  "میدون ونک" = "Vanak",
  "میدان ونک تهران" = "Vanak",
  "میدون ونک تهران" = "Vanak",
  "ونک" = "Vanak",
  "میدان_ونک_تهران" = "Vanak",
  "ونک_تهران" = "Vanak",
  "میدان_ونک" = "Vanak",
  "تجریش" = "Tajrish",
  "میدان تجریش"= "Tajrish",
  "میدون تجریش"= "Tajrish",
  "مترو تجریش"= "Tajrish",
  "بازار تجریش"= "Tajrish",
  "میدان تجریش تهران"= "Tajrish",
  "میدون تجریش تهران"= "Tajrish",
  "بازار تجریش تهران"= "Tajrish",
  "میدان_تجریش_تهران"= "Tajrish",
  "میدان_تجریش"= "Tajrish",
  "مترو_تجریش"= "Tajrish",
  "بازار_تجریش" = "Tajrish",
  "بازار_تجریش_تهران" = "Tajrish",
  "خیابان ولیعصر" = "Valiasr",
  "ولیعصر تهران" = "Valiasr",
  "خیابون ولیعصر تهران" = "Valiasr",
  "خیابان ولیعصر تهران" = "Valiasr",
  "ولیعصر_تهران" = "Valiasr",
  "خیابان_ولیعصر_تهران" = "Valiasr",
  "میدان ولیعصر" = "Valiasr_sqr",
  "میدون ولیعصر" = "Valiasr_sqr",
  "خیابان_ولیعصر" = "Valiasr",
  "چهارراه ولیعصر" = "City_theater",
  "تقاطع ولیعصر" = "City_theater",
  "مترو تئاترشهر" = "City_theater",
  "متروی تئاترشهر" = "City_theater",
  "ایستگاه تئاترشهر" = "City_theater",
  "میدان توپخانه" = "Toopkhaneh",
  "میدان توپخونه" = "Toopkhaneh",
  "میدون توپخونه" = "Toopkhaneh",
  "میدون توپخانه" = "Toopkhaneh",
  "میدان توپ‌خانه" = "Toopkhaneh",
  "میدون توپ‌خانه" = "Toopkhaneh",
  "میدون توپ‌خونه" = "Toopkhaneh",
  "میدان توپ‌خونه" = "Toopkhaneh",
  "میدان توپ خانه" = "Toopkhaneh",
  "میدان توپ خونه" = "Toopkhaneh",
  "میدون توپ خانه" = "Toopkhaneh",
  "میدون توپ خونه" = "Toopkhaneh",
  "میدان امام خمینی" = "Toopkhaneh",
  "میدون امام خمینی" = "Toopkhaneh",
  "میدان توپخانه تهران" = "Toopkhaneh",
  "میدون توپخانه تهران" = "Toopkhaneh",
  "میدان توپخونه تهران" = "Toopkhaneh",
  "میدون توپخونه تهران" = "Toopkhaneh",
  "میدان توپ خانه تهران" = "Toopkhaneh",
  "میدون توپ خانه تهران" = "Toopkhaneh",
  "میدون توپ خونه تهران" = "Toopkhaneh",
  "میدان توپ خونه تهران" = "Toopkhaneh",
  "میدان_توپخانه_تهران" = "Toopkhaneh",
  "بازار بزرگ تهران" = "Bazaar",
  "بازار تهران" = "Bazaar",
  "بازار بزرگ" = "Bazaar",
  "سبزه میدان" = "Bazaar",
  "سبزه میدان تهران" = "Bazaar",
  "سبزه میدون تهران" = "Bazaar",
  "سبزه میدون" = "Bazaar",
  "بازار_تهران" = "Bazaar",
  "سبزه_میدان_تهران" = "Bazaar",
  "میدان حسن آباد تهران" = "HassanAbad",
  "میدون حسن آباد تهران" = "HassanAbad",
  "میدان حسن‌آباد تهران" = "HassanAbad",
  "میدون حسن‌آباد تهران" = "HassanAbad",
  "میدان حسنآباد تهران" = "HassanAbad",
  "میدون حسنآباد تهران" = "HassanAbad",
  "حسن آباد تهران" = "HassanAbad",
  "حسن‌آباد تهران" = "HassanAbad",
  "حسنآباد تهران" = "HassanAbad",
  "حسن‌آباد_تهران" = "HassanAbad",
  "میدان_حسن‌آباد_تهران" = "HassanAbad",
  "تهران" = "Tehran",
  "انقلاب" = "Enghelab",
  "خیابان انقلاب" = "Enghelab_st",
  "خیابون انقلاب" = "Enghelab_st",
  "میدان انقلاب" = "Enghelab_sqr",
  "میدون انقلاب" = "Enghelab_sqr"
)

head(processed_data)

head(topic_definitions)

ready_data <- processed_data|>
  select(doc_id, createdAt, text,type, final_topic, 
         sentiment_label, sentiment_score) |>
  mutate(txtenglish = text)

head(topic_definitions)

topic_lists <-  topic_definitions|>
  mutate(name_eng = Name)|>
  mutate(represent_eng = Representation)


# Preprocess text and removing special 
# characters that affect the translation
ready_data$txtenglish <- sapply(ready_data$txtenglish, preprocess_text)

topic_lists$name_eng <- sapply(topic_lists$name_eng, preprocess_text)
topic_lists$represent_eng <- sapply(topic_lists$represent_eng, preprocess_text)

# Replace keywords
ready_data$txtenglish <- sapply(ready_data$txtenglish, replace_keywords)

topic_lists$name_eng <- sapply(topic_lists$name_eng, replace_keywords)
topic_lists$represent_eng <- sapply(topic_lists$represent_eng, replace_keywords)

# Apply translation to each text
translated <- ready_data

ready_topics <- topic_lists

# Translating the main data text into Eng
translated$txtenglish <- sapply(translated$txtenglish, translate_text)

# translating the topic definitions and names into Eng
ready_topics$name_eng <- sapply(ready_topics$name_eng, translate_text)
ready_topics$represent_eng <- sapply(ready_topics$represent_eng, translate_text)

# Making the topic definitions ready for export
topic_translated <- ready_topics

# Making the topic probs as the joinery element
probs <- select(processed_data, -createdAt, -text,-type, -final_topic, 
                                 -sentiment_label, -sentiment_score, -text2) 
head(probs)

# adding the probs fields to the data
full_data <- translated|>
  left_join(probs, by = "doc_id" )


write.csv(translated,"output/translateddata_key_probs_sentiemnts.csv")
write.csv(topic_translated,"output/translatedtopic_definitions_keyprob.csv")
write.csv(full_data, "output/full_data_translated_keyprob_sentiments.csv")

topic0 <- full_data|>
  filter(final_topic == 0)|>
  select(final_topic, text)
view(topic0)

tweets1 <- read.csv("output/Py_results/topic_definitions_don'tdelete.csv")
topicdefs <- read.csv("output/Py_results/topic_definitions_withkey_prob.csv")

display_definitions <- tweets1|>
  select(Topic, Count, Representation)|>
  write.csv("output/Py_results/topic_definitionsnotcleaned.csv")

display_definitions <- topicdefs|>
  select(Topic, Count, Representation)|>
  write.csv("output/Py_results/topic_definitionscleanedprobs.csv")




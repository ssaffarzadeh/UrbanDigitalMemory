# install.packages("corrr")
# install.packages("igraph")
# install.packages("ggraph")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(corrr)

spatial_columns <- c('prob_topic_0', 'prob_topic_1', 'prob_topic_2', 'prob_topic_4',
                     'prob_topic_5', 'prob_topic_11', 'prob_topic_12', 'prob_topic_13',
                     'prob_topic_16', 'prob_topic_18', 'prob_topic_19', 'prob_topic_21',
                     'prob_topic_22', 'prob_topic_22', 'prob_topic_29', 'prob_topic_30',
                     'prob_topic_31','prob_topic_32', 'prob_topic_37', 'prob_topic_41',
                     'prob_topic_46', 'prob_topic_52', 'prob_topic_55', 'prob_topic_76',
                     'prob_topic_82', 'prob_topic_84', 'prob_topic_86', 'prob_topic_88',
                     'prob_topic_93')

cleaned_tweets <- read.csv("output/Py_results/mostfinal_analysis_output.csv")

explicit_tweets <- cleaned_tweets|>
  filter(explicity == "Explicit")|>
  select(doc_id, text, txtenglish, Topic, eng_name, Count, 
         sentiment_label, explicity, type, timeline,
         all_of(spatial_columns))
probs_df <- explicit_tweets|>
  



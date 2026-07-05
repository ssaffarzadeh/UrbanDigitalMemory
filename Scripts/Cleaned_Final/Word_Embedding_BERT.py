# in Python, install & import the necessary libraries
import pandas as pd
import numpy as np
from parsivar import Normalizer
from bertopic import BERTopic
import torch
from transformers import AutoConfig, AutoTokenizer, AutoModelForSequenceClassification, pipeline



# Load Data
df = pd.read_parquet("tweets_ready_withkey.parquet")
docs = df['text'].astype(str).tolist()


#  Normalization (Parsivar)
# This handles the half-space (ZWNJ) and Arabic-to-Farsi character # conversion
normalizer = Normalizer()
docs = [normalizer.normalize(d) for d in docs]

# Word Embeddings & Topic Modeling (ParsBERT + BERTopic)
# We use BERTopic which can utilize ParsBERT under the hood.
# It handles UMAP (dimension reduction) and HDBSCAN (clustering).
topic_model = BERTopic(
    language="multilingual", # Supports Farsi context
    calculate_probabilities=True,
    verbose=True
)

# This creates the embeddings and finds topics automatically
topics, probs = topic_model.fit_transform(docs)


# Reduce Outliers
new_topics = topic_model.reduce_outliers(docs, topics, strategy="c-tf-idf")

# Update Topics with Stopwords
from sklearn.feature_extraction.text import CountVectorizer
farsi_stopwords = ["و", "در", "به", "از", "که", "با", "این", "است", "را", "آن"]
vectorizer_model = CountVectorizer(stop_words=farsi_stopwords)
topic_model.update_topics(docs, topics=new_topics, vectorizer_model=vectorizer_model)

# Create a dataframe from the probability matrix
prob_df = pd.DataFrame(probs).add_prefix('prob_topic_')

# Join it with your main data
df = pd.concat([df, prob_df], axis=1)

# Get the info for topics
topic_info = topic_model.get_topic_info()
print(topic_info.head(20))

# Save the run model in your project’s directory (So you never 
# have to run the embedding again)
topic_info.to_csv("topic_definitions_withkey_prob.csv", index=False)
df['final_topic'] = new_topics
df.to_csv("final_tweet_results_withkey_prob.csv", index=False)
topic_model.save("my_final_farsi_model_withkey_prob")

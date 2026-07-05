# In Python, import the necessary libraries
import pandas as pd
from transformers import pipeline
import torch

# Load the existing topic results
df = pd.read_csv("final_tweet_results_withkey_prob.csv")
docs = df['text'].astype(str).tolist()

# Setup the sentiment pipeline
# Point to the downloaded sentiment model
model_path = "ParsBERT-Sentiment/"
device = 0 if torch.cuda.is_available() else -1

# Run the sentiment analysis model as a pipeline
print("Running Sentiment Analysis")
sentiment_pipe = pipeline(
    "sentiment-analysis",
    model=model_path,
    tokenizer=model_path,
    device=device
)

# Execute in batches(For faster performance)
results = sentiment_pipe(docs, batch_size=32, truncation=True)

# Map the labels
# This fixes the 'recommended' value lable issue and standardizes 
# your labels
def map_sentiment(output):
    label = output['label'].lower()
    # Adjust these keys based on what your specific model outputs
    if "not" in label or "neg" in label:
        return "negative"
    elif "recom" in label or "pos" in label:
        return "positive"
    else:
        return "neutral"

df['sentiment_label'] = [map_sentiment(res) for res in results]
df['sentiment_score'] = [res['score'] for res in results]

# Save the final merged file
df.to_csv("data_with_sentiment_key1_prob.csv", index=False)
print("Sentiment appended to your existing topics.")

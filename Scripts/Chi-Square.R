# install.packages("chisq")
library(tidyverse)
library(ggwordcloud)
library(dplyr)

final_tweets <- read.csv("output/Py_results/mostfinal_analysis_output.csv")

# summarizing the values and columns
mixed_table <- table(final_tweets$timeline[final_tweets$type == "not_ct"], 
                  final_tweets$sentiment_label[final_tweets$type == "not_ct"])

# Running the Chi-Square
chi_results <- chisq.test(mixed_table)

print(chi_results)

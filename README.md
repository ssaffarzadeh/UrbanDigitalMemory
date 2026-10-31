[![License: PolyForm_Noncommercial_1.0.0](https://img.shields.io/badge/License-PolyForm_Noncommercial_1.0.0-purple)](https://polyformproject.org/licenses/noncommercial/1.0.0) [![License: GNU_General_Public_License_Version_3](https://img.shields.io/badge/License-GPLv3-yellow)](https://www.gnu.org/licenses/gpl-3.0.html)

# Urbanism Master Thesis Project
This repository contains the code for the urbanism master's graduation project from BK, TU Delft.

Please find the R & Python scripts in the /Scripts directory.
You can access the dataset on [4TU.Research Data](https://doi.org/10.4121/60d059e9-991f-4887-802f-3e28e41eb962.v1)

More information on the workflow and scripts is below.

# Urban Digital Memory: Leveraging Social Media Data on Public Spaces to Inform Urban Design Processes

### Keywords
*Digital Memory, Right to the city, Social Media, Public Engagement, Data-Driven Design, Tehran*

### Abstract
This study seeks to examine the capabilities of citizens’ lived experiences
shared on social media platforms as digital memories to value the spatial
features of public squares in Tehran. The evaluation is based on emotions
expressed by social media users about the public space, whether they have
positive, negative, neutral, or mixed impacts. The data will be collected as
text strings from X (formerly Twitter) using the Apify scraping app. The
collected text data went through topic modeling and sentiment analysis to
extract emotions in the form of negative, positive and neutral values. The
classified emotions were associated with the spatial features addressed
by tweet descriptions through a cognitive approach to spatial perception
and the expert’s interpretation. The valued associations shaped restrictive,
reconsidered, or no-action mitigations. At the end, a design framework was
proposed and applied to the case study. The project brought the concept of
right to the city, not only as a physical or mental entity, but also as a digital
memory, based on the fact that public space is validated by people in digital
spaces, which grants them the right to the memory as well. The other aspect of
the study would be the passive public information on design provided through
digital inputs, giving an initial overview to designers, planners, and decision
makers of an urban space.



## Data Translation

*If you have your raw data in a non-English language, you should use the "translate.R" script file.*

## Python model run for Topic Modeling & Sentiment Analysis

*For the Sentiment Analysis and Topic Modeling done by word embedding method, you're required to install the BERT model and other py libraries within the Python environement. You may find the BERT model and additional info [**here**](https://bertopic.com/). The remaining required libraries are available on the python script.*

## Recommended Data Analysis Workflow

The .RData files are not provided in this repo. Therefore, it is advised to run the script files along with the data based on the following order within the R and Python environments:

1.  Cleaning.R

    1.  To clean your raw data out of textual impurities, URLs, tagging, stop words, special characters or non-native languages that were accidentally collected.

2.  Translate.R

    1.  If your data is not in English

3.  Word_Embedding_BERT.py

4.  Sentiment_Analysis_BERT.py

5.  WordCloud_Visualisation.R

6.  Data_Visualisation.R

7.  Topic_Distribution.R

8.  *Coocurence_Matrices.R\**

9.  *Over-Under_Representation.R*\*

10. *Chi-Square_Assessment.R\**

*The \* scripts are meant for statistical assessments as additional analytical steps.*

### Additional Visualisations

For replicating the exact same visualisation steps, you can use the following R scripts stored in "Scripts/Extra_Visualisations" directory:

1.  Highlighting.R
2.  Visualisation.R

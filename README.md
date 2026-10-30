[![License: PolyForm_Noncommercial_1.0.0](https://img.shields.io/badge/License-PolyForm_Noncommercial_1.0.0-purple)](https://polyformproject.org/licenses/noncommercial/1.0.0) [![License: GNU_General_Public_License_Version_3](https://img.shields.io/badge/License-GPLv3-yellow)](https://www.gnu.org/licenses/gpl-3.0.html)

# Urbanism Master Thesis Project

Please find the R & Python scripts in the /Scripts directory

## Data Translation

*If you have your raw data in a non-english language, you should use the translate script file.*

## Python model run for Topic Modeling & Sentiment Anlysis

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

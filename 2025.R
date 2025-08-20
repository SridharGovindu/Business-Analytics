library(topicmodels)
library(lda)
library(slam)
library(stm)
library(ggplot2)
library(dplyr)
library(tidytext)
library(tm) # Framework for text mining
library(tidyverse) # Data preparation and pipes %>%
library(ggplot2) # For plotting word frequencies
library(wordcloud) # Wordclouds!
library(Rtsne)
library(rsvd)
library(geometry)
library(NLP)

# Clear up data in global environment
rm(list = ls())

# Load required library
library(readxl)

# Load data from Excel file
news <- read_excel("C:/Users/admin/Downloads/union_budget_2025(union_budget_2025)xl.xlsx")

# Check for NAs
sapply(news, function(x) sum(is.na(x)))

# Overview of original dataset
str(news)
sapply(news, typeof)


# Randomly sample 1000 rows & remove unnecessary columns
set.seed(830)
news_sample <- news[sample(nrow(news), 1048),]

# Convert categorical columns to factors (only if they exist)
if("publisher" %in% names(news_sample)) {
  news_sample$WEBSITE <- as.factor(news_sample$WEBSITE)
}

if("publisher" %in% names(news_sample)) {
  news_sample$COMMENT <- as.factor(news_sample$COMMENT)
}

# Convert LABEL into a factor (since it's categorical sentiment: -1, 0, 1, etc.)
news_sample$LABEL <- as.factor(news_sample$LABEL)

# Double-check format
sapply(news_sample, typeof)

# Text preprocessing with stm::textProcessor
processed <- textProcessor(news_sample$COMMENT, metadata = news_sample,
                           lowercase = TRUE,          # convert to lowercase
                           removestopwords = TRUE,    # remove stopwords
                           removenumbers = TRUE,      # remove numbers
                           removepunctuation = TRUE,  # remove punctuation
                           stem = TRUE,               # apply stemming
                           wordLengths = c(3, Inf),   # keep words of length >= 3
                           sparselevel = 1,           # keep all terms
                           language = "en",           # English
                           verbose = TRUE,            # print progress
                           onlycharacter = TRUE,      # remove tokens with numbers/symbols
                           striphtml = FALSE,         # don’t strip HTML (none in comments)
                           customstopwords = NULL,    # no extra stopwords
                           v1 = FALSE)                # default behavior


# Load required libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggcorrplot)

# 1. Summary Statistics

# Summary of numeric columns (only S.NO, LABEL etc. are numeric originally)
summary_stats <- news_sample %>%
  select_if(is.numeric) %>%
  summarise_all(list(
    mean = ~mean(. , na.rm = TRUE),
    median = ~median(. , na.rm = TRUE),
    sd = ~sd(. , na.rm = TRUE)
  ))

print(summary_stats)

# Mode function
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Mode for LABEL
mode_label <- get_mode(news_sample$LABEL)
cat("Mode of LABEL:", mode_label, "\n")

# 2. Data Distribution

# Histogram of LABEL
ggplot(news_sample, aes(x = LABEL)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  ggtitle("Distribution of Sentiment Labels")

# 3. Correlation Matrix / Heatmap

# Select only numeric columns
numeric_data <- news_sample %>% select_if(is.numeric)

if(ncol(numeric_data) > 1){
  cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  ggcorrplot(cor_matrix, lab = TRUE, title = "Correlation Heatmap")
} else {
  cat("Not enough numeric columns for correlation matrix.\n")
}

# 4. Notable Patterns / Trends

# Count of each sentiment label
label_counts <- table(news_sample$LABEL)
print(label_counts)

# Relative proportion
prop.table(label_counts)

# Load required library
library(ggplot2)

# Add comment length (in words and characters)
news$comment_length_words <- sapply(strsplit(news$COMMENT, "\\s+"), length)
news$comment_length_chars <- nchar(news$COMMENT)

# Histogram for word length
ggplot(news, aes(x = comment_length_words)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Comment Length (Words)",
       x = "Number of Words",
       y = "Frequency")

# Histogram for character length (optional)
ggplot(news, aes(x = comment_length_chars)) +
  geom_histogram(binwidth = 20, fill = "darkgreen", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Comment Length (Characters)",
       x = "Number of Characters",
       y = "Frequency")
library(tm)

# Copy dataset
news_cleaned <- news

# Clean COMMENT column
news_cleaned$COMMENT_CLEAN <- news_cleaned$COMMENT %>%
  tolower() %>%                        # lowercase
  removePunctuation() %>%              # remove punctuation
  removeNumbers() %>%                  # remove numbers
  removeWords(stopwords("en")) %>%     # remove stopwords
  stripWhitespace()                    # remove extra spaces

# Save to Excel
library(writexl)
write_xlsx(news_cleaned, "news_2024cleaned3.xlsx")

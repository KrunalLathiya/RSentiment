library(tidyverse)
library(tm)
library(tidytext)

# Load the crude dataset
data("crude")

# Convert the text to a corpus
corpus <- crude

# Preprocess the data
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Convert back to a data frame for tidytext operations
docs_clean <- data.frame(text = unlist(sapply(corpus, `[`, "content")))

# Tokenization
tokens <- docs_clean %>%
    unnest_tokens(word, text)

# Word frequencies
word_freq <- tokens %>%
    count(word, sort = TRUE)

# Displaying top 10 words
head(word_freq, 10)

# Analyze sentiment
sentiments <- docs_clean %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("bing"))

# Count sentiments
sentiment_counts <- sentiments %>%
    group_by(sentiment) %>%
    tally(sort = TRUE)

# Visualize sentiment distribution
sentiment_plot <- ggplot(sentiment_counts, aes(x = sentiment, y = n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    theme_minimal()

sentiment_plot
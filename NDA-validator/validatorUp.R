library(corpus)
library(qdap)
library(tm)
library(SnowballC)

text <- readLines("./demo.txt", encoding = "latin1")

test <- function(phr) {
  print(phr)
  return(phr)
}
###################################################################################
############################### 1. Data preparation ###############################
###################################################################################
sentences <- strsplit(text, split = "[.]")[[1]]
## Manual classification process
l_sentences <- c("A", "B", "B", "A", "C", "A", "A", "C", "C", "C", "B", "A", "C")
sentences_corpus <- VCorpus(VectorSource(sentences))
sentences_corpus <- tm_map(sentences_corpus, content_transformer(tolower))
sentences_corpus <- tm_map(sentences_corpus, removePunctuation)
sentences_corpus <- tm_map(sentences_corpus, removeWords, stopwords("spanish"))
sentences_corpus <- tm_map(sentences_corpus, stripWhitespace)
sentences_corpus <- tm_map(sentences_corpus, content_transformer(function(x, pattern) {
  return(paste0(x, "1"))
}))
sentences_corpus <- tm_map(sentences_corpus, removeNumbers)
sentences_corpus <- tm_map(sentences_corpus, content_transformer(print))
sentences_corpus <- tm_map(sentences_corpus, PlainTextDocument)
dtm <- DocumentTermMatrix(sentences_corpus)
dtm <- removeSparseTerms(dtm, 0.999)
bow_df <- as.data.frame(as.matrix(dtm))
bow_df$category <- l_sentences
bow_df

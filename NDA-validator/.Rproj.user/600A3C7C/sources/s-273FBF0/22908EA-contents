library(corpus)
library(qdap)
library(tm)
library(SnowballC)

text <- readLines("./demo.txt", encoding = "latin1")
lemma_dictionary <- read.delim("./lemmatization-es.txt", encoding = "UTF-8", header = FALSE, stringsAsFactors = FALSE)
names(lemma_dictionary) <- c("lemma", "term")


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
lemmas_parser <- new_stemmer(lemma_dictionary$term, lemma_dictionary$lemma)

sentences_corpus <- VCorpus(VectorSource(sentences))
sentences_corpus <- tm_map(sentences_corpus, content_transformer(tolower))
sentences_corpus <- tm_map(sentences_corpus, removePunctuation)
sentences_corpus <- tm_map(sentences_corpus, stripWhitespace)
sentences_corpus <- tm_map(sentences_corpus, content_transformer(function(x, pattern) {
  cl_x <- paste(unlist(text_tokens(x, stemmer = lemmas_parser, map_case = TRUE, drop = stopwords_es)), collapse = " ")
  cls_x <- paste(unlist(text_tokens(cl_x, stemmer = "es")), collapse = " ")
  return(cls_x)
}))
sentences_corpus <- tm_map(sentences_corpus, removeNumbers)
sentences_corpus <- tm_map(sentences_corpus, content_transformer(print))
dtm <- DocumentTermMatrix(sentences_corpus)
dtm <- removeSparseTerms(dtm, 0.999)
bow_df <- as.data.frame(as.matrix(dtm))
bow_df$category <- l_sentences
bow_df

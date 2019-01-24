library(corpus)
library(qdap)
library(tm)

text <- readLines("./demo.txt", encoding = "latin1")

###################################################################################
############################### 1. Data preparation ###############################
###################################################################################
phrases <- strsplit(text, split = "[.]")
## Manual classification process

## Data processing
p_dictionary <- read.delim("./lemmatization-es.txt", encoding = "UTF-8", header = FALSE, stringsAsFactors = FALSE)
names(p_dictionary) <- c("lemma", "term")
lemmas_parser <- new_stemmer(p_dictionary$term, p_dictionary$lemma)
# C: Clean, L: Lemmatized, S: Stemmed
cls_phrases <- list()
i <- 1
phrases
for (phrase in phrases[[1]]) {
  cl_phrase <- paste(unlist(text_tokens(phrase, stemmer = lemmas_parser, 
                                                map_case = TRUE, 
                                                drop = stopwords_es, 
                                                drop_punct = TRUE)), collapse = " ")
  cls_phrases[i] <- paste(unlist(text_tokens(cl_phrase, stemmer = "es")), collapse = " ")
  i <- i + 1
}
cls_phrases

# Categorization test
categoryA <- paste(unlist(c(cls_phrases[[1]], cls_phrases[[2]], cls_phrases[[5]], cls_phrases[[7]], cls_phrases[[13]])), collapse = " ")
categoryB <- paste(unlist(c(cls_phrases[[3]], cls_phrases[[4]], cls_phrases[[6]], cls_phrases[[8]], cls_phrases[[9]])), collapse = " ")
categoryC <- paste(unlist(c(cls_phrases[[10]], cls_phrases[[11]], cls_phrases[[12]])), collapse = " ")

# Measure frequences
freqA <- freq_terms(categoryA, 10)
freqB <- freq_terms(categoryB, 10)
freqC <- freq_terms(categoryC, 10)
plot(freqC)

# Creation of the VCorpuses
vSourceA <- VectorSource(categoryA)
VCorpusA <- VCorpus(vSourceA)
VSourceB <- VectorSource(categoryB)
VCorpusB <- VCorpus(vSourceB)
VSourceC <- VectorSource(categoryC)
VCorpusC <- VCorpus(vSourceC)

VCorpusC

# Creation of the Document Term Matrix (DTM)
categoryA_dtm <- DocumentTermMatrix(VCorpusA)
categoryA_m <- as.matrix(categoryA_dtm)
dim(categoryA_m)
categoryA_m

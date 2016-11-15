library(quanteda)
library(stm)
library(tm)
library(NLP)
library(openNLP)
library(ggplot2)
library(ggdendro)
library(cluster)
library(fpc)

require(quanteda)
Sdata <- read.csv("~/Desktop/NYU/Business Analytics/Data Sets/musicalinstrumets.csv" , header = TRUE, stringsAsFactors=FALSE)
View(Sdata)
Scorpus<- corpus(Sdata$reviewText,
                  docnames=Sdata$ID,
                  docvar= data.frame(Summary = Sdata$summary))
names(Sdata)
summary(Sdata)

Scorpus<- toLower(Scorpus, keepAcronyms = FALSE) #Converting to lower case

cleancorpus <- tokenize(Scorpus, 
                        removeNumbers=TRUE,  
                        removePunct = TRUE,
                        removeSeparators=TRUE,
                        removeTwitter=TRUE,
                        verbose=TRUE)      #

dfm.simple<- dfm(cleancorpus,
                 toLower = TRUE, 
                 ignoredFeatures =stopwords("english"), 
                 verbose=TRUE, 
                 stem=TRUE)   #Reducing to it's latin form

topfeatures<-topfeatures(dfm.simple, n=50) #Generating the maximum words being used
topfeatures

# compute the table of terms:
term.table <- table(unlist(cleancorpus))
term.table <- sort(term.table, decreasing = TRUE) #Mazimum number of usage, in decreasing order

#Cleaning corpus by removing unwanted words, SMART is inbuilt dictionary
stop_words <- stopwords("SMART")

## additional junk words showing up in the data
stop_words <- c(stop_words, "one", "the", "can", "will", "get","go","much","even","also")
stop_words <- tolower(stop_words) #reassuring lower case conversion

# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
term.table <- term.table[names(term.table) != ""] #removing the blank spaces
vocab <- names(term.table)


# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
doc <- lapply(cleancorpus, get.terms)

#############
# Compute some statistics related to the data set:
D <- length(doc)  # number of documents (1)
W <- length(vocab)  # number of terms in the vocab (1741)
doc.length <- sapply(doc, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (56196)
term.frequency <- as.integer(term.table) 

# MCMC and model tuning parameters:
K <- 10   #no. of topics
G <- 3000 #number of times the iterations to run
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = doc, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
## display runtime
t2 - t1  

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

ReviewtextLDA <- list(phi = phi,
                      theta = theta,
                      doc.length = doc.length,
                      vocab = vocab,
                      term.frequency = term.frequency)

library(LDAvis)
library(servr)

# create the JSON object to feed the visualization:
json <- createJSON(phi = ReviewtextLDA$phi, 
                   theta = ReviewtextLDA$theta, 
                   doc.length = ReviewtextLDA$doc.length, 
                   vocab = ReviewtextLDA$vocab, 
                   term.frequency = ReviewtextLDA$term.frequency)

serVis(json, out.dir = 'Sanjana Music Instruments, open.browser = TRUE)



# Sanjana-HW8


BA - Musical Instruments


With a document of more than 10,000 rows and with columns like "reviewerID","asin","reviewerName","helpful/0","helpful/1","reviewText" etc.. we have tried to make sense of the Customer Feedback.



TERM TABLE : term.table <- table(unlist(cleancorpus))
term.table <- sort(term.table, decreasing = TRUE)


JUNK WORDS : stop_words <- c(stop_words, "one", "the", "can", "will", "get","go","much","even","also")
stop_words <- tolower(stop_words)



After this we remove the words which occur less than five times and change the data format requested by the lda package


 
 LINK TO R CODE:
https://github.com/Sanjana093/Sanjana-HW8/blob/master/Sanjana%20Music%20Instruments.R

VISUALIZATION:
https://cdn.rawgit.com/Sanjana093/Sanjana-HW8/master/index.html


library(jiebaRD)
library(jiebaR)
library(wordcloud2)
options(warn=-1)
##
setwd("C:/Users/Pengfei/Desktop/272/file")
## define the stopwords
stopwords = c("or","of","and","in","from","the","but","it",
              "some","at","an","into","to","any","The","It",
              "a","out","This","this","That","that","And",
              "by","its","for","is","are","with","as","As",
              "them","we","We","be","may","they","their",
              "so","But","on","over","when","those","these",
              "no","which","not","if","else","To","where",
              "who","what","thus","A","Federalist","his","was",
              "I","has","he","had","have","In","will","can","would",
              "THE","upon","can","been","all","If","our","within",
              "under","were","other","do","yet","OF","other","than",
              "us","my")
filename = list.files()
cutter = worker()
total_word = NULL
for (i in 1:length(filename)){
  data = read.table(file = filename[i],sep="\n")
  n = nrow(data)
  for (j in 1:n){
    tex = segment(data[j,],cutter)
    total_word = c(total_word,tex)
  }
}
total_word = filter_segment(total_word,stopwords)
freq = freq(total_word)
freq = freq[order(-freq[,2]),]
count_word = freq[1:10,1]


topten = data.frame(order=seq(1,10))
number_of_words = data.frame(document = filename,number=rep(0,length(filename)))
words_matrix = matrix(0,85,10)
row.names(words_matrix) = filename
colnames(words_matrix) = count_word


for (i in 1:length(filename)){
  data = read.table(file = filename[i],sep="\n")
  n = nrow(data)
  words = NULL
  for (j in 1:n){
    tex = segment(data[j,],cutter)
    words = c(words,tex)
  }
  words = filter_segment(words,stopwords)
  freq = freq(words)
  freq = freq[order(-freq[,2]),]
  ## construct words matrix
  coun = rep(0,10)
  for (k in 1:10){
    if (count_word[k] %in% freq[,1]){
      coun[k] = freq[freq[,1]==count_word[k],2]
    }
  }
  words_matrix[i,] = coun
  ## print out the top 10 most frequent words
  topten[filename[i]] = freq[1:10,1]
  ## print out the number of words in each document
  number_of_words[i,2] = length(words)
}
topten = topten[,-1]
## answer to (1)
words_matrix = as.data.frame(words_matrix)
write.csv(words_matrix,"words matrix.csv")
## answer to (2)
topten
write.csv(topten,"topten.csv")
## answer to (3)
number_of_words
write.csv(number_of_words,"number of words.csv")
## answer to (4)
setwd("C:/Users/Pengfei/Desktop/272/Hamilton")
filename = list.files()
word_Hamilton = NULL
for (i in 1:length(filename)){
  data = read.table(file = filename[i],sep="\n")
  n = nrow(data)
  for (j in 1:n){
    tex = segment(data[j,],cutter)
    word_Hamilton = c(word_Hamilton,tex)
  }
}
word_Hamilton = filter_segment(word_Hamilton,stopwords)
freq_Hamilton = freq(word_Hamilton)
freq_Hamilton = freq_Hamilton[order(-freq_Hamilton[,2]),]
wordcloud2(freq_Hamilton)



setwd("C:/Users/Pengfei/Desktop/272/Madison")
filename = list.files()
word_Madison = NULL
for (i in 1:length(filename)){
  data = read.table(file = filename[i],sep="\n")
  n = nrow(data)
  for (j in 1:n){
    tex = segment(data[j,],cutter)
    word_Madison = c(word_Madison,tex)
  }
}
word_Madison = filter_segment(word_Madison,stopwords)
freq_Madison = freq(word_Madison)
freq_Madison = freq_Madison[order(-freq_Madison[,2]),]
wordcloud2(freq_Madison)


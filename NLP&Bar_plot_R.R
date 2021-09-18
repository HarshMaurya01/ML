bsnl<- read.csv(file.choose(),header = True)
str(bsnl)
# Build corpus
library(tm)
corpus <- iconv(bsnl$text, to="utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
# Clean text
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus, removePunctuation)

inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

# Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

#Bar plot
w <- rowSums(tdm)
w <- subset(w, w>=25)
w
barplot(w, las=2, col=rainbow(50))

# Word cloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words=250,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8,'Dark2'),
          #scale=c(5, 0.3),
          rot.per=0.7)

bsnl <- read.csv(file.choose(), header = T)
str(bsnl)

#Build corpus
#install.packages("NLP")
library(NLP)
library(tm)
corpus <- iconv(bsnl$text)
corpus <- Corpus(VectorSource(corpus))

#clean text
corpus<-iconv(bsnl$text, to="utf-8")
corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:5])
corpus<-tm_map(corpus, tolower)
inspect(corpus[1:5])
corpus<-tm_map(corpus, removePunctuation)
corpus<-tm_map(corpus, removeNumbers)
cleanset<-tm_map(corpus, removeWords, stopwords('english'))
cleanset<-tm_map(cleanset, removeWords, c('food','the', 'guys', 'himantabiswa', 'beef','got', 'people', 'abhishekkkk', 'voiceofaxom', 'anything', 'delivers', 'please', 'shaifalyca', 'recomendation', 'ufuf', 'zomatoipo', 'delivered', 'wtf', 'order', 'but', 'given', 'trp', 'motor', 'crore', 'use', 'boy', 'ofs','swiggy', 'swiggycares', 'will', 'sebi', 'get', 'ipomantra', 'amp', 'day', 'now', 'ipo', 'event', 'india', 're...', 'but...', 'can', 'payal', 'dum', 'crofs', 'now', 'one', 'he...', 'dates', 'speech', 'many'))
removeURL<-function(x) gsub ('http[[:alnum:]]*', '', x)
cleanset<-tm_map(cleanset, content_transformer(removeURL))
cleanset<-tm_map(cleanset, stripWhitespace)

#Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm<- as.matrix(tdm)
tdm <- tdm[row$Sums(tdm)>30,]
tdm[1:10,1:10]
# Network of terms
library(igraph)
tdm[tdm>1] <- 1
termM <- tdm%*% t(tdm)
termM <- termM[1:10,1:10]
g <- graph.adjacency(termM, weighted = T, mode = 'undirected')
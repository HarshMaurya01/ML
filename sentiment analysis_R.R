library(twitteR)
library("ROAuth")
library(dplyr)
library(tidytext)
library(tidyr)
library(tm)
library(topicmodels)
library(ggplot2)
library(magrittr)
library(wordcloud)
library(wordcloud2)
library(igraph)


api_key<-"LWTjhIOH8UnPCxwLW4BiHPUQ9"
api_secret<-"qngPdC66BuDeKLRznnaOlrBy0Eh4ICyAQPpoIZGgRK9QzVRZOa"
access_token<-"1402913873271791616-iZgNnMgHprKXNaZPizv0Cs06466XAU"
access_token_secret<-"uLTc0hE9PO9kp6ehVytffjYUXiwZih7uBl6w0ytX2DyDx"
setup_twitter_oauth(api_key,api_secret, access_token, access_token_secret)
tweets<-searchTwitter('@BSNLCorporate', n=1000 , lang='en')
tweets
tweetsdf<-twListToDF(tweets)
write.csv(tweetsdf, file = 'bsnl.csv',row.names = F)
params<-list(minDoFreq=1, removeNumbers=TRUE, stopwords=TRUE, stem=TRUE)
files<-lapply(file.choose(new=FALSE), readLines)
bsnl<-read.csv(file.choose(), header = T)
str(bsnl)
bsnl$text<-as.factor(bsnl$text)
str(bsnl)
corpus<-iconv(bsnl$text, to="utf-8")
corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:5])
corpus<-tm_map(corpus, tolower)
inspect(corpus[1:5])
corpus<-tm_map(corpus, removePunctuation)
corpus<-tm_map(corpus, removeNumbers)
cleanset<-tm_map(corpus, removeWords, stopwords('english'))
cleanset<-tm_map(cleanset, removeWords, c('ufcf', 'cmofkarnata...', 'pmoindia', 'get', 'since', 'cmofkarnataka', 'happen', 'day', 'cansathe', 'man', 'msdhoni', 'birthday', 'happybirthdaymsdhoni', 'dhonis', 'vijayachakram', 'maruticorp', 'bollystars', 'tatamotors', 'its', 'jio', 'dont', 'ugly', 'sushantify', 'chingssecret', 'bingosnacks', 'kotakbankltd', 'itsshilpasamai', 'boycott', 'makemytrip', 'policybazaar', 'airtelindia', 'levis', 'still', 'c...', 'salmansbrigade', 'lets', 'damngirll', 'kind', 'many', 'much', 'zeyauln', 'unless', 'indias', 'just', 'like', 'bcci', 'team', 'saal', 'sir', 'captain', 'cool','airtelnews', 'airtelpresence', 'hello', 'modi','shah', 'doval', 'rsprasad', 'bjp', 'pakistan', 'china', 'congress', 'airtel', 'idea', 'vodafone','vi', 'adani', 'will', 'now', 'bsnl', 'is', 'am', 'are', 'were', 'was', 'the', 'there', 'them', 'then', 'can', 'scam', 'fraud', 'friend', 'corrupt', 'lie', 'truth', 'spam', 'govt', 'india'))
removeURL<-function(x) gsub ('http[[:alnum:]]*', '', x)
cleanset<-tm_map(cleanset, content_transformer(removeURL))
cleanset<-tm_map(cleanset, stripWhitespace)

tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm <- tdm[rowSums(tdm)>30,]
tdm[1:10, 1:10]
#Network of terms
tdm[tdm>1] <- 1
termM <- tdm %*% t(tdm)
termM[1:10,1:10]
g <- graph.adjacency(termM,mode = 'undirected')
g
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

#Histogram of node degree
hist(V(g)$degree,
     breaks = 100,
     col = 'green',
     main = 'Histogram of Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')

#Network diagram
set.seed(222)
plot(g) 
plot(g,
     vertex.color='green',
     vertex.size = 4,
     vertex.label.dist = 1.5,
     vertex.label = NA)

#Community detection
comm <- cluster_edge_betweenness(g)
plot(comm, g)

prop <- cluster_label_prop(g)
plot(prop, g)

greed <- cluster_fast_greedy(as.undirected(g))
plot(greed, as.undirected(g))

#Hub and authorities
hs <- hub_score(g, weights = NA)$vector
as <- authority_score(g, weights=NA)$vector
par(mfrow=c(1,2))
plot(g, vertex.size=hs*30, main='Hubs',
     vertex.label=NA,
     vertex.color=rainbow(50))
plot(g, vertex.size=as*30, main='Authorities',
     vertex.label=NA,
     vertex.color=rainbow(50))

# Highlighting degrees
V(g)$label.cex <- 2.2*V(g)$degree / max(V(g)$degree) + 0.3
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+ .4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
plot(g,
     vertex.color='green',
     vertex.size = V(g)$degree*.5)

#Network of tweets
tweetM <- t(tdm) %*% tdm
g <- graph.adjacency(tweetM, mode = 'undirected')
V(g)$degree <- degree(g)
g <- simplify(g)
hist(V(g)$degree,
     breaks = 100,
     col = 'green',
     main = 'Histogram of Degree',
     ylab = 'Frequency',
     xlab = 'Degree')

#Set labels of vertices to tweet IDs
V(g)$label <- V(g)$name
V(g)$label.cex <- 1
V(g)$label.color <- rgb(.4, 0, 0, .7)
V(g)$size <- 4
V(g)$frame.color <- NA
plot(g, vertex.label=NA, vertex.size=6)

#Delete vertices
egam <- (log(E(g)$weight)+.2)/ max(log(E(g)$weight)+ .2)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
g2 <- delete.vertices(g, V(g)[degree(g)<50])
plot(g2,
     vertex.label.cex = .9,
     vertex.label.color = 'black')

# Delete edges
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
g3 <- delete.edges(g, E(g)$weight <- 1)
g3 <- delete.vertices(g3, V(g3)[degree(g3)<20])
plot(g3)

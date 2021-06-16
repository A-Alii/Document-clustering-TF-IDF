# import library
library(cluster)
library(tm) # text mining in tm package weightTf, weightTfIdf
library(SnowballC) # text stemmers
library(colorspace) # toolbox for Colors and Palettes
library(proxy) # Distance and Similarity Measures


# Cleaning environment
rm(list = ls()) 

# prepare dataframe
options(header = FALSE, stringsAsFactors = FALSE, fileEncoding = "latin1")
dataframe <- data.frame(ID=character(),
                        datetime=character(),
                        content=character(),
                        label=factor())

target.directory <- 'C:/Users/ahmed/OneDrive/Desktop'
target.directory <- paste(target.directory, 'Health-Tweets', sep = '/')
files <- list.files(path = target.directory, pattern='.txt$')

# load dataset to dataframe
for (f in files) {
  news.filename = paste(target.directory , f, sep ='/')
  news.label <- substr(f, 0, nchar(f) - 4) # Removing the 4 last characters => '.txt'
  news.data <- read.csv(news.filename,
                        encoding = 'UTF-8',
                        header = FALSE,
                        quote = "",
                        sep = '|',
                        col.names = c('ID', 'datetime', 'content'))
  news.data <- news.data[news.data$content != "", ] # get column has contain data
  news.data['label'] = news.label # We add the label of the tweet 
  news.data <- head(news.data, floor(nrow(news.data) * 0.05))
  dataframe <- rbind(dataframe, news.data) # combine by columns or rows
}

# data preprocessing
sentences <- sub("http://([[:alnum:]|[:punct:]])+", '', dataframe$content)
corpus = tm::Corpus(tm::VectorSource(sentences))
# Handling UTF-8 encoding problem from the dataset
corpus.cleaned <- tm::tm_map(corpus, function(x) iconv(x, to='UTF-8', sub='byte')) 
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::removeWords, tm::stopwords('english')) # Removing stop-words
corpus.cleaned <- tm::tm_map(corpus, tm::stemDocument, language = "english") # Stemming the words
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stripWhitespace) # Trimming excessive whitespaces



# TF
tdm <- tm::DocumentTermMatrix(corpus.cleaned)

#tdm_tfidf <- TermDocumentMatrix(corpus.cleaned,
 #                               control = list(weighting = weightTfIdf)) #using td-idf method

# IDF
tdm.tfidf <- tm::weightTfIdf(tdm)

# TF-IDF Weight
tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999)
tfidf.matrix <- as.matrix(tdm.tfidf)
# Cosine distance matrix (useful for specific clustering algorithms)
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

#PCA

PCA <- cmdscale(dist.matrix, k = 2) # Running the PCA

# elbow method to know number of k
wss <-list()

for (i in 1:10) {
  kmeanmodel <- kmeans(PCA , i)
  wss[i] <- sum(kmeanmodel$withinss)
}

plot(1:10, 
     wss, 
     xlab = "Health Tweets Cluster", 
     ylab = "With-in-Sum-of-Squares", 
     type = "b")

# Clustering K-means
truth.K <- 3
clustering.kmeans <- kmeans(PCA, truth.K)
clustering.kmeans$size


# Plotting 





#clusplot(PCA, clus = master.cluster, 
#         main = "k-means TF-IDF clustering", color = T, 
#         span = T, lines = 0, labels = 2, shade = T)

master.cluster <- clustering.kmeans$cluster
plot(PCA,
     main = 'K-Means clustering',
     col = as.factor(master.cluster),
     mai = c(0, 0, 0, 0),
     mar = c(0, 0, 0, 0),
     xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '')
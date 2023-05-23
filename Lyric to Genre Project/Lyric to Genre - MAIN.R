# initialize environment
library(NLP)
library(tm)
library(class)
library(randomForest)

# load data
song.data<-read.csv("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Stat Project/songs_uniformbygenre.csv")
table(song.data$tag) # looks uniform by genre

# clean data function using tm
clean <- function(corpus){
  corpus<-VCorpus(VectorSource(corpus))
  corpus<-tm_map(corpus,content_transformer(tolower))
  corpus<-tm_map(corpus,removeNumbers)
  corpus<-tm_map(corpus,removePunctuation)
  corpus<-tm_map(corpus,removeWords, stopwords('en')) # removes common words (the...)
  corpus<-tm_map(corpus,stemDocument) # loved->love
  corpus<-tm_map(corpus,stripWhitespace)
  return(corpus)
}

# build document term matrix
generateDTM <- function(category,corpus){
  corpus<-clean(corpus)
  dtm<-DocumentTermMatrix(corpus)
  dtm<-removeSparseTerms(dtm,0.98) # removes sparse words
  df<-as.data.frame(as.matrix(dtm))
  df$category<-category
  return(df)
}
# running program, creating df
df<-generateDTM(song.data$tag,song.data$lyrics)

# test/train datasets
set.seed(123)
index<-sample(c(T,F),nrow(df),replace=T,prob=c(0.7,0.3))
train<-df[index,]
test<-df[!index,]
names(train)

# randomForest model
rf <- randomForest(x=train[-length(train)],
                   y=train$category,
                   ntree=100)
rf.y<-predict(rf,newdata=test[-length(test)])
rf.cm<-table(test$category,rf.y)
rf.cm
sum(diag(rf.cm))/sum(rf.cm) # accuracy of model

# knn model
knn.y <- knn(train=train[,-length(train)],
           test=test[,-length(test)],
           cl=train$category,
           k=1)
knn.cm<-table(test$category,knn.y)
knn.cm
sum(diag(knn.cm))/sum(knn.cm) # accuracy of model

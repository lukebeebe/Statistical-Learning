songs<-read.csv("/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Stat Project/song_lyrics.csv") # import df
songs.en<-songs[songs$language_cld3=='en',] # only choose english
head(songs.en)

# make it uniform for decade
decades<-c(1960,1970,1980,1990,2000,2010,2020,2030)
songs.en.sample<-NULL
i=1
for(i in i:length(decades)){
  print(i)
  index<-songs.en$year>=decades[i]&songs.en$year<decades[i+1]
  decade<-songs.en[index,]
  x<-sample(1:nrow(decade),10000,replace=F) # sample numbers
  songs.en.sample<-rbind(songs.en.sample,decade[x,]) # create sample df
  if(i==length(decades)-1){break}
}
head(songs.en.sample$year) # inspect head of sample df
table(songs.en.sample$year)
table(songs.en.sample$tag)
write.csv(songs.en.sample,file = "/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Stat Project/songs_uniformbydecade.csv")

# make it uniform for genre
songs.en.sample<-NULL
tags<-unique(songs.en$tag)
i=1
for(i in i:length(tags)){
  print(i)
  index<-songs.en$tag==tags[i]
  genre<-songs.en[index,]
  x<-sample(1:nrow(genre),10000,replace=F) # sample numbers
  songs.en.sample<-rbind(songs.en.sample,genre[x,]) # create sample df
}
head(songs.en.sample$tag)
table(songs.en.sample$tag)
write.csv(songs.en.sample,file = "/Users/lukebeebe/Documents/School/Rutgers/Spring 2023/Applied Stat Project/songs_uniformbygenre.csv")

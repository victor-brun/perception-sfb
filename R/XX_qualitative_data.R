## Exploration of qualitative data

rm(list=ls())

library(tidyr) ; library(dplyr) ; library("tm") ; library("SnowballC") ; library("wordcloud")  
library("RColorBrewer")

df_quali = read.csv("data/general_quali.csv", sep=";")
df_quanti = read.csv("data/general_quanti.csv", sep=";")

df_quanti2 = df_quanti %>% 
  select(c(Session, Residence))

df_quali = df_quali %>% 
  left_join(df_quanti2, by="Session")

write.csv(df_quali, "df_quali.csv")


# Writing to .txt
write.table(df_quali, file = "df_quali.txt", sep = "\t",
            row.names = TRUE, col.names = NA)

# Importing text data
filePath <- "df_quali.txt"
text <- readLines(filePath)
docs <- Corpus(VectorSource(text))
inspect(docs)


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convertir le texte en minuscule
docs <- tm_map(docs, content_transformer(tolower))
# Supprimer les nombres
docs <- tm_map(docs, removeNumbers)
# Supprimer les mots vides anglais
docs <- tm_map(docs, removeWords, stopwords("english"))
# Supprimer votre propre liste de mots non désirés
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Supprimer les ponctuations
docs <- tm_map(docs, removePunctuation)
# Supprimer les espaces vides supplémentaires
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Instalacja pakietów

install.packages("here")
library(here)

install.packages("lsa")
library(lsa)

install.packages(c("tm", "hunspell", "topicmodels", "wordcloud"))

install.packages(c("proxy"))
library(proxy)

install.packages(c("corrplot"))
library(corrplot)

install.packages(c("dendextend"))
library(dendextend)

install.packages(c("cluster"))
library(cluster)

install.packages(c("clues"))
library(clues)

#te do klasyfikacji
library(tm)
library(hunspell)
library(stringr)

#te do diriszlom
library(topicmodels)
library(wordcloud)


# Ustawienie katalogu roboczego i struktury projektu

setwd(here())

inputDir <- ".\\data"
outputDir <- ".\\results"
utilsDir <- ".\\utils"

# Utworzenie korpusu dokumentów (a)
vcorpus <- VCorpus(
  DirSource(inputDir, pattern = "*.txt", encoding = "UTF-8"),
  readerControl = list(language = "pl_PL")
)

# Wstępne przetwarzanie: nadmierne puste znaki (b)

vcorpus <- tm_map(vcorpus, stripWhitespace)

# Wstępne przetwarzanie: numery, znaki interpunkcyjne, do małej litery (b)

vcorpus <- tm_map(vcorpus, content_transformer(tolower))
vcorpus <- tm_map(vcorpus, removeNumbers)
vcorpus <- tm_map(vcorpus, removePunctuation)


# Wstępne przetwarzanie: słowa ze stop listy (b)

stoplistFile <- paste(utilsDir, "\\", "stopwords_pl.txt", sep = "", collapse = NULL)
stoplist <- readLines(stoplistFile, encoding = "UTF-8")
vcorpus <- tm_map(vcorpus, removeWords, stoplist)

vcorpus <- tm_map(vcorpus, stripWhitespace)
vcorpus <- tm_map(vcorpus, removePunctuation)

# Wstępne przetwarzanie: usunięcie niechcianych znaków

removeChar <- content_transformer(
  function(x, pattern, replacement) gsub(pattern, replacement, x)
)

vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8722), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8217), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8211), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8220), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8221), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8222), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(8230), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(187), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(171), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(190), "")
vcorpus <- tm_map(vcorpus,removeChar, intToUtf8(190), "")

vcorpus <- tm_map(vcorpus, stripWhitespace)

# Wstępne przetwarzanie: lematyzacja

polish <- dictionary(lang = "pl_PL", affix = NULL, cache = TRUE)

lemmatize <- function(text) {
  singleText <- str_trim(as.character(paste(text,collapse = "")))
  parsedText <- strsplit(singleText, " ")

  newTextVec <- hunspell_stem(parsedText[[1]], dict = polish)
  
  newTextVec
    for (i in 1:length(newTextVec)) {
     if (length(newTextVec[[i]]) == 0) newTextVec[i] <- parsedText[[1]][i]
     if (length(newTextVec[[i]]) > 1) newTextVec[i] <- newTextVec[[i]][1]
   }
  newText <- paste(newTextVec, collapse = " ")
  return(newText)
}

vcorpus <- tm_map(vcorpus, content_transformer(lemmatize))

# Usunięcie rozszerzeń z nazw dokumentów w korpusie

cutExtensions <- function(document) {
  meta(document, "id") <- gsub(
    pattern="\\.txt$",
    "",
    meta(document, "id")
  )
  return(document)
}

vcorpus <- tm_map(vcorpus, cutExtensions)

# Zapisanie korpusu do plików

preprocessedDir <- paste(outputDir, "\\", "preprocessed", sep = "", collapse = NULL)
dir.create(preprocessedDir)
writeCorpus(vcorpus, path = preprocessedDir)

# Wyświetlanie informacji o korpusie  --- A

summary(vcorpus)
inspect(vcorpus[1])


# Tworzenie macierzy częstości --- c 4 b

tdm_tf_all <- TermDocumentMatrix(vcorpus)
tdm_bin_all <- TermDocumentMatrix(vcorpus, control = list(weigthing = weightBin))

tdm_tf_1_20 <- TermDocumentMatrix(vcorpus, control = list(bounds = list(global = c(1, Inf))))
tdm_tfidf_1_20 <- TermDocumentMatrix(vcorpus, control = list( weighting = weightTfIdf, bounds = list(global = c(1, Inf))))
dtm_tfidf_1_20 <- DocumentTermMatrix(vcorpus, control = list(weighting = weightTfIdf, bounds = list(global = c(1,Inf))))

tdm_tf_2_19 <- TermDocumentMatrix(vcorpus, control = list(bounds = list(global = c(2,19))))
tdm_tfidf_2_19 <- TermDocumentMatrix(vcorpus, control = list( weighting = weightTfIdf, bounds = list(global = c(2,19))))
dtm_tfidf_2_19 <- DocumentTermMatrix(vcorpus, control = list(weighting = weightTfIdf, bounds = list(global = c(2,19))))

tdm_tf_3_18 <- TermDocumentMatrix(vcorpus, control = list(bounds = list(global = c(3,18))))
tdm_tfidf_3_18 <- TermDocumentMatrix(vcorpus, control = list( weighting = weightTfIdf, bounds = list(global = c(3,18))))
dtm_tfidf_3_18 <- DocumentTermMatrix(vcorpus, control = list(weighting = weightTfIdf, bounds = list(global = c(3,18))))

tdm_tf_4_17 <- TermDocumentMatrix(vcorpus, control = list(bounds = list(global = c(4,17))))
tdm_tfidf_4_17 <- TermDocumentMatrix(vcorpus, control = list( weighting = weightTfIdf, bounds = list(global = c(4,17))))
dtm_tfidf_4_17 <- DocumentTermMatrix(vcorpus, control = list(weighting = weightTfIdf, bounds = list(global = c(4,17))))

tdm_tf_all
tdm_bin_all

tdm_tf_1_20
tdm_tf_2_19
tdm_tf_3_18
tdm_tf_4_17

tdm_tfidf_1_20
tdm_tfidf_2_19
tdm_tfidf_3_18
tdm_tfidf_4_17

# Analiza głównych składowych - d 4 b

pca_1_20 <- prcomp(dtm_tfidf_1_20)
pca_2_19 <- prcomp(dtm_tfidf_2_19)
pca_3_18 <- prcomp(dtm_tfidf_3_18)
pca_4_17 <- prcomp(dtm_tfidf_4_17)

legend <- paste(paste("d", 1:20, sep = ""), rownames(dtm_tfidf_1_20), sep = " - ")

## Wykres dla 1 - 20
pca_plot_1_20 <- paste(outputDir,'\\',"pca_1_20.png",sep = "", collapse = NULL)

png(filename = pca_plot_1_20, width = 1024)
options(scipen = 5)
par(mar = c(5.1, 4.1, 4.1, 25.1), xpd = TRUE)
plot(pca_1_20$x[,1], pca_1_20$x[,2], pch = 1, col = "black", xlab = "", ylab = "")
text(pca_1_20$x[,1], pca_1_20$x[,2], paste("d",1:19,sep = ""), col = "black",pos = 4)
legend("topright",  inset = c(-0.57,0), legend, text.font = 16, cex = 0.5, text.col = "black")
dev.off()

## Wykres dla 2 - 19
pca_plot_2_19 <- paste(outputDir,'\\',"pca_2_19.png",sep = "", collapse = NULL)

png(filename = pca_plot_2_19, width = 1024)
options(scipen = 5)
par(mar = c(5.1, 4.1, 4.1, 25.1), xpd = TRUE)
plot(pca_2_19$x[,1], pca_2_19$x[,2], pch = 1, col = "black", xlab = "", ylab = "")
text(pca_2_19$x[,1], pca_2_19$x[,2], paste("d",1:19,sep = ""), col = "black",pos = 4)
legend("topright",  inset = c(-0.57,0), legend, text.font = 16, cex = 0.5, text.col = "black")
dev.off()

## Wykres dla 3 - 18
pca_plot_3_18 <- paste(outputDir,'\\',"pca_3_18.png",sep = "", collapse = NULL)

png(filename = pca_plot_3_18, width = 1024)
options(scipen = 5)
par(mar = c(5.1, 4.1, 4.1, 25.1), xpd = TRUE)
plot(pca_3_18$x[,1], pca_3_18$x[,2], pch = 1, col = "black", xlab = "", ylab = "")
text(pca_3_18$x[,1], pca_3_18$x[,2], paste("d",1:19,sep = ""), col = "black",pos = 4)
legend("topright",  inset = c(-0.57,0), legend, text.font = 16, cex = 0.5, text.col = "black")
dev.off()

## Wykres dla 4 - 17
pca_plot_4_17 <- paste(outputDir,'\\',"pca_4_17.png",sep = "", collapse = NULL)

png(filename = pca_plot_4_17, width = 1024)
options(scipen = 5)
par(mar = c(5.1, 4.1, 4.1, 25.1), xpd = TRUE)
plot(pca_4_17$x[,1], pca_4_17$x[,2], pch = 1, col = "black", xlab = "", ylab = "")
text(pca_4_17$x[,1], pca_3_18$x[,2], paste("d",1:19,sep = ""), col = "black",pos = 4)
legend("topright",  inset = c(-0.57,0), legend, text.font = 16, cex = 0.5, text.col = "black")
dev.off()

# Dekompozycja wedłud wartości osobliwych (analiza ukrytych wymiarów semantycznych) d 4 b

tdm_tfidf_1_20_matrix <- as.matrix(tdm_tfidf_1_20)
tdm_tfidf_2_19_matrix <- as.matrix(tdm_tfidf_2_19)
tdm_tfidf_3_18_matrix <- as.matrix(tdm_tfidf_3_18)
tdm_tfidf_4_17_matrix <- as.matrix(tdm_tfidf_4_17)

dtm_tfidf_1_20_matrix <- as.matrix(dtm_tfidf_1_20)
dtm_tfidf_2_19_matrix <- as.matrix(dtm_tfidf_2_19)
dtm_tfidf_3_18_matrix <- as.matrix(dtm_tfidf_3_18)
dtm_tfidf_4_17_matrix <- as.matrix(dtm_tfidf_4_17)

dtm_tf_1_20 <- DocumentTermMatrix(vcorpus, control = list( bounds = list( global = c(1,20))))
dtm_tf_2_19 <- DocumentTermMatrix(vcorpus, control = list( bounds = list( global = c(2,19))))
dtm_tf_3_18 <- DocumentTermMatrix(vcorpus, control = list( bounds = list( global = c(3,18))))
dtm_tf_4_17 <- DocumentTermMatrix(vcorpus, control = list( bounds = list( global = c(4,17))))

dtm_tf_1_20_matrix <- as.matrix(dtm_tf_1_20)
dtm_tf_2_19_matrix <- as.matrix(dtm_tf_2_19)
dtm_tf_3_18_matrix <- as.matrix(dtm_tf_3_18)
dtm_tf_4_17_matrix <- as.matrix(dtm_tf_4_17)

lsa_model_1_20 <- lsa(tdm_tfidf_1_20_matrix)
lsa_model_2_19 <- lsa(tdm_tfidf_2_19_matrix)
lsa_model_3_18 <- lsa(tdm_tfidf_3_18_matrix)
lsa_model_4_17 <- lsa(tdm_tfidf_4_17_matrix)

# lsa_model$tk # odpowiednik macierzy U, współrzędne wyrazów 
# lsa_modeldk # odpowiednik macierzy V, współrzędne dokumentów
# lsa_model$sk # odpowiednik macierzy D, znaczenie składowych

coordTerms <- lsa_model_1_20$tk %*% diag(lsa_model_1_20$sk)
coordDocs <- lsa_model_1_20$dk %*% diag(lsa_model_1_20$sk)
words <- c("samochód", "auto", "kierowca", "technologia", "przeglądarka", "internet", "serce", 'hiv', "rak", "zdrowie", "dieta", "gen", "dna") 
coordWords <- coordTerms[words,]

legend <- paste(paste("d",1:20,sep = ""), rownames(coordDocs), sep = " - ")

plot(coordDocs[,1],coordDocs[,2], pch=1, col="black", xlim=c(-0.2,0.05))
points(coordWords[,1],coordWords[,2], pch=2, col="red")
text(coordDocs[,1],coordDocs[,2],paste("d",1:19,sep=""),pos=4, col="black")
text(coordWords[,1],coordWords[,2],rownames(coordWords),pos=4, col="red")
legend("bottomleft", legend, cex=0.4, text.col="dark violet")

#klasyfikacja wzorcowa i bezwzorcowa

#odleglosc euklidesowa - dendrgramy oraz wykresy sklupkowe wykrytych tematow
dist_1 <- dist(dtm_tfidf_1_20_matrix, method = "euclidean")
model_hclust_1 <- hclust(dist_1, method = "single")
plot(model_hclust_1, cex=0.7)

barplot(model_hclust_1$height, names.arg = 19:1)

dist_2 <- dist(dtm_tfidf_2_19_matrix, method = "euclidean")
model_hclust_2 <- hclust(dist_2, method = "single")
plot(model_hclust_2, cex=0.7)

barplot(model_hclust_2$height, names.arg = 19:1)

dist_3 <- dist(dtm_tfidf_3_18_matrix, method = "euclidean")
model_hclust_3 <- hclust(dist_3, method = "single")
plot(model_hclust_3, cex=0.7)

barplot(model_hclust_3$height, names.arg = 19:1)

dist_4 <- dist(dtm_tfidf_4_17_matrix, method = "euclidean")
model_hclust_4 <- hclust(dist_4, method = "single")
plot(model_hclust_4, cex=0.7)

barplot(model_hclust_4$height, names.arg = 19:1)

#a teraz tak jak powinno sie robic, czyli odlegloscia Cosinusowa

dist_5 <- dist(dtm_tfidf_1_20_matrix, method = "cosine")
model_hclust_5 <- hclust(dist_5, method = "ward.D2")
plot(model_hclust_5, cex=0.7)

barplot(model_hclust_5$height, names.arg = 19:1)

dist_6 <- dist(dtm_tfidf_2_19_matrix, method = "cosine")
model_hclust_6 <- hclust(dist_6, method = "ward.D2")
plot(model_hclust_6, cex=0.7)

barplot(model_hclust_6$height, names.arg = 19:1)

dist_7 <- dist(dtm_tfidf_3_18_matrix, method = "cosine")
model_hclust_7 <- hclust(dist_7, method = "ward.D2")
plot(model_hclust_7, cex=0.7)

barplot(model_hclust_7$height, names.arg = 19:1)

dist_8 <- dist(dtm_tfidf_4_17_matrix, method = "cosine")
model_hclust_8 <- hclust(dist_8, method = "ward.D2")
plot(model_hclust_8, cex=0.7)

barplot(model_hclust_8$height, names.arg = 19:1)

#klasyfikacja wzorcowa - klasyfikujemy tylko i wylacznie
#najlepsze, czyli 4-17 w cosinusowej

#najpierw dla domyslnej wartosci 5 tematow

model_hclust_8.clusters <- cutree(model_hclust_8,5)
res <- matrix(0,20,5)
rownames(res) <- names(model_hclust_8.clusters)
for (i in 1:20) {
  res[i,model_hclust_8.clusters[i]] <- 1
}
corrplot(res)

#dla 6
model_hclust_8.clusters <- cutree(model_hclust_8,6)
res <- matrix(0,20,6)
rownames(res) <- names(model_hclust_8.clusters)
for (i in 1:20) {
  res[i,model_hclust_8.clusters[i]] <- 1
}
corrplot(res)

#dla 10
model_hclust_8.clusters <- cutree(model_hclust_8,10)
res <- matrix(0,20,10)
rownames(res) <- names(model_hclust_8.clusters)
for (i in 1:20) {
  res[i,model_hclust_8.clusters[i]] <- 1
}
corrplot(res)

#dla 2
model_hclust_8.clusters <- cutree(model_hclust_8,2)
res <- matrix(0,20,2)
rownames(res) <- names(model_hclust_8.clusters)
for (i in 1:20) {
  res[i,model_hclust_8.clusters[i]] <- 1
}
corrplot(res)

#dla 3
model_hclust_8.clusters <- cutree(model_hclust_8,3)
res <- matrix(0,20,3)
rownames(res) <- names(model_hclust_8.clusters)
for (i in 1:20) {
  res[i,model_hclust_8.clusters[i]] <- 1
}
corrplot(res)


#Latent Dirichlet Allocation

#utworzenie modelu LDA
n_words <- ncol(dtm_tf_4_17)
n_groups <- 5

lda_model_5 <- LDA(dtm_tf_4_17, 
                   k=n_groups, 
                   method="Gibbs",
                   control=list(
                     burnin=2000,
                     thin=100,
                     iter=3000
                   )
)
res_5 <- posterior(lda_model_5)


#same keywordy i prawdopodobieństwo wystąpienia
keywords_tf_1 <- head(sort(dtm_tf_4_17_matrix[1,], decreasing = TRUE))
keywords_tf_1

keywords_tf_19 <- head(sort(dtm_tf_4_17_matrix[19,], decreasing = TRUE))
keywords_tf_19

keywords_tfidf_1 <- head(sort(dtm_tfidf_4_17_matrix[1,], decreasing = TRUE))
keywords_tfidf_1

keywords_tfidf_19 <- head(sort(dtm_tfidf_4_17_matrix[19,], decreasing = TRUE))
keywords_tfidf_19

words_1 <- c(res_5$topics[1,]%*%res_5$terms)
names(words_1) <- colnames(res_5$terms)
keywords_lda_1 <- head(sort(words_1, decreasing = TRUE))
keywords_lda_1

words_19 <- c(res_5$topics[19,]%*%res_5$terms)
names(words_19) <- colnames(res_5$terms)
keywords_lda_19 <- head(sort(words_19, decreasing = TRUE))
keywords_lda_19

wordcloud(vcorpus[1])
wordcloud(vcorpus[19])

d_1 <- res_5$topics[1,]
barplot(rev(d_1), 
        horiz = TRUE, 
        main=rownames(res_5$topics)[1],
        xlab="Probability"
)

d_5 <- res_5$topics[5,]
barplot(rev(d_5), 
        horiz = TRUE, 
        main=rownames(res_5$topics)[5],
        xlab="Probability"
)


d_9 <- res_5$topics[9,]
barplot(rev(d_9), 
        horiz = TRUE, 
        main=rownames(res_5$topics)[9],
        xlab="Probability"
)

d_13 <- res_5$topics[13,]
barplot(rev(d_13), 
        horiz = TRUE, 
        main=rownames(res_5$topics)[13],
        xlab="Probability"
)


d_17 <- res_5$topics[17,]
barplot(rev(d_17), 
        horiz = TRUE, 
        main=rownames(res_5$topics)[17],
        xlab="Probability"
)

#udział słów w tematach
t_1 <- head(sort(res_5$terms[1,], decreasing = TRUE),20)
barplot(rev(t_1), 
        horiz = TRUE,
        las=1,
        main="Topic 1",
        xlab="Probability"
)

t_2 <- head(sort(res_5$terms[2,], decreasing = TRUE),20)
barplot(rev(t_2), 
        horiz = TRUE,
        las=1,
        main="Topic 2",
        xlab="Probability"
)

t_3 <- head(sort(res_5$terms[3,], decreasing = TRUE),20)
barplot(rev(t_3), 
        horiz = TRUE,
        las=1,
        main="Topic 3",
        xlab="Probability"
)

t_4 <- head(sort(res_5$terms[4,], decreasing = TRUE),20)
barplot(rev(t_4), 
        horiz = TRUE,
        las=1,
        main="Topic 4",
        xlab="Probability"
)

t_5 <- head(sort(res_5$terms[5,], decreasing = TRUE),20)
barplot(rev(t_5), 
        horiz = TRUE,
        las=1,
        main="Topic 5",
        xlab="Probability"
)



















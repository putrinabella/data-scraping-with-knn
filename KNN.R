# set working directory
setwd("~/0) Computer Science/0) Term 5/Text Mining/UAS")

# import library
library(tm)
library(xlsx)
library(katadasaR)
library(textclean)
library(wordcloud2)
library (caTools)
library(gmodels)
library (rpart)
library (class)

# remove list
rm(list = ls())

# membaca data
main_data = read.xlsx("Data Covid Awal.xlsx", sheetName = "Sheet1", stringsAsFactors = FALSE)

# mengubah data dalam bentuk char
main_data_text <- main_data$text  
as.character(main_data_text)
head(main_data_text)
main_data_text[5]

# menghapus slangword
spell.lex <- read.csv("slangword.csv")

main_data_text <- replace_internet_slang(main_data_text, slang = paste0("\\b", spell.lex$slang, "\\b"), replacement = spell.lex$formal, ignore.case = TRUE)
head(main_data_text)

# mengubah data menjadi corpus
data_text = cbind.data.frame(paste0("doc_",c(1:nrow(main_data))), main_data_text)
colnames(data_text) = c("doc_id", "text")
corpus = VCorpus(DataframeSource(data_text))

# menghilangkan user
removeUser<-function(x) {
  return(gsub("@\\w+", "", x))
}

corpus.processed = tm_map(corpus, content_transformer(removeUser))

# menghilangkan url
cleanURL<-function(x) {
  return(gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
}

corpus.processed = tm_map(corpus.processed, content_transformer(cleanURL))

# menghilangkan hastag
cleanHashtag<-function(x) {
  return(gsub('#\\S+', '', x))
}

corpus.processed = tm_map(corpus.processed, content_transformer(cleanHashtag))

# mengubah jadi huruf kecil
corpus.processed = tm_map(corpus.processed, content_transformer(tolower))

#menghilangkan emoticon
removeEmoticon<- function(x) {
  return(gsub("[^\x01-\x7F]", "", x))
}
corpus.processed =tm_map(corpus.processed, content_transformer(removeEmoticon))

# menghapus tanda baca
corpus.processed = tm_map(corpus.processed, removePunctuation)

# menghapus angka
corpus.processed = tm_map(corpus.processed, removeNumbers)

# mengubah katadasar
getKataDasar<-function(x) {
  str = unlist(strsplit(stripWhitespace(x), " "))
  str<-sapply(str,katadasaR)
  str<-paste(str,collapse = " ")
  return(str)
}
corpus.processed = tm_map(corpus.processed,content_transformer(getKataDasar))

# menghapus stopwords
stopwords.id=readLines("masdevid.txt")

corpus.processed = tm_map(corpus.processed, removeWords, stopwords.id)

# membersihkan spasi berlebihan
corpus.processed = tm_map(corpus.processed, stripWhitespace)

# menggabung data_teks.cleaned dengan class_label dari data_teks
corpus.df = data.frame(text = unlist(sapply(corpus.processed, '[', "content")), stringsAsFactors = FALSE)
data_teks = cbind.data.frame(corpus.df, main_data$label)
colnames(data_teks) = c("text", "class_label")

View(data_teks)
# menyimpan xlsx
write.xlsx(data_teks, "UAS) Output Cleaning.xlsx", row.names = FALSE)

#frequensi Kata
data_frequency = data_teks
data_frequency.corpus = cbind.data.frame(paste0("doc_",c(1:nrow(data_frequency))), data_frequency$text)
colnames(data_frequency.corpus) = c("doc_id", "text")
corpus = VCorpus(DataframeSource(data_frequency.corpus))

tdm = TermDocumentMatrix(corpus,control=list(weighting=weightTf))
tdm.matrix = as.matrix(tdm)
term.freq = rowSums(tdm.matrix)
freq.df = data.frame(word=names(term.freq), frequency=term.freq)
freq.df = freq.df[order(freq.df[,2], decreasing=T),]
print(freq.df[1:20,])

#wordcloud 2
head(freq.df)

# normal
wordcloud2(data = freq.df)

# TF IDF
data_teks_unigram = data_teks
main_data.corpus = cbind.data.frame(paste0("doc_",c(1:nrow(data_teks_unigram))), data_teks_unigram$text)
colnames(main_data.corpus) = c("doc_id", "text")
corpus = VCorpus(DataframeSource(main_data.corpus))
dtm = DocumentTermMatrix(corpus, control = list(weighting = function(x) weightTf(x)))
main_data.unigram = cbind.data.frame(as.data.frame(as.matrix(dtm)), data_teks$class_label)
colnames(main_data.unigram)[ncol(main_data.unigram)] = "class_label"

write.xlsx(main_data.unigram, "UAS) Unigram.xlsx", row.names = FALSE)





# pembobotan 
dtm = DocumentTermMatrix(corpus, control = list(weighting = function(x) weightTf(x)))
dtm
dtm = removeSparseTerms(dtm, 0.99)
dtm

# mengambil data label
dlabel = as.factor(data_teks$class_label)
levels(dlabel)

#memisahkan dataset menjadi data training dan testing
set.seed(300)
trainFrac = 60

trainIdx = sample.int(nrow(dtm), size = ceiling(nrow(dtm) - trainFrac), replace = FALSE)
trainIdx = sort(trainIdx)

testIdx = setdiff(1:nrow(dtm), trainIdx)
dtmTrain = dtm[trainIdx,]
dtmTest = dtm[testIdx,]
dLabelTrain = dlabel[trainIdx]
dLabelTest = dlabel[testIdx]
dtmTrain
dtmTest

dframeTrain = data.frame(as.matrix(dtmTrain))
dframeTrain$label = as.factor(dLabelTrain)

# klasifikasi KNN
knn = knn(dtmTrain, dtmTest, dLabelTrain, k = 5, prob = TRUE)

knnTable = table(knn, dLabelTest, dnn = list('predicted', 'actual'))
knnTable

CrossTable(knn,dLabelTest)


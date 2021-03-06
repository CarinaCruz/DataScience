#############################################################
# TEXT MINING
#############################################################

####################
# WORDCLOUD
####################
install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer"))
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# carregando o daraset e n�o converte as strings para fator
data <- read.csv("questoes.csv", stringsAsFactors = FALSE)
head(data)

# criando um corpus (tipo de objeto para text mining)
dataCorpus <- Corpus(VectorSource(data$Question))
class(dataCorpus)

# convertendo corpus em um documento de texto plano
dataCorpus <- tm_map(dataCorpus, PlainTextDocument)

# remove pontuacao
dataCorpus <- tm_map(dataCorpus, removePunctuation)

# remover palavras espec�ficas do ingl�s (palavras comumente usadas)
dataCorpus <- tm_map(dataCorpus, removeWords, stopwords("english"))

# V�rias vers�es de uma palavra s�o convertidas (agrupamento de palavras que aparecem com mais frequencia)
dataCorpus <- tm_map(dataCorpus, stemDocument)

dataCorpus <- Corpus(VectorSource(data))

# wordcloud
wordcloud(dataCorpus, max.words = 50, random.order = FALSE)


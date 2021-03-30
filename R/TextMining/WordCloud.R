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

# carregando o daraset e não converte as strings para fator
data <- read.csv("questoes.csv", stringsAsFactors = FALSE)
head(data)

# criando um corpus (tipo de objeto para text mining)
dataCorpus <- Corpus(VectorSource(data$Question))
class(dataCorpus)

# convertendo corpus em um documento de texto plano
dataCorpus <- tm_map(dataCorpus, PlainTextDocument)

# remove pontuacao
dataCorpus <- tm_map(dataCorpus, removePunctuation)

# remover palavras específicas do inglês (palavras comumente usadas)
dataCorpus <- tm_map(dataCorpus, removeWords, stopwords("english"))

# Várias versões de uma palavra são convertidas (agrupamento de palavras que aparecem com mais frequencia)
dataCorpus <- tm_map(dataCorpus, stemDocument)

dataCorpus <- Corpus(VectorSource(data))

# wordcloud
wordcloud(dataCorpus, max.words = 50, random.order = FALSE)


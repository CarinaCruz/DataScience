#####################################################################
# Modelo para prever a ocorrência de câncer com KNN
####################################################################


####################################################################
# Carregando os dados 
####################################################################

# Verificando o diretório 
getwd()

# Carregando o dataset
dados <- read.csv("cancer_data.csv")

# Visualizando as primeiras colunas
head(dados)

# Visualizando informações sobre os dados 
str(dados)

####################################################################
# Explorando os dados
####################################################################

# Excluindo a coluna id para que o modelo não sofra nenhuma interferência por conta desses valores
dados <- dados[-1]
head(dados)

# Verificando se há algum valor nulo
any(is.na(dados))

# Verificando a coluna que classifica se é câncer ou não
table(dados$diagnosis)

# Ela pode ser classificada em factor, portanto fazemos a conversão
dados$diagnosis <- factor(dados$diagnosis, levels = c("B", "M"), labels = c("Benigno", "Maligno"))

# Para conferir essa nova coluna
head(dados)

# Agora podemos conferir que há uma coluna do tipo factor para classificar o 
# câncer como Benigno ou Maligno
str(dados$diagnosis)

# Verificando a proporção do diagnóstico
round(prop.table(table(dados$diagnosis)) * 100, digits = 1)

# Verificando as Medidas de Tendência Central

# Vemos com esse comando que há um problema de escala entre esses valores
summary(dados[c("radius_mean", "area_mean", "smoothness_mean")])

# Normalização
normalizar <- function(x){
  return ((x - min(x))/(max(x)-min(x)))
}

# Testando a normalização
normalizar(c(1, 2, 3, 4, 5))
normalizar(c(10, 20, 30, 40, 50))

# Aplicando a função de normalização aos dados 
dados_normalizados <- as.data.frame(lapply(dados[2:31], normalizar))

# Visualizando a normalização através da comparação dos dados originais e os normalizados
summary(dados[c("radius_mean", "area_mean", "smoothness_mean")])
summary(dados_normalizados[c("radius_mean", "area_mean", "smoothness_mean")])


####################################################################
# Treinando o modelo
####################################################################

# Carregando o class
install.packages("class")
library(class)

# Dividindo o conjunto de dados em treino e teste
# (conjunto de linhas e todas as colunas)
dados_treino <- dados_normalizados[1:469,]
dados_teste <- dados_normalizados[470:569,]

# Criando labels para os dados de treino e de teste
# (conjunto de linhas e a primeira coluna)
dados_treino_labels <- dados[1:469, 1]
dados_teste_labels <- dados[470:569, 1]

# Criando o modelo
modelo <- knn(train = dados_treino, 
              test = dados_teste,
              cl = dados_treino_labels,
              k = 21)


####################################################################
# Avaliando e Interpretando o Modelo
####################################################################

# Carregando o gmodels
install.packages("gmodels")
library(gmodels)

# Criando uma tabela cruzada dos dados previstos x dados atuais
CrossTable(x = dados_teste_labels, y = modelo, prop.chisq = FALSE)

# Para os casos Benignos = o modelo acertou 61 casos (true negative) o modelo não errou (false positive)
# Para os cados Malignos = o modelo errou 2 casos que eram Malignos e 
# foram classificados como Benignos (false negative) e acertou 37 casos (true positive)

# 98% de taxa de acerto

# Maligno 2
# Benigno 61

####################################################################
# Otimizando o Modelo
####################################################################

# Usando scale() para padronizar a forma como os dados estão distribuídos com o z-score
dados_z <- as.data.frame(scale(dados[-1]))

# Confirmando a transformação realizada
summary(dados_z$area_mean)

# Criando os dados de treino e teste
dados_treino <- dados_z[1:469,]
dados_teste <- dados_z[470:569,]

# Criando as labels
dados_treino_labels <- dados[1:469,1]
dados_teste_labels <- dados[470:569,1]

# Reclassificando
modelo_v2 <- knn(train = dados_treino,
                 test = dados_teste,
                 cl = dados_treino_labels,
                 k = 21)

# Criando uma tabela cruzada dos dados previstos x dados atuais
CrossTable(x = dados_teste_labels, y = modelo_v2, prop.chisq = FALSE)

# Maligno 5
# Benigno 61

###############################################################################
# Como o resultado foi pior que o anterior, testaremos uma série de recursos:
###############################################################################

# Reconstruindo os datasets

# Dividindo o conjunto de dados em treino e teste
# (conjunto de linhas e todas as colunas)
dados_treino <- dados_normalizados[1:469,]
dados_teste <- dados_normalizados[470:569,]

# Criando labels para os dados de treino e de teste
# (conjunto de linhas e a primeira coluna)
dados_treino_labels <- dados[1:469, 1]
dados_teste_labels <- dados[470:569, 1]

###############################################################################
# Reclassificando (agora com o k = 1)
modelo_v3 <- knn(train = dados_treino,
                 test = dados_teste,
                 cl = dados_treino_labels,
                 k = 1)



# Criando uma tabela cruzada dos dados previstos x dados atuais
CrossTable(x = dados_teste_labels, y = modelo_v3, prop.chisq = FALSE)

# Maligno 1
# Benigno 58
###############################################################################


###############################################################################
# Reclassificando (agora com o k = 5)
modelo_v4 <- knn(train = dados_treino,
                 test = dados_teste,
                 cl = dados_treino_labels,
                 k = 5)


# Criando uma tabela cruzada dos dados previstos x dados atuais
CrossTable(x = dados_teste_labels, y = modelo_v4, prop.chisq = FALSE)

# Maligno 2
# Benigno 61
###############################################################################


###############################################################################
# Reclassificando (agora com o k = 15)
modelo_v5 <- knn(train = dados_treino,
                 test = dados_teste,
                 cl = dados_treino_labels,
                 k = 15)


# Criando uma tabela cruzada dos dados previstos x dados atuais
CrossTable(x = dados_teste_labels, y = modelo_v5, prop.chisq = FALSE)

# Maligno 5
# Benigno 61
###############################################################################


###############################################################################
# Reclassificando (agora com o k = 11)
modelo_v6 <- knn(train = dados_treino,
                 test = dados_teste,
                 cl = dados_treino_labels,
                 k = 11)


# Criando uma tabela cruzada dos dados previstos x dados atuais
CrossTable(x = dados_teste_labels, y = modelo_v6, prop.chisq = FALSE)
# Maligno 3
# Benigno 61
###############################################################################



###############################################################################
# Reclassificando (agora com o k = 27)
modelo_v7 <- knn(train = dados_treino,
                 test = dados_teste,
                 cl = dados_treino_labels,
                 k = 27)


# Criando uma tabela cruzada dos dados previstos x dados atuais
CrossTable(x = dados_teste_labels, y = modelo_v7, prop.chisq = FALSE)

###############################################################################

# Após análise com outro valores de K, percebemos que o melhor desempenho 
# se deu quando k = 21 

###############################################################################
# Calculando taxa de erro
###############################################################################

prev = NULL
taxa_erro = NULL


for (i in 1:20){
  set.seed(101)
  prev = knn(train = dados_treino, 
             test = dados_teste,
             cl = dados_treino_labels,
             k = i)
  taxa_erro[i] = mean(dados$diagnosis != prev)
}

# Obtendo os valores de k e das taxas de erro
library(ggplot2)
k.values <- 1:20
df_erro <- data.frame(taxa_erro, k.values)
df_erro

# à medida que aumentamos k, diminuímos a taxa de erro do modelo
ggplot(df_erro, aes(x = k.values, y = taxa_erro)) +
  geom_point() +
  geom_line()
  














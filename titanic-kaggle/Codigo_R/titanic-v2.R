# Define the QUESTION
# -> Sobreviventes no naufragio do Titanic
# Define the IDEAL DATA SET
# -> 
# Determine WHAT DATA you can access ( Determine QUE DADOS você pode acessar)
# -> 
# OBTAIN the data ( OBTENHA os dados )
# -> Efetuar o download das informações no site www.kaggle.com
# -> Não consegui mudar meu displayname para FSBDS20_Jose Carlos, ficou Jose Carlos
# CLEAN the data
# Exploratory DATA ANALYSIS
# Statistical PREDICTION/MODELING
# INTERPRET Results
# CHALLENGE results
# Synthesize/ WRITE UP results
# Create reproducible CODE
#

# Survival:   Sobrevivente
# PassengerId: ID único de um passageiro
# pClass: Classe de bilhetes  
# Sex:    Sexo
# Age:    Idade em anos   
# SibSp:  Número de irmãos / cônjuges a bordo do Titanic  
# Parch:  Número de pais / filhos a bordo do Titanic  
# Ticket: Numero do bilhete   
# Fare:   Tarifa de passageiros   
# Cabin:  Número de cabine    
# Embarked:   Porto de embarcação



# Quantidade de Mulheres e Homens no dataset de Traino
table(is.na(train$Sex))
# FALSE 
# 891 

table(is.na(train$Pclass))
# FALSE 
# 891 

table(is.na(train$Age))
# FALSE  TRUE 
# 714   177 

table(is.na(train$SibSp))
# FALSE  TRUE 
# 714   177 

table(is.na(train$Parch))
# FALSE 
# 891 

table(is.na(train$Ticket))
# FALSE 
# 891 

table(is.na(train$Fare))
# FALSE 
# 891 

table(is.na(train$Cabin))
# FALSE 
# 891 

table(is.na(train$Embarked))
# FALSE 
# 891 


# Dados com valores nulos
# Estes dados precisam ser tratados, não sei como ainda, 2019-05-30 - 15:04
#       | SibSp  | Age    |
# ------+--------+--------+
# FALSE |  714   | 714    |
#  TRUE |  177   | 177    |
# --------------------------


plot(table(train$Sex))
# female   male 
# 314    577 

# Quantidade de pessoas por Classe
table(train$Pclass)
plot(table(train$Pclass))
#   1   2   3 
#  216 184 491

# Quantidade de pessoas por Classe
table(train$Age)
plot(table(train$Age))
#0.42 0.67 0.75 0.83 0.92  1    2    3    4    5    6    7    8    9   10   11   12   13   14 
#  1    1    2    2    1   7   10    6   10    4    3    3    4    8    2    4    1    2    6 
#  
#14.5  15   16   17   18   19   20 20.5   21   22   23 23.5   24 24.5   25   26   27   28 28.5 
# 1    5    17   13   26   25   15    1   24   27   15    1   30    1   23   18   18   25    2 
#
# 29   30 30.5   31   32 32.5   33   34 34.5   35   36 36.5   37   38   39   40 40.5   41   42 
# 20   25    2   17   18    2   15   15    1   18   22    1    6   11   14   13    2    6   13 
# 43   44   45 45.5   46   47   48   49   50   51   52   53   54   55 55.5   56   57   58   59 
# 5    9   12    2    3    9    9    6   10    7    6    1    8    2    1    4    2    5    2 
# 60   61   62   63   64   65   66   70 70.5   71   74   80 
# 4    3    4    2    2    3    1    2    1    2    1    1
table(train$SibSp)
plot(train$SibSp)
library(ggplot2) # Visualização de Dados
library(readr) # Leitura de arquivo
library('randomForest') #predição de sobreviventes
boxplot(train$Pclass)
boxplot(train$SibSp)
boxplot(train$Age)
# CLEAN os dados
# Análise exploratória de dados
# Estatística PREVISÃO / MODELAGEM
# INTERPRET Resultados
# Resultados do DESAFIO
# Synthesize / WRITE UP resultados
# Criar CÓDIGO reproduzível
#
#
#
#
#

# Leitura dos arquivos
train <- read.csv("train.csv")
test <- read.csv("test.csv")
train <- read.csv("train.csv",stringsAsFactor=F)
test <- read.csv("test.csv",stringsAsFactor=F)

# Comparando os Datasets 
ncol(train)
ncol(test)
# > ncol(train)
# [1] 12
# > ncol(test)
# [1] 11


#Igualando o numero de colunas com a coluna faltante e acrescentando o valor NA
test$Survived <- NA

train$TrainSet <- TRUE
test$TrainSet <- FALSE
dados_full <- rbind(train,test)
table(dados_full$TrainSet)
table(dados_full$Survived, dados_full$Sex)
boxplot(dados_full$Fare)

boxplot.stats(dados_full$Fare)
stats

# Conhecendo os Dados
str(train)
str(test)
#Verificando dados Nulos
table(is.na(train))
#####################
#  FALSE   TRUE 
#  10515   177
#####################

table(is.na(test))
# FALSE  TRUE 
# 4511    87 

# 

ptabela <- lm(Survived~(Sex*-1)+(Pclass<3)+Fare, data = train)
ptabela <- lm(Survived~Sex+Pclass+(Parch < 3)+(SibSp < 5), data = train)

ptabela <- lm(Survived~Sex+( Pclass > 0)+(SibSp < 5 & Age > 0), data = train)

table(dados_full$Age)
ptabela <- lm(Survived~Sex+Pclass+Age, data = train)
ptabela <- lm(Survived~as.factor(Sex)+Age+Pclass, data = train,subset =Pclass<3)
table(dados_full$SibSp)
coef(ptabela)
summary(ptabela)
summary(test)
#0.607 + 0.3892
ptabela

#coef(ptabela)

pc<-predict(ptabela, newdata = test)
pc[is.na(pc)] <- 1

#View(pc)
pr7 <- ifelse(pc < 0.5,0,1)  
pr_data7 <- cbind(test$PassengerId,pr7)
pr_data7 <- as.data.frame(pr_data7)

# Colocando o nome das colunas 
names(pr_data7) <- c("PassengerId", "Survived")

# Verificando Mortos e Vivos
table(pr_data7$Survived)
# VIVOS E MORTOS ( vivos = 1, mortos  = 0)
#   0   1 
# 203 129 

# Valores Nulos
table(is.na(pr_data7))
# FALSE  TRUE 
# 750    86

# Numero de colunas na tabela 
ncol(pr_data7)
# [1] 2

# Tratamento de valores Nulos ???
#  pr_data7[is.na(pr_data7)] <- 0  
str(pr_data7)
# Escrevendo o arquivo em Disco
write.csv(pr_data7, file="predict79.csv", row.names = FALSE)
266 + 152
205+127

418-332

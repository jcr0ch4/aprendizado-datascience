# Leitura dos arquivos
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Execução do modelo
ptabela <- lm(Survived~as.factor(Sex)+Age+Pclass, data = train,subset =Pclass<3)

# Analisando o R Quadrado, Erro Padrão e P-valor
summary(ptabela)
#coef(ptabela)

# Execução do modelo
pc<-predict(ptabela, newdata = test)

# Classificação de Survived, através do gatilho 0.5
pr7 <- ifelse(pc < 0.5,0,1)  

# Montagem do dataframe para saída de dados
pr_data7 <- cbind(test$PassengerId,pr7)
pr_data7 <- as.data.frame(pr_data7)

# Nome das Colunas
names(pr_data7) <- c("PassengerId", "Survived")

# Gravação em disco do arquivo a ser submetido no site Kaggle
write.csv(pr_data7, file="predict73.csv", row.names = FALSE)
rm(list = ls())

trn <- read.csv("training.csv", header = TRUE, sep = ";" )
tst <- read.csv("test.csv", header = TRUE, sep = ";")

### KNN

library(FNN)

set.seed(1234)

# Divisão da base de dados em variáveis resposta e explicativa para os dados de teste e treinamento

x_trn_knn <- trn[,1:11]
y_trn_knn <- trn[,12]

x_tst_knn <- tst[,1:11]
y_tst_knn <- tst[,12]

# Treinamento do modelo

y_pred_knn <- knn(x_trn_knn, x_tst_knn, y_trn_knn, k=2)

# Erro Total de Classificação

mean(y_pred_knn != y_tst_knn)


### Regressão Logística

library(ISLR)

set.seed(1234)

# Montagem do modelo

modelo_rl <- glm(quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, data = trn, family = binomial)

# Previsão da probabilidade

prob_rl <- predict(modelo_rl, newdata = tst[, c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol")], type = "response")

projec_rl <- cbind(tst, prob_rl)

# Achando o ponto de cutoff

library(ROCR)

prev_rl <- prediction(prob_rl, projec_rl$quality)

# Curva ROC

curva_ROC <- performance(prev_rl, "tpr", "fpr")
plot(curva_ROC, col = "red", lwd = 5)
      #olhando a curva ROC, foi decidido um ponto de cutoff de 0.6

# Erro Total de Classificação

table(tst$quality, prob_rl > 0.6)


### CART

set.seed(1234)

library(tree)

# Montagem do modelo

tree_CART <- tree(quality~., data = trn)

# Treinando o modelo

y_pred_CART <- predict(tree_CART, newdata = tst, type = "class")

# Erro Total de Classificação

table(observed = tst$quality, predicted = y_pred_CART)


### Bagging

set.seed(1234)

library(ipred)
library(randomForest)

gbag <- bagging(quality ~ ., data = trn, coob = TRUE)
print(gbag)


### Random Forest

set.seed(1234)

# Montando o modelo

modelo_rf <- randomForest(quality ~ ., data = trn)

# Treinando o modelo

pr_rf <- ?predict(modelo_rf, tst, type = "prob")

y_pred_rf <- predict(modelo_rf,tst)

# Erro Total de Classificação

(test_error_rf <- mean(y_pred_rf != tst$quality))

table(Predicted = y_pred_rf, Observed = tst$quality)


### Boosting

set.seed(1234)

library(fastAdaboost)

# Montando o modelo

modelo_boost <- adaboost(quality ~ ., trn, nIter = 50)

# Treinando o modelo

y_pred_boost <- predict(modelo_boost, newdata = tst)

# Erro Total de Classificação

y_pred_boost$error


### SVM

set.seed(1234)

library(e1071)

# Montagem do modelo

modelo_svm <- svm(quality ~ ., data = trn, kernel = "linear", scale = FALSE, type = "C-classification")

# Treinando o modelo

y_pred_svm <- predict(modelo_svm, tst)

# Erro total de Classificação

mean(y_pred_svm != tst$quality)

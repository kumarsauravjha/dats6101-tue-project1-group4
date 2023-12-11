library(caret)
library (ggplot2)

nfl_players_clean <- read.csv("dataset/nfl_players_clean.csv")

nfl_players_clean$position_group <- factor(nfl_players_clean$position_group)

data <- nfl_players_clean[c("weight", "height", "position_group")]

train_index <- createDataPartition(data$position_group, times=1, p=0.8, list=F)

train_data <- data[train_index,]
test_data <- data[-train_index,]

preProcValues <- preProcess(train_data, method=c("center", "scale"))
train_data <- predict(preProcValues, train_data)
test_data <- predict(preProcValues, test_data)

knn_model <- train(position_group~., data=train_data, method="knn",
                    trControl=trainControl(method="cv"),
                    tuneGrid=data.frame(k=c(20,40,50,100, 200, 250)))


best_knn <- knn3(position_group ~., data=train_data, k=knn_model$bestTune$k)

train_preds <- predict(best_knn, train_data, type="class")
cm1 <- confusionMatrix(train_preds, train_data$position_group)
print(cm1)

preds <- predict(best_knn, test_data, type="class")
cm <- confusionMatrix(preds, test_data$position_group)
print(cm)

test_data_clean <- data[-train_index,]
test_data_clean$pred_knn <- preds
plt <- ggplot(data=test_data_clean) + geom_point(mapping = aes(x=height, y=weight, col=pred_knn)) + 
  labs(title = "Height vs Weight by position group") + labs(x="Height (inches)", y="Weight (lbs)")
print(plt)
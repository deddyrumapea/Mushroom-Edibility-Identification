library(e1071)
library(caret)
# library(devtools)

msrm <- read.csv(file = "data/mushroom_factored.csv", stringsAsFactors = TRUE)

msrm.train.idx <- sample(x = round(nrow(msrm)), size = round(nrow(msrm)*0.5))
msrm.train <- msrm[msrm.train.idx,]
msrm.test <- msrm[-msrm.train.idx,]
write.csv(x = msrm.train, file = "data/mushroom_train.csv", quote = FALSE, row.names = FALSE)
write.csv(x = msrm.test, file = "data/mushroom_test.csv", quote = FALSE, row.names = FALSE)

msrm.svm <- svm(class ~ ., data = msrm.train)
pred <- predict(msrm.svm, msrm.test)
confusionMatrix(pred, msrm.test$class)

saveRDS(msrm.svm, "model/svm.RDS")

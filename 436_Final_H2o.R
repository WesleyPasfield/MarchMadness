## Initialize H2o

library(h2o)
localh2o <- h2o.init(ip = 'localhost', port = 54321, nthreads = -1, max_mem_size = '4g')
h2o_server = h2o.init(nthreads = -1)

## Read in CSV file & remove NAs

setwd("/home/cloudera/Downloads")
model <- read.csv("/home/cloudera/Downloads/RegSeasonFix.csv", header = TRUE, sep = ",")
model[model == '#N/A'] <- NA
model <- na.omit(model)
model$AdjO <- as.numeric(as.character(model$AdjO))
model$AdjD <- as.numeric(as.character(model$AdjD))
model$Tempo <- as.numeric(as.character(model$Tempo))
model$SOS <- as.numeric(as.character(model$SOS))
model$ORebDiff <- as.numeric(as.character(model$ORebDiff))
model$DRebDiff <- as.numeric(as.character(model$DRebDiff))
model$TODiff <- as.numeric(as.character(model$TODiff))
model$X3ptReliance <- as.numeric(as.character(model$X3ptReliance))
model$TS <- as.numeric(as.character(model$TS))
model$TSAllowed <- as.numeric(as.character(model$TSAllowed))
model$BlockDiff <- as.numeric(as.character(model$BlockDiff))
summary(model)

## Create Train and Test files

set.seed(3456)
library(caret)
trainIndex <- createDataPartition(model$BinaryMargin, p = .75,
                                  list = FALSE,
                                  times = 1)
modelTrain <- model[trainIndex,]
modelTest <- model[-trainIndex,]
head(modelTrain)
summary(modelTrain)

write.table(modelTrain, "/home/cloudera/Downloads/RegSeasonNewEdit.csv", sep =",", 
            row.names = FALSE, col.names = TRUE)
write.table(modelTest, "/home/cloudera/Downloads/RegSeasonTest.csv", sep =",", 
            row.names = FALSE, col.names = TRUE)

## Check out demo of H2o, then load in file for modeling & testing then look at summary

demo(h2o.glm)
newFile.hex <- h2o.importFile(localh2o, path="/home/cloudera/Downloads/RegSeasonNewEdit.csv", 
                              key = "newFile.hex")
testFile.hex <- h2o.importFile(localh2o, path="/home/cloudera/Downloads/RegSeasonTest.csv",
                               key = "testFile.hex")
summary(newFile.hex)

newFile.glm <- h2o.glm(x = c("Location", "AdjO", "AdjD", "Tempo", "SOS", "ORebDiff", 
                             "DRebDiff", "TODiff", "X3ptReliance", "TS", "TSAllowed", 
                             "BlockDiff"),
                       data = newFile.hex,y = "BinaryMargin", family = "binomial", 
                       nfolds = 10, alpha = 0.5)
print(newFile.glm)

## Create prediction using Test Set

store.hex <- h2o.predict(newFile.glm, testFile.hex)
h2o.exportFile(store.hex, path = "/home/cloudera/Downloads/validate.csv")
print(store.hex)

validate <- read.csv("/home/cloudera/Downloads/validate.csv")
validateComp <- validate[,1]
testComp <- modelTest[,13:14]
finalValidate <- cbind(validateComp, testComp)
finalValidate <- as.data.frame(finalValidate)
colnames(finalValidate)
finalValidate$check <- finalValidate$validate - finalValidate$BinaryMargin

finalValidate$checker[finalValidate$check == 1] <- "MissedUpset"
finalValidate$checker[finalValidate$check == 0] <- "Correct"
finalValidate$checker[finalValidate$check == -1] <- "Upset"

table(finalValidate$checker, finalValidate$Margin)

## Create Prediction vs. Tourney results

tourney <- read.csv("/home/cloudera/Downloads/tourneyResults(4).csv")

tourney.hex <- h2o.importFile(localh2o, path="/home/cloudera/Downloads/tourneyResults(4).csv", 
                              key = "tourney.hex")
summary(tourney.hex)

store.hex <- h2o.predict(newFile.glm, tourney.hex)
h2o.exportFile(store.hex, path = "/home/cloudera/Downloads/validate3.csv")
print(store.hex)

## Validate Results

validate <- read.csv("/home/cloudera/Downloads/validate3.csv")
validateComp <- validate[,1]
testComp <- tourney[,13:14]
finalValidate <- cbind(validateComp, testComp)
finalValidate <- as.data.frame(finalValidate)
colnames(finalValidate)
finalValidate$check <- finalValidate$validate - finalValidate$BinaryMargin

## Check accuracy of results

finalValidate$checker[finalValidate$check == 1] <- "MissedUpset"
finalValidate$checker[finalValidate$check == 0] <- "Correct"
finalValidate$checker[finalValidate$check == -1] <- "Upset"

table(finalValidate$checker, finalValidate$Margin)

## Edit Submission File

submission <- read.csv("/home/cloudera/Downloads/SubmissionTest.csv")
submission <- submission[,-13]
write.table(submission, "/home/cloudera/Downloads/SubmissionTestNew.csv", sep =",", 
            row.names = FALSE, col.names = TRUE)

## Create Submission H2o file

submission.hex <- h2o.importFile(localh2o, path="/home/cloudera/Downloads/SubmissionTestNew.csv", 
                              key = "submission.hex")
summary(submission.hex)

## Create Predictions for GLM model

store.hex <- h2o.predict(newFile.glm, submission.hex)
h2o.exportFile(store.hex, path = "/home/cloudera/Downloads/validate6.csv")
print(store.hex)

## Random Forest Model

newModel <- h2o.randomForest(x = c("Location", "AdjO", "AdjD", "Tempo", "SOS", "ORebDiff",
                               "DRebDiff", "TODiff", "X3ptReliance", "TS", "TSAllowed",
                               "BlockDiff"), y = "BinaryMargin",
                             data = newFile.hex,
                             classification = TRUE,
                             ntree = 600,
                             depth = 7,
                             validation = testFile.hex)
print(newModel)

## Create Random Forest Predictions

tourneyTest <- h2o.predict(newModel, submission.hex)
tourneyTestDF <- as.data.frame(tourneyTest)
write.table(tourneyTestDF, "/home/cloudera/Downloads/SubmissionTestRF2.csv", sep =",", 
            row.names = FALSE, col.names = TRUE)

## GBM Model

newModelGBM <- h2o.gbm(x = c("Location", "AdjO", "AdjD", "Tempo", "SOS", "ORebDiff",
                                   "DRebDiff", "TODiff", "X3ptReliance", "TS", "TSAllowed",
                                   "BlockDiff"), y = "BinaryMargin",
                    distribution = "multinomial",
                    data = newFile.hex,
                    n.trees = 5000,
                    interaction.depth = 5,
                    shrinkage = .01,
                    validation = testFile.hex)
## Create GBM Predictions

print(newModelGBM)
tourneyTest <- h2o.predict(newModelGBM, submission.hex)
tourneyTestDF <- as.data.frame(tourneyTest)
write.table(tourneyTestDF, "/home/cloudera/Downloads/SubmissionTestGBM.csv", sep =",", 
            row.names = FALSE, col.names = TRUE)
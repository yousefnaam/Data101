citybike.train <- read.csv"~/Downloads/citybike.train.csv")
View(citybike.train)
plot(citybike.train$bikeid,citybike.train$tripduration)
summary(citybike.train$tripduration)
summary(citybike.train$bikeid)

bikeidModel = lm(tripduration~bikeid, data=citybike.train)
summary(bikeidModel)
citybike.test100 = read.csv("~/Downloads/citybike.test100.csv")
View(citybike.test100)
bikeidpredictions = predict(bikeid.lm.model,citybike.test100)
citybike.test100withduration <- read.csv("~/Downloads/citybike.test100withduration.csv")

citybike.train$Day = as.numeric(substr(citybike.train$starttime,9,10))

day.lm.model = lm(tripduration~Day, data=citybike.train)
citybike.test100$Day = as.numeric(substr(citybike.test100$starttime,9,10))
day.lm.predictions = predict(day.lm.model,citybike.test100)

all.predictions = data.frame(Predicted=rep(600,10000))
View(all.predictions)
write.csv(all.predictions,file="all.predictions.csv")

testData = read.csv("Downloads/citybike.train.csv")
DataA <- read.csv("/Downloads/citybike.test10000A.csv")
DataB = read.csv("/Downloads/citybike.test10000B.csv")
f10000Amodel = function() {
  weight = lm(tripduration~I(start.station.latitude^2), data=testData)
  predicted = predict(weight, testData)
  testData$predicted = predicted
  summary(weight)
  regr.error(predicted, testData$tripduration) 
  
  lapply(predicted, write, "errors.txt", append=TRUE)
}

fun_10000B_model <- function() {
  trainingData$Day <- as.numeric(substr(trainingData$starttime,9,10))
  my_weight = lm(tripduration~start.station.latitude + I(bikeid^3) + Day + start.station.id, data=trainingData)
  testDataB$Day <- as.numeric(substr(testDataB$starttime,9,10))
  prediction = predict(my_weight, testDataB)
  write.csv(prediction, file="~/test.csv")
  data = read.csv("~/test.csv")
  file.remove("~/test.csv")
  colnames(data) = c("Id", "Predicted")
  write.csv(data, file="~predictions.csv", row.names = FALSE)
  
  summary(my_weight)
}

fun_10000A_model()
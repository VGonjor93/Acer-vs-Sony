####Preparing necessary libraries and packages (create_report by ggGally data esplorer and ggpairs)-------

install.packages("caret", dep=TRUE)
install.packages("C50", dependencies = TRUE)
install.packages("inum")
install.packages("ggthemes")
library(caret)
library(mlbench)
library(readr)
library(C50)
library(ggplot2)
library(ggthemes)

#outliers <- boxplot(data$cal1)$out

####Loading data needed#####

CompleteResponses <- read_csv("C:/Users/poni6/Downloads/CompleteResponses.csv")
View(CompleteResponses)
str(CompleteResponses)


SurveyIncomplete <- read_csv("C:/Users/poni6/Downloads/SurveyIncomplete.csv")
View(SurveyIncomplete)
str(SurveyIncomplete)


####Preprocessing data####

CompleteResponses$elevel<-as.ordered(CompleteResponses$elevel)
CompleteResponses$age<-as.integer(CompleteResponses$age)
CompleteResponses$zipcode<-as.factor(CompleteResponses$zipcode)
CompleteResponses$car<-as.factor(CompleteResponses$car)
CompleteResponses$brand<-as.factor(CompleteResponses$brand)
summary(CompleteResponses)
anyNA(CompleteResponses)

SurveyIncomplete$elevel<-as.ordered(SurveyIncomplete$elevel)
SurveyIncomplete$age<-as.integer(SurveyIncomplete$age)
SurveyIncomplete$zipcode<-as.factor(SurveyIncomplete$zipcode)
SurveyIncomplete$car<-as.factor(SurveyIncomplete$car)
SurveyIncomplete$brand<-as.factor(SurveyIncomplete$brand)
summary(SurveyIncomplete)

####Data partition####

set.seed(420)
inTrain <- createDataPartition(CompleteResponses$brand, p = 0.75,  list = FALSE)
trainSize <- CompleteResponses[ inTrain,]
testSize <- CompleteResponses[ -inTrain,]
nrow(trainSize)
nrow(testSize)


####Plotting####

ggplot(CompleteResponses, aes(x=age, y=salary)) + geom_point()
ggplot(CompleteResponses, aes(x=brand, fill=brand)) + geom_bar(color="black") + labs(title = "Salary vs Age") 
ggplot(CompleteResponses, aes(x=salary, fill=brand)) + geom_histogram( colour= "black", bins=20)
ggplot(CompleteResponses, aes(x=salary)) + geom_histogram(bins = 20, fill="lightblue", colour="darkblue") + facet_wrap(~elevel, scales = "free_x")
ggplot(CompleteResponses, aes(x=age, y=salary, col=brand)) + geom_point() + labs(title = "Salary vs Age") + scale_color_manual(name = "Brands", labels = c("Acer", "Sony"), values = c("orchid3", "goldenrod3"))
ggplot(CompleteResponses, aes(x=brand, y=salary, fill=brand)) + geom_boxplot()
ggplot(CompleteResponses, aes(x="", fill=brand)) + geom_bar(color="black") + coord_polar(theta = "y") + geom_text(y=cumsum(brand), color = "white") + labs(title = "Brand Preference") + scale_fill_manual(name = "Brands", labels = c("Acer", "Sony"), values = c("orchid3", "goldenrod3"))
ggplot(CompleteResponses, aes(x=salary, fill=brand)) + geom_histogram( colour= "black", bins=10) + labs(title = "Relationship between brand and salary") + scale_x_continuous(breaks = seq(20000,150000,15000)) + theme(axis.text.x = element_text(angle=60, hjust=1)) + scale_y_continuous(labels = scales::percent) + theme_economist_white()

ggplot(SurveyIncomplete, aes(x="", fill=SurveyIncomplete$PredictedBrand)) + geom_bar(color="black") + coord_polar(theta = "y") + labs(title = "Predicted Brand Preference") + scale_fill_manual(name = "Brands", labels = c("Acer", "Sony"), values = c("orchid3", "goldenrod3"))
ggplot(CompleteResponses, aes(x=brand, fill=brand)) + geom_bar(color="black") + labs(title = "Salary vs Age") 



####Model Training####

fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

c5Model <- train(brand ~ ., data = trainSize ,method = "C5.0", trControl=fitControl, tuneLength = 1)
c5Model
summary(c5Model)
plot(c5Model)

rfModel <- train(brand ~ ., data = trainSize, method = "rf", trControl=fitControl, tuneLength = 1)
rfModel
summary(rfModel)
varImp(rfModel)


c5Model2 <- train(brand ~ salary+age, data = trainSize ,method = "C5.0", trControl=fitControl, tuneLength = 1)
c5Model2
summary(c5Model2)
plot(c5Model2)

rfModel2 <- train(brand ~ salary+age, data = trainSize, method = "rf", trControl=fitControl, tuneLength = 1)
rfModel2
summary(rfModel2)
varImp(rfModel2)



rfGrid <- expand.grid(mtry=c(20,21,22))
system.time(rfModel3 <- train(brand~ salary+age, data = trainSize, method = "rf", trControl=fitControl, tuneGrid=rfGrid))
rfModel3
varImp(rfModel3)

####Final model rf####

rfGridFinal <- expand.grid(mtry=21)
system.time(rfModelFinal <- train(brand~ ., data = trainSize, methor = "rf", trControl= fitControl, tuneGrid=rfGridFinal))
rfModelFinal
varImp(rfModelFinal)

####Final model c5####

c5Model <- train(brand ~ ., data = trainSize ,method = "C5.0", trControl=fitControl, tuneLength = 1)
c5Model

####Prediction####

predictionrf <- predict(rfModelFinal, testSize)
postResample(predictionrf, testSize$brand)
summary(predictionrf)

predictionC5 <- predict(c5Model, testSize)
postResample(predictionC5, testSize$brand)
summary(predictionC5)

FinalPrediction <- predict(rfModelFinal, SurveyIncomplete)
summary(FinalPrediction)
View(FinalPrediction)

postResample(FinalPrediction, testSize$brand)


SurveyIncomplete["PredictedBrand"] <- FinalPrediction

totalAcer <-  5632
totalSony <-  9266
plot(x=totalAcer, y=totalSony)

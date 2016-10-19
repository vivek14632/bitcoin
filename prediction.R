
bitcoin<-read.csv(file.choose(),header = T)
#bitcoin_jan_15_to_june_16.csv . Its in box folder
bitcoin$Date<-as.Date(bitcoin$Date)

#training till may
training<-bitcoin[bitcoin$Date<='2016-05-31',]
#testing for june
test<-bitcoin[bitcoin$Date>'2016-05-31',]

#Autoregressive model
arModel<-ar(training$Close.Price)
arModelPrediction<-predict(arModel,n.ahead = 29)


plot(test$Close.Price,type="b",ylim = c(1,1000),xlab = "Hours",
     ylab = "USD", main = "Prediction for July 2016")
lines(as.numeric(arModelPrediction$pred),pch=2,type = "b")

#There are different algorithms for AR model. In the following code, we evaluate different algorithms
arModel<-ar(training$Close.Price,model="yule-walker")
arModelPrediction<-predict(arModel,n.ahead = 29)
lines(as.numeric(arModelPrediction$pred),pch=3,type = "b")

arModel<-ar(training$Close.Price,model="burg")
arModelPrediction<-predict(arModel,n.ahead = 29)
lines(as.numeric(arModelPrediction$pred),pch=4,type = "b")
?ar

arModel<-ar(training$Close.Price,model="ols")
arModelPrediction<-predict(arModel,n.ahead = 29)
lines(as.numeric(arModelPrediction$pred),pch=5,type = "b")

arModel<-ar(training$Close.Price,model="mle")
arModelPrediction<-predict(arModel,n.ahead = 29)
lines(as.numeric(arModelPrediction$pred),pch=6,type = "b")

arModel<-ar(training$Close.Price,model="yw")
arModelPrediction<-predict(arModel,n.ahead = 29)
lines(as.numeric(arModelPrediction$pred),pch=7,type = "b")


legend("bottom",c("Actual","AR prediction",
                  "AR-yule-walker","AR-Burg","AR-ols","AR-mle",
                  "AR-yw"),pch=c(1,2,3,4,5,6,7))

#I have stored the plot in the the box folder: bitcoin/results

#Function to calculate RMSE
rmse<-function(x,y)
{
  sqrt(sum((x-y)^2)/length(x))
}

rmse(test$Close.Price,as.numeric(arModelPrediction$pred))


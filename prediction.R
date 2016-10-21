
bitcoin<-read.csv(file.choose(),header = T)
#bitcoin_jan_15_to_june_16.csv . Its in box folder
bitcoin$Date<-as.Date(bitcoin$Date)

#training till may
training<-bitcoin[bitcoin$Date<='2016-05-31',]
#testing for june
test<-bitcoin[bitcoin$Date>'2016-05-31',]

#Autoregressive model
# Here it is taking default order (p). We have to explore by giving different orders
arModel<-ar(training$Close.Price)
arModelPrediction<-predict(arModel,n.ahead = 29)
#RMSE (Roor Mean Square Error). The function is defined at the bottom of the code.
#Actually, that piece of code should be run first before applying the function.
#In the following line, we are applying RMSC function on the testing data
rmse(test$Close.Price,as.numeric(arModelPrediction$pred))

# Plotting the test values. 'ylim' is to mention the range of y values. R will consider this range for all the plots. 
plot(test$Close.Price,type="b",ylim = c(1,1000),xlab = "Hours",
     ylab = "USD", main = "Prediction for July 2016")

#Plotting the predicted values. pch=2 displays the points as *.
lines(as.numeric(arModelPrediction$pred),pch=2,type = "b")

#There are different algorithms for AR model. In the following code, we evaluate different algorithms
arModel<-ar(training$Close.Price,model="yule-walker")
arModelPrediction<-predict(arModel,n.ahead = 29)
lines(as.numeric(arModelPrediction$pred),pch=3,type = "b")
rmse(test$Close.Price,as.numeric(arModelPrediction$pred))

arModel<-ar(training$Close.Price,model="burg")
arModelPrediction<-predict(arModel,n.ahead = 29)
lines(as.numeric(arModelPrediction$pred),pch=4,type = "b")
rmse(test$Close.Price,as.numeric(arModelPrediction$pred))

arModel<-ar(training$Close.Price,model="ols")
arModelPrediction<-predict(arModel,n.ahead = 29)
lines(as.numeric(arModelPrediction$pred),pch=5,type = "b")
rmse(test$Close.Price,as.numeric(arModelPrediction$pred))

arModel<-ar(training$Close.Price,model="mle")
arModelPrediction<-predict(arModel,n.ahead = 29)
lines(as.numeric(arModelPrediction$pred),pch=6,type = "b")
rmse(test$Close.Price,as.numeric(arModelPrediction$pred))

arModel<-ar(training$Close.Price,model="yw")
arModelPrediction<-predict(arModel,n.ahead = 29)
lines(as.numeric(arModelPrediction$pred),pch=7,type = "b")
rmse(test$Close.Price,as.numeric(arModelPrediction$pred))

#show the legends for each of the models.
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

#TODO: write function for MAE (Mean Absolute Error) as above and run it for all the models. We shall see if there is any significant difference in the error among  different models

bitcoin<-read.csv(file.choose(),header = T)
#bitcoin_jan_15_to_june_16.csv . Its in box folder
bitcoin$Date<-as.Date(bitcoin$Date)

#training till may
training<-bitcoin[bitcoin$Date<='2016-05-31',]
#testing for june
test<-bitcoin[bitcoin$Date>'2016-05-31',]

#Autoregressive model
# Here it is taking default order (p). We have to explore by giving different orders
arModel<-ar(training$Close.Price)

arModelPrediction<-predict(arModel,n.ahead = 29)

rmse<-function(x,y)
{
  sqrt(sum((x-y)^2)/length(x))
}

mape<-function(x,y)
{
  (sum(abs(x-y)/x)/length(x))
}
rmseM<-rep(0,29)
mapeM<-rep(0,29)
for (i in 1:29)
{
  rmseM[i]<-rmse(test$Close.Price[1:i],as.numeric(arModelPrediction$pred)[1:i])
}

rmseM

for (i in 1:29)
{
  mapeM[i]<-mape(test$Close.Price[1:i],as.numeric(arModelPrediction$pred)[1:i])
}

plot(rmseM,type="b")
?plot

plot(mapeM,type="b",xlab="Number of Days of Prediction",ylab="MAPE",main="AR model")

plot(rmseM,type="b",xlab="Number of Days of Prediction",ylab="RMSE",main="AR model")


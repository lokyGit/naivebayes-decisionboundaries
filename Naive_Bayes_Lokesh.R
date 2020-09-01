decisionplot = function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl = data[,class] else cl = 1
  data = data[,1:2]
  k = length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # making the grid
  r = sapply(data, range, na.rm = TRUE)
  xs = seq(r[1,1], r[2,1], length.out = resolution)
  ys = seq(r[1,2], r[2,2], length.out = resolution)
  g = cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) = colnames(r)
  g = as.data.frame(g)
  
  ## getting class labels from predict function
  
  p = predict(model, g, type = predict_type)
  if(is.list(p)) p = p$class
  p = as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z = matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}

# loading the dataset
data = read.csv("/Users/lokeshpalacharla/Library/Mobile Documents/com~apple~CloudDocs/NEU/Classes/Summer 2020/Predictive Analytics/Week 5/naive_bayes.csv",
                header = T)
# Looking at the structure of the data label
str(data$label)
# Changing the label to a factor
data$label = as.factor(data$label)
# Looking at the structure of the data after type conversion
str(data)
# looking at the head of the dataset
head(data)
# plotting the label as col
plot(data[,1:2], col = data[,3])


## Decision boundary for naive bayes model
library(e1071)
library(caret)

model_nb = naiveBayes(label ~ ., data=data)

# plotting the decision boundaries
decisionplot(model_nb, data, class = "label", main = "Naive Bayes")

# Calculating the prediction accuracy
predict_nb =  predict(model_nb,data)
cm_nb =  confusionMatrix(predict_nb,data$label) #Accuracy : 96.82% 

## Decision boundary for multinomial logistic regression mdel
library(nnet)
ml = data
str(ml)
#Setting the reference foor the multinomial logistic regression
ml$label = relevel(ml$label, ref = "3") 
# multinomial logistic regression
model_ml =  multinom(label ~ ., data = ml)
# plotting the decision boundaries
decisionplot(model_ml, ml, class = "label", main = "Multinomial Logistic Regression")
# Calculating the prediction accuracy
predict_ml = predict(model_ml,ml)
cm_ml =  confusionMatrix(predict_ml,ml$label) #Accuracy : 96.82%   

#Validating with other method
pred_tab_ml = table(Prediction = predict_ml,Actual = ml$label)
pred_error_ml =  1 - sum(diag(pred_tab_ml))/sum(pred_tab_ml)
error_percentage_ml = round(pred_error_ml*100,2) # 3.18% error
pred_accuracy_ml = round((1 - pred_error_ml)*100,2) # 96.82%


## Single Layered Neural Network
library(nnet)
nn = data
# Single layered Neural network
model_nn = nnet(label ~ ., data=nn, size = 1, maxit = 1000, trace = F)
# plotting the decision boundaries
decisionplot(model_nn, nn, class = "label", main = "Single Layered Neural Network")
# Calculating the prediction accuracy
predict_nn =  predict(model_nn,nn,type = "class")
predict_nn=as.factor(predict_nn)
cm_nn = confusionMatrix(predict_nn,nn$label) # Accuaracy - 95%

#Validating the accuracy through manual accuracy calculation
pred_tab_nn1 = table(Prediction = predict_nn,Actual = nn$label)
pred_error_nn1 =  1 - sum(diag(pred_tab_nn1))/sum(pred_tab_nn1)
error_percentage_nn1 = round(pred_error_nn1*100,2) # 5% error
pred_accuracy_nn1 = round((1 - pred_error_nn1)*100,2) #  95%


## Double Layered Neural Network
model_nn2 = nnet(label ~ ., data=nn, size = 2, maxit = 1000, trace = F)
# plotting the decision boundaries
decisionplot(model_nn2, nn, class = "label", main = "Double Layered Neural Network")
# Calculating the prediction accuracy
predict_nn2 =  predict(model_nn2,nn,type = "class")
predict_nn2=as.factor(predict_nn2)
cm_nn2 = confusionMatrix(predict_nn2,nn$label) # Accuracy: 96.82% 

## four Layered Neural Network
model_nn4 = nnet(label ~ ., data=nn, size = 4, maxit = 1000, trace = F)
# plotting the decision boundaries
decisionplot(model_nn4, nn, class = "label", main = "Four Layered Neural Network")
# Calculating the prediction accuracy
predict_nn4 =  predict(model_nn4,nn,type = "class")
predict_nn4=as.factor(predict_nn4)
cm_nn4 = confusionMatrix(predict_nn4,nn$label) # Accuracy: 98.18% 


## Six Layered Neural Network
model_nn6 = nnet(label ~ ., data=nn, size = 6, maxit = 1000, trace = F)
# plotting the decision boundaries
decisionplot(model_nn6, nn, class = "label", main = "Six Layered Neural Network")
# Calculating the prediction accuracy
predict_nn6 =  predict(model_nn6,nn,type = "class")
predict_nn6=as.factor(predict_nn6)
cm_nn6 = confusionMatrix(predict_nn6,nn$label) # Accuracy: 99.09% 

## Nine Layered Neural Network
model_nn9 = nnet(label ~ ., data=nn, size = 9, maxit = 1000, trace = F)
# plotting the decision boundaries
decisionplot(model_nn9, nn, class = "label", main = "Nine Layered Neural Network")
# Calculating the prediction accuracy
predict_nn9 =  predict(model_nn9,nn,type = "class")
predict_nn9=as.factor(predict_nn9)
cm_nn9 = confusionMatrix(predict_nn9,nn$label) # Accuracy: 100% 











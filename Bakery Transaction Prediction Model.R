# Prediction of Coffee Sales Per Day
rm(list=ls())

# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
# library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
# library(rsample)
# library(yardstick) 

# Modeling
library(keras)
library(tensorflow)

setwd("C:\\dana\\Newie")

source("Bakery Transaction Analysis.R",encoding="utf-8")
source("essential functions.R",encoding="utf-8")

coffee_data = coffee_data [,-c(2,3)]
coffee_data_group = coffee_data %>% group_by (Date=Date) %>%  summarize( Count = n() ) 
head(coffee_data_group)

#### use ts dat#############
# coffee_data_ts=as.ts(coffee_data_group$Count)
# 
# coffee_sales <- coffee_data_ts %>%
#   tk_tbl() %>%
#   mutate(index = as_date(index)) %>%
#   as_tbl_time(value = Count)

##### transform data to stationarity
# diffed = diff(coffee_data_group$Count, differences = 7)
# head(diffed)
# 
# supervised = lag_transform(diffed, 7)
# head(supervised)



#plot(coffee_data_group)
theme_set(theme_minimal())

ggplot(data = coffee_data_group, aes(x = seq_along(Date), y = Count))+
  geom_line(color = "#00AFBB", size = 1)+
  geom_vline(xintercept = seq(from=7,to=lengths(coffee_data_group)[2],by=7),linetype="dotted")+
  theme_minimal()


############# plot ACF############
# train %>%
#   tidy_acf(Count, lags = 0:max_lag)
# 
# max_lag = 7 * 4 #predict for 7*4 days (4 weeks)
# train %>%
#   tidy_acf( train, Count, lags = 0:max_lag) %>%
#   ggplot(aes(lag, acf)) +
#   geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
#   geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
#   annotate("text", label = "10 Year Mark", x = 130, y = 0.8, 
#            color = palette_light()[[2]], size = 6, hjust = 0) +
#   theme_tq() +
#   labs(title = "ACF: Coffee Sales")

#create X_train and y_train datas with timesteps = 7 for feed our LSTM model
X = data.frame()
Y = data.frame()
timesteps = 7
for (i in (timesteps): (lengths(coffee_data_group )[2]) )
{
  # i=8
  X=rbind(X,t(coffee_data_group[(i-timesteps+1):i,2]))
  Y=rbind(Y,coffee_data_group[i+1,2])
}
Data=cbind(X,Y)
colnames(Data) <- c( paste0('x-', 7), paste0('x-', 6),paste0('x-', 5),paste0('x-', 4),paste0('x-', 3),paste0('x-', 2),paste0('x-', 1),'x')
rownames(Data) <- c(seq(1,nrow(Data),1))
Data<-Data[-lengths(Data)[2],]

#split train and test data
len=lengths(Data)[2]

train <- Data[1:(len*0.8),]
test <- Data[floor((len*0.8)+1):len,]

summary(train)

#normalize the varibles
# train_scaled =train
# ind <- sapply(train_scaled , is.numeric)
# train_scaled [ind] <- lapply(train_scaled [ind], range01)

## scale data
scale_data = function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
  
}

Scaled = scale_data(train, test, c(0, 1))

y_train = Scaled$scaled_train[, timesteps+1]
x_train = Scaled$scaled_train[, 1:(timesteps)]

y_test = Scaled$scaled_test[, timesteps+1]
x_test = Scaled$scaled_test[, 1:(timesteps)]

## inverse-transform
invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for( i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}

x_train=data.matrix(x_train)
x_test=data.matrix(x_test)

# Reshape the input to 3-dim
dim(x_train)<-c(dim(x_train)[1],timesteps,1)
dim(x_test)<-c(dim(x_test)[1],timesteps,1)

# specify required arguments
X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1                # must be a common factor of both the train and test samples
units = 1                     # can adjust this, in model tuninig phase

#=========================================================================================

model <- keras_model_sequential() 
model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE)%>%
  layer_dense(units = 1)

#compible the model
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)

#model summary
summary(model)
# ____________________________________________________________________________________
# Layer (type)                         Output Shape                      Param #      
# ====================================================================================
#   lstm_10 (LSTM)                       (1, 1)                            12           
# ____________________________________________________________________________________
# dense_10 (Dense)                     (1, 1)                            2            
# ====================================================================================
#   Total params: 14
# Trainable params: 14
# Non-trainable params: 0
# ____________________________________________________________________________________

#fit the model
Epochs = 300   
for(i in 1:Epochs ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}

#make predictions
L = dim(x_test)[1]
scaler = Scaled$scaler
predictions = numeric(L)

for(i in 1:L){
  X = x_test[i,,]
  dim(X) = c(1,7,1)
  yhat = model %>% predict(X, batch_size=batch_size)
  # invert scaling
  yhat = invert_scaling(yhat, scaler,  c(0, 1))
  # invert differencing
  # yhat  = yhat + Series[(n+i)]
  # store
  predictions[i] <- yhat
}

#Plot the values, We will compare Predicted Coffe Sales and Real Coffee Sales
y_test = invert_scaling(y_test, scaler,  c(0, 1))
real_coffe_sales = y_test

y=real_coffe_sales
fitting1=as.data.frame(y)
fitting1$label="real"
fitting1$x=seq(1,length(y_test))
y=predictions
fitting2=as.data.frame(y)
fitting2$label="pred"
fitting2$x=seq(1,length(y_test))
fitting=rbind(fitting1,fitting2)

ggplot(fitting, aes(y = y, x = x, colour = label)) +
 geom_point() + geom_smooth(span = 0.1, fill = NA)


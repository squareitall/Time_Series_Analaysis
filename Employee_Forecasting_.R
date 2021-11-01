library(fpp3)
library(dplyr)
library(reshape2)


df=filter(tourism,Region %in% c('Sydney','South Coast','North Coast NSW') & Purpose=='Holiday')
#Index is in quarter steps and region,
#state and purpose serves as the observation columns

####################################################### Checking distribution of sum of trips###########
df_without_quarter=df[,!c(colnames(df) %in% c('Quarter'))]

g_reg_trip=group_by(df_without_quarter,Region)%>%summarise(total_trips=sum(Trips))
g_reg_trip=group_by(df,Region)%>%summarise(total_trips=sum(Trips))

#spread is like unstacking or pivoting
df_trips_sum=spread(g_reg_trip,Region,total_trips)

num_cols=unlist(lapply(df_trips_sum,is.numeric))
sum_3_reg=apply(df_trips_sum[,num_cols],1,sum)

df_trips_sum[,num_cols]=df_trips_sum[,num_cols]/sum_3_reg

df_melt = melt(df_trips_sum ,  id.vars = 'Quarter', variable.name = 'trip_sum')

#Number of trips follows a ratio 0f 3.4:3.6:3 for Sydney:NSW:SC
ggplot(df_melt, aes(Quarter,value)) + geom_line(aes(colour = trip_sum))

####################################################### Checking distribution of sum of trips###########

eval_demand = function(x){
  
  if (substr(x['Region'],1,2)=='Sy'){
    as.integer(x['Trips'])*5*0.03}
  else{
    as.integer(x['Trips'])*2*0.04
  }}


df=mutate(df,demand=apply(df[,c('Region','Trips')],1,eval_demand))


##############Splitting into train and test set
df_train=subset(df,Quarter >= yearquarter('2008 Q1') & Quarter <= yearquarter('2016 Q4'))

df_test=subset(df,Quarter >= yearquarter('2017 Q1'))
##############################################


###Need to create demand column that is product of number trips and hotel_nights for each trip###
##Additionally need to segregate the df_train data region wise and then train respective forecasting model on each dataset##
################



forecast_models=model(df_train,
                      m.bst=ETS(demand),
                      m.AAM = ETS(demand ~ error("A") + trend("A") + season("M")),
                      m.AAdM=ETS(demand ~ error('A') + trend('Ad')+season('M'))
                      )

#tidy(forecast_models)
#augment(forecast_models)

forecast_models
report(forecast_models)

# North Coast NSW ETS(M,N,M)- Multiplicative error with no trend and Multiplicative seasonality  254 258
# South Coast     ETS(M,N,M)- Multiplicative error with no trend and Multiplicative seasonality   235  239
# Sydney          ETS(A,N,A)- Additive error No Trend and Additive Seasonality                    288 292



#### Forecast for coming four quarters from the best model
f.nsw=forecast_models %>% filter(Region == 'North Coast NSW')%>% select(m.bst) %>%
  forecast(h=4)

f.sc=forecast_models %>% filter(Region == 'South Coast')%>% select(m.bst) %>%
  forecast(h=4)

f.sy=forecast_models %>% filter(Region == 'Sydney')%>% select(m.bst) %>%
forecast(h=4)


#forecast_models %>% filter(Region == 'Sydney')%>% select(m.bst) %>% report()



df_test$demand=apply(df_test[,c('Region','Trips')],1,eval_demand)
df$demand=apply(df[,c('Region','Trips')],1,eval_demand)


forecast_best_models=model(df_train,
                      m.bst=ETS(demand))


f=forecast_best_models%>%forecast(h=4)

#Get MAPE scores for test or forecast values
accuracy(f,df_test)


train_aug=augment(forecast_best_models)
train_aug=mutate(train_aug,MAPE=as.numeric(abs((100*(.resid))/demand))) #yt-yest/yt

#MAPE of training set
train_aug[,!c(colnames(train_aug) %in% c('Quarter'))] %>% group_by(Region)%>% summarise(mean(MAPE))



#Ans 1.3 left #MAPE for the combined prediction
############################ANS 1-3 ends




########Ans 4-6 is centered around mutation
df=select(df,Region,demand)
df_total=spread(df,Region,demand)#pivoting


df_total=mutate(df_total,
       demand=apply(df_total[c('North Coast NSW','South Coast','Sydney')],1,sum))

df_final=select(df_total,Quarter,demand)


df_train=subset(df_final,Quarter >= yearquarter('2008 Q1') & Quarter <= yearquarter('2016 Q4'))

df_test=subset(df_final,Quarter >= yearquarter('2017 Q1'))
forecast_models_agg=model(df_train,
                      m.bst=ETS(demand),
                      m.AAM = ETS(demand ~ error("A") + trend("A") + season("M")),
                      m.AAdM=ETS(demand ~ error('A') + trend('Ad')+season('M'))
)


report(forecast_models_agg)

forecast_models_agg
#The lowest AIC is obtained for the auto model that is ANA based ETS model, with additive error no trend and additive seasonality.

agg_bst_model=model(df_train,
                    m.bst=ETS(demand))

f=agg_bst_model%>%forecast(h=4)

accuracy(f,df_test)
accuracy(f,df_final)


train_aug=augment(agg_bst_model)
train_aug=mutate(train_aug,MAPE=as.numeric(abs((100*(.resid))/demand))) #yt-yest/yt

#MAPE of training set
train_aug[,!c(colnames(train_aug) %in% c('Quarter'))]%>% summarise(mean(MAPE))

#4.84 is the training MAPE for the aggregated data


# Test MAPE for the aggregated data model is significantly low as compared to three different cluster models
## This may be attributed to interaction between clusters of data and increase in total number of data that lead to decrease in prediction error

#I will opt to perform analysis on the aggregated dataset and will be using the best ETS model obtained from the test train analysis
## Best model obtained is the ETS(A,N,A)
# The forecast model should be built on the collected data all-together and have additive error form with no trend and additive seasonality 



############Ans 7-9############################

df=select(df,Region,demand)
df_total=spread(df,Region,demand)#pivoting


df_total=mutate(df_total,
                demand=apply(df_total[c('North Coast NSW','South Coast','Sydney')],1,sum))

df_final=select(df_total,Quarter,demand)

#df_final consist of the entire data spanning from first quarter of 1998 till the last quarter of 2017
#I will train a ETS(A,N,A) forecast model on this entire dataset


forecast_model_final=model(df_final,
                          m.AAN = ETS(demand ~ error("A") + trend("N") + season("A"))
)

report(forecast_model_final)


train_aug=augment(forecast_model_final)
train_aug=mutate(train_aug,MAPE=as.numeric(abs((100*(.resid))/demand))) #yt-yest/yt

#MAPE of training set
train_aug[,!c(colnames(train_aug) %in% c('Quarter'))]%>% summarise(mean(MAPE))

#Training MAPE is 6.02 when trained on the entire dataset

#Plotting the forecast for upcoming quarters
forecast_2018=forecast(forecast_model_final,h=4)




#Predicting boundary limits of our estimate where we are 99 percent time sure that the true value will lie
# We have lower_bound estimate and upper bound of the estimate for all the quarters
forecast_2018%>%hilo(level =c(99))%>%unpack_hilo('99%')



### Reevaluation of our estimates for the filtered data
##Only the training data will be change.

df_train=subset(df_final,Quarter >= yearquarter('2010 Q1'))#Exclude data older than 2010 from training set


forecast_model_final_pruned=model(df_train,
                           m.AAN = ETS(demand ~ error("A") + trend("N") + season("A"))
)

report(forecast_model_final_pruned)


train_aug=augment(forecast_model_final_pruned)
train_aug=mutate(train_aug,MAPE=as.numeric(abs((100*(.resid))/demand))) #yt-yest/yt

#MAPE of training set
train_aug[,!c(colnames(train_aug) %in% c('Quarter'))]%>% summarise(mean(MAPE))

#Training MAPE is 4.54 when trained on the entire dataset

#Plotting the forecast for upcoming quarters
forecast_2018_fromPruned=forecast(forecast_model_final_pruned,h=4)


autoplot(forecast_2018_fromPruned)

#Predicting boundary limits of our estimate where we are 99 percent time sure that the true value will lie
# We have lower_bound estimate and upper bound of the estimate for all the quarters
forecast_2018_fromPruned%>%hilo(level =c(99))%>%unpack_hilo('99%')





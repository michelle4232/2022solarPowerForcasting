library(dplyr)
library(mice)
library(caret)
library(xgboost)
library(gbm)

###讀檔###
input_file = read.csv("train.csv",header = T,sep=",")
input_file

#檢視檔案內容
summary(input_file)
#sum(is.na(input_file[,1]))
#有na:  Temp_m:1458    Irradiance:24      Temp:15


###資料前處理###
#擷取日期字的月份字串並對應成春夏秋冬
input = input_file
for( i in c(1 : length(input$Date))){
  word = input$Date[i]
  if(as.integer(substr(word,6,6))>= 3 & as.integer(substr(word,6,6))<= 5){
    input[i,13] = "spring"
  }else if(as.integer(substr(word,6,6))>= 6 & as.integer(substr(word,6,6))<= 8){
    input[i,13] = "summer"
  }else if(as.integer(substr(word,6,6))>= 9 & as.integer(substr(word,6,6))<= 11){
    input[i,13] = "autumn"
  }else{
    input[i,13] = "winter"
  }

}
colnames(input)[13] = "season"


#篩選資料，可能因素
data = input %>% mutate(Season=recode(season, 
                                 "spring" = 1,
                                 "summer" = 2,
                                 "autumn" = 3,
                                 "winter" = 4)) %>%
                mutate(ModuleNew=recode(Module, 
                          "MM60-6RT-300" = 300,
                          "SEC-6M-60A-295" = 295,
                          "AUO PM060MW3 320W" = 320,
                          "AUO PM060MW3 325W" = 325)) %>%
        select(Generation, Irradiance, Capacity, Irradiance_m, Temp, ModuleNew,Season,Lat)
data


##資料補值
#mice補值
md.pattern(data)
imputedData <- mice(data, m=5, maxit = 50, method = 'pmm', seed = 500)
fit=with(imputedData,lm(Generation ~ Irradiance + Temp))
summary(fit)
pooled=pool(fit)
completeData <- mice::complete(imputedData,action=4)
#2:318.81325
#4:315.03479


##拆解數據
library(caret)
trains = createDataPartition(
  y = completeData$Generation,
  p = 0.85,
  list = F,
  times = 1
)

trains2 = sample(trains, nrow(completeData)*0.7)
valids = setdiff(trains, trains2)

train = completeData[trains2,]
valids = completeData[valids,]
test = completeData[-trains,]
set.seed(123)


#將資料轉成xgboost規定ㄉ格式
colnames(completeData)
dvfunc <- dummyVars(~. , data = train[,2:8], fullRank = TRUE)
data_trainx = predict(dvfunc, newdata = train[,2:8])
data_trainy = train$Generation

data_validx = predict(dvfunc, newdata = valids[,2:8])
data_validy = valids$Generation

data_testx = predict(dvfunc, newdata = test[,2:8])
data_testy = test$Generation

dtrain <- xgb.DMatrix(data = data_trainx,
                      label = data_trainy)
dvalid <- xgb.DMatrix(data = data_validx,
                      label = data_validy)
dtest <- xgb.DMatrix(data = data_testx,
                      label = data_testy)


##設定參數
watch_list = list(train=dtrain, test = dvalid)
# train GBM model
# set parameters
param <-list(objective = "reg:linear",
             booster = "gbtree",
             eta = 0.001, #default = 0.3
             gamma=0,
             max_depth=3, #default=6
             min_child_weight=4, #default=1  #設值設定為1。您需要在子樹中指定最小的（海塞）例項權重的和，然後這個構建過程將放棄進一步的分割。線上性迴歸模式中，在每個節點最少所需例項數量將簡單的同時部署。更大,更保守的演算法。引數範圍是0到∞。
             subsample=1,#隨機蒐集資料防止數過擬和
             colsample_bytree=1#預設值設定為1。在構建每棵樹時,您需要指定列的子樣品比。範圍是0到1。
)




###訓練###
# Fitting XGBoost to the Training set
set.seed(1)
xgboost_model <- xgb.train(params = param, data = dtrain, nrounds = 6000,
                           watchlist = list(train = dtrain, val = dvalid),
                           eval_metric = "rmse",
                           print_every_n = 50, early_stopping_rounds = 500
)



###預測###
y_pred <- predict(xgboost_model, dtest)



###rmse計算###
error <- y_pred - data_testy
rmse_in <- sqrt(mean(error^2)) ## in-sample RMSE
rmse_in


#####評估篩選出來ㄉ結果#####
###Feature select###
#define final model
model_xgboost = xgboost(data = dtrain, max.depth = 3, nrounds = 10000, verbose = 0)
summary(model_xgboost)

# Compute feature importance matrix
importance_matrix = xgb.importance(colnames(dtrain), model = xgboost_model)
importance_matrix

# Nice graph
xgb.plot.importance(importance_matrix[1:7,])












#####test.csv!!!!!!!!!!!!!!!!!!!!!#####
test_file = read.csv("test.csv",header = T,sep=",")
test_submissionfile = read.csv("submission.csv",header = T,sep=",")

###資資料前處理###
for( i in c(1 : length(test_file$Date))){
  word = test_file$Date[i]
  if(as.integer(substr(word,6,6))>= 3 & as.integer(substr(word,6,6))<= 5){
    test_file[i,13] = "spring"
  }else if(as.integer(substr(word,6,6))>= 6 & as.integer(substr(word,6,6))<= 8){
    test_file[i,13] = "summer"
  }else if(as.integer(substr(word,6,6))>= 9 & as.integer(substr(word,6,6))<= 11){
    test_file[i,13] = "autumn"
  }else{
    test_file[i,13] = "winter"
  }
  
}
colnames(test_file)[13] = "season"


###篩選資料###
test_data = test_file %>% mutate(Season=recode(season, 
                                               "spring" = 1,
                                               "summer" = 2,
                                               "autumn" = 3,
                                               "winter" = 4)) %>%
                                 mutate(ModuleNew=recode(Module, 
                                                  "MM60-6RT-300" = 300,
                                                  "SEC-6M-60A-295" = 295,
                                                  "AUO PM060MW3 320W" = 320,
                                                  "AUO PM060MW3 325W" = 325)) %>%
  select(Irradiance, Capacity, Irradiance_m, Temp, ModuleNew,Season,Lat)


testDataAns = test_data


##料補值
test_imputedData <- mice(testDataAns, m=5, 
                         maxit = 50, method = 
                           'pmm', seed = 500)
test_completeData <- mice::complete(test_imputedData,2)

sum(is.na(test_completeData))


##資料改成xgboost規定的形式
dtestdata <- xgb.DMatrix(data = as.matrix(test_completeData))
##預測結果
test_submissionfile$Generation <- predict(xgboost_model, newdata=dtestdata, type="response")
write.csv(test_submissionfile,file="xgboost_submission1917119_returnRun_Lat.csv",row.names = FALSE)


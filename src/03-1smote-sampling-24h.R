
source(file="./src/01data-load.R")
source(file="./src/10function-collection.R")

library(lightgbm)
library(Matrix)
library(MLmetrics)
library(caret)
library(DMwR)


############################################################ 24h ##################################################################

############################################################ 1-1 ###################################################################
train1_loc1 %>% head


select_data = train1_loc1
select_data %>% head

# plant_num == 1 인경우 실행

select_data$month = month(select_data$plant1_train.mea_ddhr)
select_data$day = day(select_data$plant1_train.mea_ddhr)
select_data$hour = hour(select_data$plant1_train.mea_ddhr)
select_data$minute = minute(select_data$plant1_train.mea_ddhr)
select_data$id = NA

select_data$plant1_train.mea_ddhr = NULL




# loc_num = 1 인 경우 실행  
select_data$after48_loc1 = NULL


select_data$plant_test.planet = NULL
select_data$plant_test.loc = NULL
select_data$after24h = NULL
select_data$after48h = NULL
select_data$after48_loc1 = NULL

select_data$id = 1:nrow(select_data)




# 성능 검증

set.seed(601)

parts = createDataPartition(select_data$after24_loc1, p = .8)
data.train = select_data[parts$Resample1, ]
data.test = select_data[-parts$Resample1, ]

sol_key = data.test$after24_loc1
test_ids = data.test$id

data.test$id = NULL
data.test$after24_loc1 = NA



# create LGB dataset
varnames = setdiff(colnames(data.test), c("after24_loc1", "id"))

data.train$id = NULL

data.train$after24_loc1 = as.factor(data.train$after24_loc1)

## 5:5 샘플링 - 20000, 100
## 10:1 샘플링 - 5000, 1000
## 5:1 샘플링 - 5000, 500
x = SMOTE(after24_loc1 ~., data.train, perc.over = 5000, perc.under = 1000)
table(x$after24_loc1)


x$after24_loc1 = as.integer(as.character(x$after24_loc1))


data = rbind(x, data.test) %>% as.data.table()
#data = rbind(data.train, data.test) %>% as.data.table()

data$after24_loc1 = as.integer(data$after24_loc1)
#data = rbind(data.train, data.test) %>% as.data.table()


train_sparse = Matrix(as.matrix(data[!is.na(after24_loc1), varnames, with = F]), sparse = TRUE)
test_sparse = Matrix(as.matrix(data[is.na(after24_loc1), varnames, with = F]), sparse = TRUE)

y_train = data[!is.na(after24_loc1), after24_loc1]
#test_ids = data[is.na(target), id]

lgb.train = lgb.Dataset(data = train_sparse, label = y_train)

lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.05, num_leaves = 45,
                      num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                      eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)

best.iter = lgb.model.cv$best_iter
print(best.iter)

# train final model

lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.05,
                      num_leaves = 45, num_threads = 10, nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini)


## learning_rate 0.02 -> 0.03 변경시 성능 개선됨.
## learning_rate 0.03 -> 0.05 변경시 성능 개선됨.
## lr = 0.05 일때 가장 좋은 성능

## num_leaves 25 -> 35 변경시 성능 개선됨.(특이도 0.39 -> 0.41)
## num_leaves 35 -> 50 변경시 성능 개선됨.(특이도 0.41 -> 0.42)
## num_leaves 50 -> 45 변경시 성능 개선됨.(특이도 0.42 -> 0.43)

tree.imp = lgb.importance(lgb.model, percentage = T)
lgb.plot.importance(tree.imp, top_n = 15L, measure = "Gain")



X24H_COND_LOC_PROB = predict(lgb.model, test_sparse)
preds = data.table(id = data.test$id, X24H_COND_LOC = ifelse(X24H_COND_LOC_PROB >= 0.5, 1, 0), X24H_COND_LOC_PROB = X24H_COND_LOC_PROB)

preds$real_value = sol_key


preds$real_value = as.factor(preds$real_value)
preds$X24H_COND_LOC = as.factor(preds$X24H_COND_LOC)

confusionMatrix(preds$real_value, preds$X24H_COND_LOC)






















############################################################ 1-2 ###################################################################

train1_loc2 %>% head


select_data = train1_loc2
select_data %>% head

# plant_num == 1 인경우 실행

select_data$month = month(select_data$plant1_train.mea_ddhr)
select_data$day = day(select_data$plant1_train.mea_ddhr)
select_data$hour = hour(select_data$plant1_train.mea_ddhr)
select_data$minute = minute(select_data$plant1_train.mea_ddhr)
select_data$id = NA

select_data$plant1_train.mea_ddhr = NULL






# loc_num = 2 인 경우 실행
select_data$after48_loc2 = NULL


select_data$plant_test.planet = NULL
select_data$plant_test.loc = NULL
select_data$after24h = NULL
select_data$after48h = NULL
select_data$after48_loc1 = NULL

select_data$id = 1:nrow(select_data)





# 성능 검증

set.seed(601)

parts = createDataPartition(select_data$after24_loc2, p = .8)
data.train = select_data[parts$Resample1, ]
data.test = select_data[-parts$Resample1, ]

sol_key = data.test$after24_loc2
test_ids = data.test$id

data.test$id = NULL
data.test$after24_loc2 = NA



# create LGB dataset
varnames = setdiff(colnames(data.test), c("after24_loc2", "id"))

data.train$id = NULL

data.train$after24_loc2 = as.factor(data.train$after24_loc2)

## 5:5 샘플링 - 20000, 100
## 10:1 샘플링 - 5000, 1000
## 5:1 샘플링 - 5000, 500
x = SMOTE(after24_loc2 ~., data.train, perc.over = 5000, perc.under = 1000)
table(x$after24_loc2)


x$after24_loc2 = as.integer(as.character(x$after24_loc2))

data = rbind(x, data.test) %>% as.data.table()
#data = rbind(data.train, data.test) %>% as.data.table()



data$after24_loc2 = as.integer(data$after24_loc2)
#data = rbind(data.train, data.test) %>% as.data.table()


train_sparse = Matrix(as.matrix(data[!is.na(after24_loc2), varnames, with = F]), sparse = TRUE)
test_sparse = Matrix(as.matrix(data[is.na(after24_loc2), varnames, with = F]), sparse = TRUE)

y_train = data[!is.na(after24_loc2), after24_loc2]
#test_ids = data[is.na(target), id]

lgb.train = lgb.Dataset(data = train_sparse, label = y_train)

lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.03, num_leaves = 200,
                      num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                      eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)

best.iter = lgb.model.cv$best_iter
print(best.iter)

# train final model

lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.03,
                      num_leaves = 200, num_threads = 10, nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini)


## learning_rate 0.05 -> 0.03 변경시 성능 개선됨. (특이도 0.47 -> 0.5)
## learning_rate 0.03 -> 0.02 변경시 성능 떨어짐. (특이도 0.5 -> 0.48)
## learning_rate 0.02 -> 0.035 변경시 성능 떨어짐. (특이도 0.48 -> 0.47)
## learning_rate 0.02 -> 0.025 변경시 성능 떨어짐. (특이도 0.47 -> 0.46)
## lr = 0.03 일때 가장 좋은 성능

## num_leaves 45 -> 25 변경시 성능 개선됨.(특이도 0.5 -> 0.40)
## num_leaves 25 -> 35 변경시 성능 개선됨.(특이도 0.40 -> 0.45)
## num_leaves 35 -> 55 변경시 성능 개선됨.(특이도 0.45 -> 0.51)
## num_leaves 55 -> 60 변경시 성능 개선됨.(특이도 0.51 -> 0.51)
## num_leaves 60 -> 100 변경시 성능 개선됨.(특이도 0.51 -> 0.529)
## num_leaves 100 -> 150 변경시 성능 개선됨.(특이도 0.529 -> 0.530)
## num_leaves 150 -> 200 변경시 성능 개선됨.(특이도 0.529 -> 0.546)
## num_leaves 200 -> 300 변경시 성능 동일.(특이도 0.546 -> 0.546)
## num_leaves 300 -> 500 변경시 성능 동일.(특이도 0.546 -> 0.546)

## num_leaves 200 일때 가장 좋은 성능





#tree.imp = lgb.importance(lgb.model, percentage = T)
#lgb.plot.importance(tree.imp, top_n = 15L, measure = "Gain")



X24H_COND_LOC_PROB = predict(lgb.model, test_sparse)
preds = data.table(id = data.test$id, X24H_COND_LOC = ifelse(X24H_COND_LOC_PROB >= 0.5, 1, 0), X24H_COND_LOC_PROB = X24H_COND_LOC_PROB)

preds$real_value = sol_key

preds

preds$real_value = as.factor(preds$real_value)
preds$X24H_COND_LOC = as.factor(preds$X24H_COND_LOC)

confusionMatrix(preds$real_value, preds$X24H_COND_LOC)













############################################################ 1-3 ###################################################################






select_data = train1_loc3

# plant_num == 1 인경우 실행

select_data$month = month(select_data$plant1_train.mea_ddhr)
select_data$day = day(select_data$plant1_train.mea_ddhr)
select_data$hour = hour(select_data$plant1_train.mea_ddhr)
select_data$minute = minute(select_data$plant1_train.mea_ddhr)
select_data$id = NA

select_data$plant1_train.mea_ddhr = NULL




# loc_num = 3 인 경우 실행 
select_data$after48_loc3 = NULL



select_data$plant_test.planet = NULL
select_data$plant_test.loc = NULL
select_data$after24h = NULL
select_data$after48h = NULL
select_data$after48_loc3 = NULL

select_data$id = 1:nrow(select_data)





# 성능 검증

set.seed(601)

parts = createDataPartition(select_data$after24_loc3, p = .8)
data.train = select_data[parts$Resample1, ]
data.test = select_data[-parts$Resample1, ]

sol_key = data.test$after24_loc3
test_ids = data.test$id

data.test$id = NULL
data.test$after24_loc3 = NA



# create LGB dataset
varnames = setdiff(colnames(data.test), c("after24_loc3", "id"))

data.train$id = NULL

data.train$after24_loc3 = as.factor(data.train$after24_loc3)

## 5:5 샘플링 - 20000, 100
## 10:1 샘플링 - 5000, 1000
## 5:1 샘플링 - 5000, 500
x = SMOTE(after24_loc3 ~., data.train, perc.over = 5000, perc.under = 1000)
table(x$after24_loc3)


x$after24_loc3 = as.integer(as.character(x$after24_loc3))

data = rbind(x, data.test) %>% as.data.table()
#data = rbind(data.train, data.test) %>% as.data.table()


str(data)

data$after24_loc3 = as.integer(data$after24_loc3)
#data = rbind(data.train, data.test) %>% as.data.table()


train_sparse = Matrix(as.matrix(data[!is.na(after24_loc3), varnames, with = F]), sparse = TRUE)
test_sparse = Matrix(as.matrix(data[is.na(after24_loc3), varnames, with = F]), sparse = TRUE)

y_train = data[!is.na(after24_loc3), after24_loc3]
#test_ids = data[is.na(target), id]

lgb.train = lgb.Dataset(data = train_sparse, label = y_train)

lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.01, num_leaves = 300,
                      num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                      eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)

best.iter = lgb.model.cv$best_iter
print(best.iter)

# train final model

lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.01,
                      num_leaves = 300, num_threads = 10, nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini)



## 0.05, 45 (특이도 0.4974)
## learning_rate 0.05 -> 0.03 변경시 성능 개선됨. (특이도 0.47)
## learning_rate 0.03 -> 0.02 변경시 성능 개선됨. (특이도 0.49)
## learning_rate 0.02 -> 0.01 변경시 성능 떨어짐. (특이도 0.50)
## learning_rate 0.01 -> 0.07 변경시 성능 떨어짐. (특이도 0.47)
## lr = 0.01 일때 가장 좋은 성능

## num_leaves 45 -> 100 변경시 성능 개선됨.(특이도 0.5854)
## num_leaves 100 -> 200 변경시 성능 개선됨.(특이도 0.5901)
## num_leaves 200 -> 300 변경시 성능 개선됨.(특이도 0.5926)

## num_leaves 300 일때 가장 좋은 성능





#tree.imp = lgb.importance(lgb.model, percentage = T)
#lgb.plot.importance(tree.imp, top_n = 15L, measure = "Gain")



X24H_COND_LOC_PROB = predict(lgb.model, test_sparse)
preds = data.table(id = data.test$id, X24H_COND_LOC = ifelse(X24H_COND_LOC_PROB >= 0.5, 1, 0), X24H_COND_LOC_PROB = X24H_COND_LOC_PROB)

preds$real_value = sol_key

preds

preds$real_value = as.factor(preds$real_value)
preds$X24H_COND_LOC = as.factor(preds$X24H_COND_LOC)

confusionMatrix(preds$real_value, preds$X24H_COND_LOC)













############################################################ 2-1 ###################################################################




select_data = train2_loc1




# plant_num == 2 인경우 실행
select_data$month = month(select_data$plant2_train.mea_ddhr)
select_data$day = day(select_data$plant2_train.mea_ddhr)
select_data$hour = hour(select_data$plant2_train.mea_ddhr)
select_data$minute = minute(select_data$plant2_train.mea_ddhr)
select_data$id = NA

select_data$plant2_train.mea_ddhr = NULL


# loc_num = 1 인 경우 실행  
select_data$after48_loc1 = NULL


select_data$plant_test.planet = NULL
select_data$plant_test.loc = NULL
select_data$after24h = NULL
select_data$after48h = NULL
select_data$after48_loc3 = NULL

select_data %>% head
select_data$id = 1:nrow(select_data)





# 성능 검증

set.seed(601)

parts = createDataPartition(select_data$after24_loc1, p = .8)
data.train = select_data[parts$Resample1, ]
data.test = select_data[-parts$Resample1, ]

sol_key = data.test$after24_loc1
test_ids = data.test$id

data.test$id = NULL
data.test$after24_loc1 = NA



# create LGB dataset
varnames = setdiff(colnames(data.test), c("after24_loc1", "id"))

data.train$id = NULL

data.train$after24_loc1 = as.factor(data.train$after24_loc1)

## 5:5 샘플링 - 20000, 100
## 10:1 샘플링 - 5000, 1000
## 5:1 샘플링 - 5000, 500
x = SMOTE(after24_loc1 ~., data.train, perc.over = 5000, perc.under = 1000)
table(x$after24_loc1)


x$after24_loc1 = as.integer(as.character(x$after24_loc1))

data = rbind(x, data.test) %>% as.data.table()
#data = rbind(data.train, data.test) %>% as.data.table()


data$after24_loc1 = as.integer(data$after24_loc1)
#data = rbind(data.train, data.test) %>% as.data.table()


train_sparse = Matrix(as.matrix(data[!is.na(after24_loc1), varnames, with = F]), sparse = TRUE)
test_sparse = Matrix(as.matrix(data[is.na(after24_loc1), varnames, with = F]), sparse = TRUE)

y_train = data[!is.na(after24_loc1), after24_loc1]
#test_ids = data[is.na(target), id]

lgb.train = lgb.Dataset(data = train_sparse, label = y_train)

lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.01, num_leaves = 200,
                      num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                      eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)

best.iter = lgb.model.cv$best_iter
print(best.iter)

# train final model

lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.01,
                      num_leaves = 200, num_threads = 10, nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini)



## 0.05, 45 (특이도 0.4248)
## learning_rate 0.05 -> 0.03 변경시 성능 개선됨. (특이도 0.42)
## learning_rate 0.03 -> 0.02 변경시 성능 개선됨. (특이도 0.42)
## learning_rate 0.02 -> 0.01 변경시 성능 떨어짐. (특이도 0.42)
## lr = 0.01 일때 가장 좋은 성능

## num_leaves 45 -> 100 변경시 성능 개선됨.(특이도 0.435)
## num_leaves 100 -> 200 변경시 성능 개선됨.(특이도 0.439)

## num_leaves 200 일때 가장 좋은 성능





#tree.imp = lgb.importance(lgb.model, percentage = T)
#lgb.plot.importance(tree.imp, top_n = 15L, measure = "Gain")



X24H_COND_LOC_PROB = predict(lgb.model, test_sparse)
preds = data.table(id = data.test$id, X24H_COND_LOC = ifelse(X24H_COND_LOC_PROB >= 0.5, 1, 0), X24H_COND_LOC_PROB = X24H_COND_LOC_PROB)

preds$real_value = sol_key

preds$real_value = as.factor(preds$real_value)
preds$X24H_COND_LOC = as.factor(preds$X24H_COND_LOC)

confusionMatrix(preds$real_value, preds$X24H_COND_LOC)







############################################################ 2-2 ###################################################################






select_data = train2_loc2


# plant_num == 2 인경우 실행
select_data$month = month(select_data$plant2_train.mea_ddhr)
select_data$day = day(select_data$plant2_train.mea_ddhr)
select_data$hour = hour(select_data$plant2_train.mea_ddhr)
select_data$minute = minute(select_data$plant2_train.mea_ddhr)
select_data$id = NA

select_data$plant2_train.mea_ddhr = NULL


select_data %>% head


# loc_num = 2 인 경우 실행
select_data$after48_loc2 = NULL




select_data$plant_test.planet = NULL
select_data$plant_test.loc = NULL
select_data$after24h = NULL
select_data$after48h = NULL
select_data$after48_loc3 = NULL

select_data$id = 1:nrow(select_data)






# 성능 검증

set.seed(601)

parts = createDataPartition(select_data$after24_loc2, p = .8)
data.train = select_data[parts$Resample1, ]
data.test = select_data[-parts$Resample1, ]

sol_key = data.test$after24_loc2
test_ids = data.test$id

data.test$id = NULL
data.test$after24_loc2 = NA



# create LGB dataset
varnames = setdiff(colnames(data.test), c("after24_loc2", "id"))

data.train$id = NULL

data.train$after24_loc2 = as.factor(data.train$after24_loc2)

## 5:5 샘플링 - 20000, 100
## 10:1 샘플링 - 5000, 1000
## 5:1 샘플링 - 5000, 500
x = SMOTE(after24_loc2 ~., data.train, perc.over = 5000, perc.under = 1000)
table(x$after24_loc2)


x$after24_loc2 = as.integer(as.character(x$after24_loc2))

data = rbind(x, data.test) %>% as.data.table()
#data = rbind(data.train, data.test) %>% as.data.table()


data$after24_loc2 = as.integer(data$after24_loc2)
#data = rbind(data.train, data.test) %>% as.data.table()


train_sparse = Matrix(as.matrix(data[!is.na(after24_loc2), varnames, with = F]), sparse = TRUE)
test_sparse = Matrix(as.matrix(data[is.na(after24_loc2), varnames, with = F]), sparse = TRUE)

y_train = data[!is.na(after24_loc2), after24_loc2]
#test_ids = data[is.na(target), id]

lgb.train = lgb.Dataset(data = train_sparse, label = y_train)

lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.01, num_leaves = 45,
                      num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                      eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)

best.iter = lgb.model.cv$best_iter
print(best.iter)

# train final model

lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.01,
                      num_leaves = 45, num_threads = 10, nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini)



## 0.05, 45 (특이도 0.4167)
## learning_rate 0.05 -> 0.03 변경시 성능 개선됨. (특이도 0.40)
## learning_rate 0.03 -> 0.01 변경시 성능 떨어짐. (특이도 0.436)
## learning_rate 0.01 -> 0.07 변경시 성능 떨어짐. (특이도 0.398)
## learning_rate 0.07 -> 0.005 변경시 성능 떨어짐. (특이도 0.4286)
## lr = 0.01 일때 가장 좋은 성능

## num_leaves 45 -> 100 변경시 성능 개선됨.(특이도 0.436)
## num_leaves 100 -> 200 변경시 성능 개선됨.(특이도 0.436)
## num_leaves 200 -> 25 변경시 성능 개선됨.(특이도 0.3719)
## num_leaves 25 -> 40 변경시 성능 개선됨.(특이도 0.4206)

## num_leaves 45 일때 가장 좋은 성능





#tree.imp = lgb.importance(lgb.model, percentage = T)
#lgb.plot.importance(tree.imp, top_n = 15L, measure = "Gain")



X24H_COND_LOC_PROB = predict(lgb.model, test_sparse)
preds = data.table(id = data.test$id, X24H_COND_LOC = ifelse(X24H_COND_LOC_PROB >= 0.5, 1, 0), X24H_COND_LOC_PROB = X24H_COND_LOC_PROB)

preds$real_value = sol_key

preds$real_value = as.factor(preds$real_value)
preds$X24H_COND_LOC = as.factor(preds$X24H_COND_LOC)

confusionMatrix(preds$real_value, preds$X24H_COND_LOC)















############################################################ 2-3 ###################################################################




select_data = train2_loc3



# plant_num == 2 인경우 실행
select_data$month = month(select_data$plant2_train.mea_ddhr)
select_data$day = day(select_data$plant2_train.mea_ddhr)
select_data$hour = hour(select_data$plant2_train.mea_ddhr)
select_data$minute = minute(select_data$plant2_train.mea_ddhr)
select_data$id = NA

select_data$plant2_train.mea_ddhr = NULL



# loc_num = 3 인 경우 실행 
select_data$after48_loc3 = NULL



select_data$plant_test.planet = NULL
select_data$plant_test.loc = NULL
select_data$after24h = NULL
select_data$after48h = NULL
select_data$after48_loc3 = NULL

select_data %>% head
select_data$id = 1:nrow(select_data)





# 성능 검증

set.seed(601)

parts = createDataPartition(select_data$after24_loc3, p = .8)
data.train = select_data[parts$Resample1, ]
data.test = select_data[-parts$Resample1, ]

sol_key = data.test$after24_loc3
test_ids = data.test$id

data.test$id = NULL
data.test$after24_loc3 = NA



# create LGB dataset
varnames = setdiff(colnames(data.test), c("after24_loc3", "id"))

data.train$id = NULL

data.train$after24_loc3 = as.factor(data.train$after24_loc3)

## 5:5 샘플링 - 20000, 100
## 10:1 샘플링 - 5000, 1000
## 5:1 샘플링 - 5000, 500
x = SMOTE(after24_loc3 ~., data.train, perc.over = 100000, perc.under = 5000)
table(x$after24_loc3)


x$after24_loc3 = as.integer(as.character(x$after24_loc3))

data = rbind(x, data.test) %>% as.data.table()
#data = rbind(data.train, data.test) %>% as.data.table()



data$after24_loc3 = as.integer(data$after24_loc3)
#data = rbind(data.train, data.test) %>% as.data.table()


train_sparse = Matrix(as.matrix(data[!is.na(after24_loc3), varnames, with = F]), sparse = TRUE)
test_sparse = Matrix(as.matrix(data[is.na(after24_loc3), varnames, with = F]), sparse = TRUE)

y_train = data[!is.na(after24_loc3), after24_loc3]
#test_ids = data[is.na(target), id]

lgb.train = lgb.Dataset(data = train_sparse, label = y_train)

lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.03, num_leaves = 50,
                      num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                      eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)

best.iter = lgb.model.cv$best_iter
print(best.iter)

# train final model

lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.03,
                      num_leaves = 50, num_threads = 10, nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini)



## 0.05, 45 (특이도 0.009) - smote 재조정 전.
## 0.05, 45 (특이도 0.03) - smote 재조정 35000 / 1000.
## 0.05, 45 (특이도 0.1) - smote 재조정 50000 / 2500.
## 0.05, 45 (특이도 0.1429) - smote 재조정 70000 / 3500.
## 0.05, 45 (특이도 0.1429) - smote 재조정 100000 / 5000.


## learning_rate 0.05 -> 0.05 변경시 성능 개선됨. (특이도 0.14)
## learning_rate 0.03 -> 0.01 변경시 성능 개선됨. (특이도 0.07)
## learning_rate 0.01 -> 0.03 변경시 성능 떨어짐. (특이도 0.2)
## learning_rate 0.03 -> 0.07 변경시 성능 떨어짐. (특이도 0.16)
## lr = 0.03 일때 가장 좋은 성능

## num_leaves 45 -> 100 변경시 성능 개선됨.(특이도 0.2)
## num_leaves 100 -> 300 변경시 성능 개선됨.(특이도 0.2)
## num_leaves 100 -> 50 변경시 성능 개선됨.(특이도 0.2)

## num_leaves 50 일때 가장 좋은 성능



#tree.imp = lgb.importance(lgb.model, percentage = T)
#lgb.plot.importance(tree.imp, top_n = 15L, measure = "Gain")



X24H_COND_LOC_PROB = predict(lgb.model, test_sparse)
preds = data.table(id = data.test$id, X24H_COND_LOC = ifelse(X24H_COND_LOC_PROB >= 0.5, 1, 0), X24H_COND_LOC_PROB = X24H_COND_LOC_PROB)

preds$real_value = sol_key

preds$real_value = as.factor(preds$real_value)
preds$X24H_COND_LOC = as.factor(preds$X24H_COND_LOC)

confusionMatrix(preds$real_value, preds$X24H_COND_LOC)









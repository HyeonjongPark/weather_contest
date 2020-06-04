

source(file="./src/01data-load.R")
source(file="./src/10function-collection.R")

library(lightgbm)
library(Matrix)
library(MLmetrics)
library(caret)
library(DMwR)




############################################################ 48h ##################################################################

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
select_data$after24_loc1 = NULL


select_data$plant_test.planet = NULL
select_data$plant_test.loc = NULL
select_data$after24h = NULL
select_data$after48h = NULL

select_data$id = 1:nrow(select_data)




# 성능 검증

set.seed(601)

parts = createDataPartition(select_data$after48_loc1, p = .8)
data.train = select_data[parts$Resample1, ]
data.test = select_data[-parts$Resample1, ]

sol_key = data.test$after48_loc1
test_ids = data.test$id

data.test$id = NULL
data.test$after48_loc1 = NA



# create LGB dataset
varnames = setdiff(colnames(data.test), c("after48_loc1", "id"))

data.train$id = NULL

data.train$after48_loc1 = as.factor(data.train$after48_loc1)

## 5:5 샘플링 - 20000, 100
## 10:1 샘플링 - 5000, 1000
## 5:1 샘플링 - 5000, 500
x = SMOTE(after48_loc1 ~., data.train, perc.over = 5000, perc.under = 1000)
table(x$after48_loc1)


x$after48_loc1 = as.integer(as.character(x$after48_loc1))


data = rbind(x, data.test) %>% as.data.table()
#data = rbind(data.train, data.test) %>% as.data.table()

data$after48_loc1 = as.integer(data$after48_loc1)
#data = rbind(data.train, data.test) %>% as.data.table()


train_sparse = Matrix(as.matrix(data[!is.na(after48_loc1), varnames, with = F]), sparse = TRUE)
test_sparse = Matrix(as.matrix(data[is.na(after48_loc1), varnames, with = F]), sparse = TRUE)

y_train = data[!is.na(after48_loc1), after48_loc1]
#test_ids = data[is.na(target), id]

lgb.train = lgb.Dataset(data = train_sparse, label = y_train)

lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.01, num_leaves = 60,
                      num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                      eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)

best.iter = lgb.model.cv$best_iter
print(best.iter)

# train final model

lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.01,
                      num_leaves = 60, num_threads = 10, nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini)



## 0.05, 45 (특이도 0.40)
## learning_rate 0.05 -> 0.03 변경시 성능 개선됨. (특이도 0.38)
## learning_rate 0.03 -> 0.01 변경시 성능 개선됨. (특이도 0.4144)
## learning_rate 0.01 -> 0.08 변경시 성능 개선됨. (특이도 0.4138)
## learning_rate 0.01 -> 0.1 변경시 성능 개선됨. (특이도 0.38)
## learning_rate 0.01 -> 0.008 변경시 성능 개선됨. (특이도 0.4144)

## lr = 0.01 일때 가장 좋은 성능

## num_leaves 45 -> 40 변경시 성능 개선됨.(특이도 0.4)
## num_leaves 40 -> 60 변경시 성능 개선됨.(특이도 0.4182)

## num_leaves 60 일때 가장 좋은 성능


#tree.imp = lgb.importance(lgb.model, percentage = T)
#lgb.plot.importance(tree.imp, top_n = 15L, measure = "Gain")



X48H_COND_LOC_PROB = predict(lgb.model, test_sparse)
preds = data.table(id = data.test$id, X48H_COND_LOC = ifelse(X48H_COND_LOC_PROB >= 0.5, 1, 0), X48H_COND_LOC_PROB = X48H_COND_LOC_PROB)

preds$real_value = sol_key


preds$real_value = as.factor(preds$real_value)
preds$X48H_COND_LOC = as.factor(preds$X48H_COND_LOC)

confusionMatrix(preds$real_value, preds$X48H_COND_LOC)






















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
select_data$after24_loc2 = NULL


select_data$plant_test.planet = NULL
select_data$plant_test.loc = NULL
select_data$after24h = NULL
select_data$after48h = NULL

select_data$id = 1:nrow(select_data)





# 성능 검증

set.seed(601)

parts = createDataPartition(select_data$after48_loc2, p = .8)
data.train = select_data[parts$Resample1, ]
data.test = select_data[-parts$Resample1, ]

sol_key = data.test$after48_loc2
test_ids = data.test$id

data.test$id = NULL
data.test$after48_loc2 = NA



# create LGB dataset
varnames = setdiff(colnames(data.test), c("after48_loc2", "id"))

data.train$id = NULL

data.train$after48_loc2 = as.factor(data.train$after48_loc2)

## 5:5 샘플링 - 20000, 100
## 10:1 샘플링 - 5000, 1000
## 5:1 샘플링 - 5000, 500
x = SMOTE(after48_loc2 ~., data.train, perc.over = 5000, perc.under = 1000)
table(x$after48_loc2)


x$after48_loc2 = as.integer(as.character(x$after48_loc2))

data = rbind(x, data.test) %>% as.data.table()
#data = rbind(data.train, data.test) %>% as.data.table()



data$after48_loc2 = as.integer(data$after48_loc2)
#data = rbind(data.train, data.test) %>% as.data.table()


train_sparse = Matrix(as.matrix(data[!is.na(after48_loc2), varnames, with = F]), sparse = TRUE)
test_sparse = Matrix(as.matrix(data[is.na(after48_loc2), varnames, with = F]), sparse = TRUE)

y_train = data[!is.na(after48_loc2), after48_loc2]
#test_ids = data[is.na(target), id]

lgb.train = lgb.Dataset(data = train_sparse, label = y_train)

lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.01, num_leaves = 100,
                      num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                      eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)

best.iter = lgb.model.cv$best_iter
print(best.iter)

# train final model

lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.01,
                      num_leaves = 100, num_threads = 10, nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini)




## 0.015, 100 (특이도 0.6396)




#tree.imp = lgb.importance(lgb.model, percentage = T)
#lgb.plot.importance(tree.imp, top_n = 15L, measure = "Gain")



X48H_COND_LOC_PROB = predict(lgb.model, test_sparse)
preds = data.table(id = data.test$id, X48H_COND_LOC = ifelse(X48H_COND_LOC_PROB >= 0.5, 1, 0), X48H_COND_LOC_PROB = X48H_COND_LOC_PROB)

preds$real_value = sol_key

preds

preds$real_value = as.factor(preds$real_value)
preds$X48H_COND_LOC = as.factor(preds$X48H_COND_LOC)

confusionMatrix(preds$real_value, preds$X48H_COND_LOC)





## grid 서치.
list_df = list()
for(i in c(0.01, 0.03, 0.05, 0.07)) {
  for(j in c(25, 45, 100, 200)) {
    
    
    set.seed(601)
    
    parts = createDataPartition(select_data$after48_loc2, p = .8)
    data.train = select_data[parts$Resample1, ]
    data.test = select_data[-parts$Resample1, ]
    
    sol_key = data.test$after48_loc2
    test_ids = data.test$id
    
    data.test$id = NULL
    data.test$after48_loc2 = NA
    
    
    varnames = setdiff(colnames(data.test), c("after48_loc2", "id"))
    
    data.train$id = NULL
    
    data.train$after48_loc2 = as.factor(data.train$after48_loc2)
    
    
    x = SMOTE(after48_loc2 ~., data.train, perc.over = 5000, perc.under = 1000)
    table(x$after48_loc2)
    
    
    x$after48_loc2 = as.integer(as.character(x$after48_loc2))
    
    data = rbind(x, data.test) %>% as.data.table()
    
    
    
    data$after48_loc2 = as.integer(data$after48_loc2)
    
    
    train_sparse = Matrix(as.matrix(data[!is.na(after48_loc2), varnames, with = F]), sparse = TRUE)
    test_sparse = Matrix(as.matrix(data[is.na(after48_loc2), varnames, with = F]), sparse = TRUE)
    
    y_train = data[!is.na(after48_loc2), after48_loc2]
    
    lgb.train = lgb.Dataset(data = train_sparse, label = y_train)
    
    lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = i, num_leaves = j,
                          num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                          eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)
    
    best.iter = lgb.model.cv$best_iter
    print(best.iter)
    
    
    lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = i,
                          num_leaves = j, num_threads = 10, nrounds = best.iter,
                          eval_freq = 20, eval = lgb.normalizedgini)
    
    
    
    
    X48H_COND_LOC_PROB = predict(lgb.model, test_sparse)
    preds = data.table(id = data.test$id, X48H_COND_LOC = ifelse(X48H_COND_LOC_PROB >= 0.5, 1, 0), X48H_COND_LOC_PROB = X48H_COND_LOC_PROB)
    
    preds$real_value = sol_key
    
    
    preds$real_value = as.factor(preds$real_value)
    preds$X48H_COND_LOC = as.factor(preds$X48H_COND_LOC)
    
    
    
    list_df = append(list_df, confusionMatrix(preds$real_value, preds$X48H_COND_LOC))
    
    
  }
}

list_df




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
select_data$after24_loc3 = NULL



select_data$plant_test.planet = NULL
select_data$plant_test.loc = NULL
select_data$after24h = NULL
select_data$after48h = NULL

select_data$id = 1:nrow(select_data)





# 성능 검증

set.seed(601)

parts = createDataPartition(select_data$after48_loc3, p = .8)
data.train = select_data[parts$Resample1, ]
data.test = select_data[-parts$Resample1, ]

sol_key = data.test$after48_loc3
test_ids = data.test$id

data.test$id = NULL
data.test$after48_loc3 = NA



# create LGB dataset
varnames = setdiff(colnames(data.test), c("after48_loc3", "id"))

data.train$id = NULL

data.train$after48_loc3 = as.factor(data.train$after48_loc3)

## 5:5 샘플링 - 20000, 100
## 10:1 샘플링 - 5000, 1000
## 5:1 샘플링 - 5000, 500
x = SMOTE(after48_loc3 ~., data.train, perc.over = 5000, perc.under = 1000)
table(x$after48_loc3)


x$after48_loc3 = as.integer(as.character(x$after48_loc3))

data = rbind(x, data.test) %>% as.data.table()
#data = rbind(data.train, data.test) %>% as.data.table()



data$after48_loc3 = as.integer(data$after48_loc3)
#data = rbind(data.train, data.test) %>% as.data.table()


train_sparse = Matrix(as.matrix(data[!is.na(after48_loc3), varnames, with = F]), sparse = TRUE)
test_sparse = Matrix(as.matrix(data[is.na(after48_loc3), varnames, with = F]), sparse = TRUE)

y_train = data[!is.na(after48_loc3), after48_loc3]
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



## 0.01, 200(특이도 0.605)


#tree.imp = lgb.importance(lgb.model, percentage = T)
#lgb.plot.importance(tree.imp, top_n = 15L, measure = "Gain")



X48H_COND_LOC_PROB = predict(lgb.model, test_sparse)
preds = data.table(id = data.test$id, X48H_COND_LOC = ifelse(X48H_COND_LOC_PROB >= 0.5, 1, 0), X48H_COND_LOC_PROB = X48H_COND_LOC_PROB)

preds$real_value = sol_key

preds

preds$real_value = as.factor(preds$real_value)
preds$X48H_COND_LOC = as.factor(preds$X48H_COND_LOC)

confusionMatrix(preds$real_value, preds$X48H_COND_LOC)





## grid 서치.
list_df = list()
for(i in c(0.01, 0.03, 0.05, 0.07)) {
  for(j in c(25, 45, 100, 200)) {
    
    
    set.seed(601)
    
    parts = createDataPartition(select_data$after48_loc3, p = .8)
    data.train = select_data[parts$Resample1, ]
    data.test = select_data[-parts$Resample1, ]
    
    sol_key = data.test$after48_loc3
    test_ids = data.test$id
    
    data.test$id = NULL
    data.test$after48_loc3 = NA
    
    
    varnames = setdiff(colnames(data.test), c("after48_loc3", "id"))
    
    data.train$id = NULL
    
    data.train$after48_loc3 = as.factor(data.train$after48_loc3)
    
    
    x = SMOTE(after48_loc3 ~., data.train, perc.over = 5000, perc.under = 1000)
    table(x$after48_loc3)
    
    
    x$after48_loc3 = as.integer(as.character(x$after48_loc3))
    
    data = rbind(x, data.test) %>% as.data.table()
    
    
    
    data$after48_loc3 = as.integer(data$after48_loc3)
    
    
    train_sparse = Matrix(as.matrix(data[!is.na(after48_loc3), varnames, with = F]), sparse = TRUE)
    test_sparse = Matrix(as.matrix(data[is.na(after48_loc3), varnames, with = F]), sparse = TRUE)
    
    y_train = data[!is.na(after48_loc3), after48_loc3]
    
    lgb.train = lgb.Dataset(data = train_sparse, label = y_train)
    
    lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = i, num_leaves = j,
                          num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                          eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)
    
    best.iter = lgb.model.cv$best_iter
    print(best.iter)
    
    
    lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = i,
                          num_leaves = j, num_threads = 10, nrounds = best.iter,
                          eval_freq = 20, eval = lgb.normalizedgini)
    
    
    
    
    X48H_COND_LOC_PROB = predict(lgb.model, test_sparse)
    preds = data.table(id = data.test$id, X48H_COND_LOC = ifelse(X48H_COND_LOC_PROB >= 0.5, 1, 0), X48H_COND_LOC_PROB = X48H_COND_LOC_PROB)
    
    preds$real_value = sol_key
    
    
    preds$real_value = as.factor(preds$real_value)
    preds$X48H_COND_LOC = as.factor(preds$X48H_COND_LOC)
    
    
    
    list_df = append(list_df, confusionMatrix(preds$real_value, preds$X48H_COND_LOC))
    
    
  }
}

list_df










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
select_data$after24_loc1 = NULL


select_data$plant_test.planet = NULL
select_data$plant_test.loc = NULL
select_data$after24h = NULL
select_data$after48h = NULL

select_data %>% head
select_data$id = 1:nrow(select_data)





# 성능 검증

set.seed(601)

parts = createDataPartition(select_data$after48_loc1, p = .8)
data.train = select_data[parts$Resample1, ]
data.test = select_data[-parts$Resample1, ]

sol_key = data.test$after48_loc1
test_ids = data.test$id

data.test$id = NULL
data.test$after48_loc1 = NA



# create LGB dataset
varnames = setdiff(colnames(data.test), c("after48_loc1", "id"))

data.train$id = NULL

data.train$after48_loc1 = as.factor(data.train$after48_loc1)

## 5:5 샘플링 - 20000, 100
## 10:1 샘플링 - 5000, 1000
## 5:1 샘플링 - 5000, 500
x = SMOTE(after48_loc1 ~., data.train, perc.over = 10000, perc.under = 1000)
table(x$after48_loc1)


x$after48_loc1 = as.integer(as.character(x$after48_loc1))

data = rbind(x, data.test) %>% as.data.table()
#data = rbind(data.train, data.test) %>% as.data.table()


data$after48_loc1 = as.integer(data$after48_loc1)
#data = rbind(data.train, data.test) %>% as.data.table()


train_sparse = Matrix(as.matrix(data[!is.na(after48_loc1), varnames, with = F]), sparse = TRUE)
test_sparse = Matrix(as.matrix(data[is.na(after48_loc1), varnames, with = F]), sparse = TRUE)

y_train = data[!is.na(after48_loc1), after48_loc1]
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



## 0.01, 200 (특이도 0.5)





#tree.imp = lgb.importance(lgb.model, percentage = T)
#lgb.plot.importance(tree.imp, top_n = 15L, measure = "Gain")



X48H_COND_LOC_PROB = predict(lgb.model, test_sparse)
preds = data.table(id = data.test$id, X48H_COND_LOC = ifelse(X48H_COND_LOC_PROB >= 0.5, 1, 0), X48H_COND_LOC_PROB = X48H_COND_LOC_PROB)

preds$real_value = sol_key

preds$real_value = as.factor(preds$real_value)
preds$X48H_COND_LOC = as.factor(preds$X48H_COND_LOC)

confusionMatrix(preds$real_value, preds$X48H_COND_LOC)





## grid 서치.
list_df = list()
for(i in c(0.01, 0.03, 0.05, 0.07)) {
  for(j in c(25, 45, 100, 200)) {
    
    
    set.seed(601)
    
    parts = createDataPartition(select_data$after48_loc1, p = .8)
    data.train = select_data[parts$Resample1, ]
    data.test = select_data[-parts$Resample1, ]
    
    sol_key = data.test$after48_loc1
    test_ids = data.test$id
    
    data.test$id = NULL
    data.test$after48_loc1 = NA
    
    
    varnames = setdiff(colnames(data.test), c("after48_loc1", "id"))
    
    data.train$id = NULL
    
    data.train$after48_loc1 = as.factor(data.train$after48_loc1)
    
    
    x = SMOTE(after48_loc1 ~., data.train, perc.over = 5000, perc.under = 1000)
    table(x$after48_loc1)
    
    
    x$after48_loc1 = as.integer(as.character(x$after48_loc1))
    
    data = rbind(x, data.test) %>% as.data.table()
    
    
    
    data$after48_loc1 = as.integer(data$after48_loc1)
    
    
    train_sparse = Matrix(as.matrix(data[!is.na(after48_loc1), varnames, with = F]), sparse = TRUE)
    test_sparse = Matrix(as.matrix(data[is.na(after48_loc1), varnames, with = F]), sparse = TRUE)
    
    y_train = data[!is.na(after48_loc1), after48_loc1]
    
    lgb.train = lgb.Dataset(data = train_sparse, label = y_train)
    
    lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = i, num_leaves = j,
                          num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                          eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)
    
    best.iter = lgb.model.cv$best_iter
    print(best.iter)
    
    
    lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = i,
                          num_leaves = j, num_threads = 10, nrounds = best.iter,
                          eval_freq = 20, eval = lgb.normalizedgini)
    
    
    
    
    X48H_COND_LOC_PROB = predict(lgb.model, test_sparse)
    preds = data.table(id = data.test$id, X48H_COND_LOC = ifelse(X48H_COND_LOC_PROB >= 0.5, 1, 0), X48H_COND_LOC_PROB = X48H_COND_LOC_PROB)
    
    preds$real_value = sol_key
    
    
    preds$real_value = as.factor(preds$real_value)
    preds$X48H_COND_LOC = as.factor(preds$X48H_COND_LOC)
    
    
    
    list_df = append(list_df, confusionMatrix(preds$real_value, preds$X48H_COND_LOC))
    
    
  }
}

list_df

preds$X48H_COND_LOC %>% table
preds$real_value %>% table






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
select_data$after24_loc2 = NULL

select_data$plant_test.planet = NULL
select_data$plant_test.loc = NULL
select_data$after24h = NULL
select_data$after48h = NULL

select_data$id = 1:nrow(select_data)






# 성능 검증

set.seed(601)

parts = createDataPartition(select_data$after48_loc2, p = .8)
data.train = select_data[parts$Resample1, ]
data.test = select_data[-parts$Resample1, ]

sol_key = data.test$after48_loc2
test_ids = data.test$id

data.test$id = NULL
data.test$after48_loc2 = NA



# create LGB dataset
varnames = setdiff(colnames(data.test), c("after48_loc2", "id"))

data.train$id = NULL

data.train$after48_loc2 = as.factor(data.train$after48_loc2)

## 5:5 샘플링 - 20000, 100
## 10:1 샘플링 - 5000, 1000
## 5:1 샘플링 - 5000, 500
x = SMOTE(after48_loc2 ~., data.train, perc.over = 5000, perc.under = 1000)
table(x$after48_loc2)


x$after48_loc2 = as.integer(as.character(x$after48_loc2))

data = rbind(x, data.test) %>% as.data.table()
#data = rbind(data.train, data.test) %>% as.data.table()


data$after48_loc2 = as.integer(data$after48_loc2)
#data = rbind(data.train, data.test) %>% as.data.table()


train_sparse = Matrix(as.matrix(data[!is.na(after48_loc2), varnames, with = F]), sparse = TRUE)
test_sparse = Matrix(as.matrix(data[is.na(after48_loc2), varnames, with = F]), sparse = TRUE)

y_train = data[!is.na(after48_loc2), after48_loc2]
#test_ids = data[is.na(target), id]

lgb.train = lgb.Dataset(data = train_sparse, label = y_train)

lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.03, num_leaves = 100,
                      num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                      eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)

best.iter = lgb.model.cv$best_iter
print(best.iter)

# train final model

lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.03,
                      num_leaves = 100, num_threads = 10, nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini)



## 0.03, 100  (특이도 0.41)



#tree.imp = lgb.importance(lgb.model, percentage = T)
#lgb.plot.importance(tree.imp, top_n = 15L, measure = "Gain")



X48H_COND_LOC_PROB = predict(lgb.model, test_sparse)
preds = data.table(id = data.test$id, X48H_COND_LOC = ifelse(X48H_COND_LOC_PROB >= 0.5, 1, 0), X48H_COND_LOC_PROB = X48H_COND_LOC_PROB)

preds$real_value = sol_key

preds$real_value = as.factor(preds$real_value)
preds$X48H_COND_LOC = as.factor(preds$X48H_COND_LOC)

confusionMatrix(preds$real_value, preds$X48H_COND_LOC)







## grid 서치.
list_df = list()
for(i in c(0.01, 0.03, 0.05, 0.07)) {
  for(j in c(25, 45, 100, 200)) {
    
    
    set.seed(601)
    
    parts = createDataPartition(select_data$after48_loc2, p = .8)
    data.train = select_data[parts$Resample1, ]
    data.test = select_data[-parts$Resample1, ]
    
    sol_key = data.test$after48_loc2
    test_ids = data.test$id
    
    data.test$id = NULL
    data.test$after48_loc2 = NA
    
    
    varnames = setdiff(colnames(data.test), c("after48_loc2", "id"))
    
    data.train$id = NULL
    
    data.train$after48_loc2 = as.factor(data.train$after48_loc2)
    
    
    x = SMOTE(after48_loc2 ~., data.train, perc.over = 5000, perc.under = 1000)
    table(x$after48_loc2)
    
    
    x$after48_loc2 = as.integer(as.character(x$after48_loc2))
    
    data = rbind(x, data.test) %>% as.data.table()
    
    
    
    data$after48_loc2 = as.integer(data$after48_loc2)
    
    
    train_sparse = Matrix(as.matrix(data[!is.na(after48_loc2), varnames, with = F]), sparse = TRUE)
    test_sparse = Matrix(as.matrix(data[is.na(after48_loc2), varnames, with = F]), sparse = TRUE)
    
    y_train = data[!is.na(after48_loc2), after48_loc2]
    
    lgb.train = lgb.Dataset(data = train_sparse, label = y_train)
    
    lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = i, num_leaves = j,
                          num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                          eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)
    
    best.iter = lgb.model.cv$best_iter
    print(best.iter)
    
    
    lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = i,
                          num_leaves = j, num_threads = 10, nrounds = best.iter,
                          eval_freq = 20, eval = lgb.normalizedgini)
    
    
    
    
    X48H_COND_LOC_PROB = predict(lgb.model, test_sparse)
    preds = data.table(id = data.test$id, X48H_COND_LOC = ifelse(X48H_COND_LOC_PROB >= 0.5, 1, 0), X48H_COND_LOC_PROB = X48H_COND_LOC_PROB)
    
    preds$real_value = sol_key
    
    
    preds$real_value = as.factor(preds$real_value)
    preds$X48H_COND_LOC = as.factor(preds$X48H_COND_LOC)
    
    
    
    list_df = append(list_df, confusionMatrix(preds$real_value, preds$X48H_COND_LOC))
    
    
  }
}



list_df









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
select_data$after24_loc3 = NULL



select_data$plant_test.planet = NULL
select_data$plant_test.loc = NULL
select_data$after24h = NULL
select_data$after48h = NULL

select_data %>% head
select_data$id = 1:nrow(select_data)





# 성능 검증

set.seed(601)

parts = createDataPartition(select_data$after48_loc3, p = .8)
data.train = select_data[parts$Resample1, ]
data.test = select_data[-parts$Resample1, ]

sol_key = data.test$after48_loc3
test_ids = data.test$id

data.test$id = NULL
data.test$after48_loc3 = NA



# create LGB dataset
varnames = setdiff(colnames(data.test), c("after48_loc3", "id"))

data.train$id = NULL

data.train$after48_loc3 = as.factor(data.train$after48_loc3)

## 5:5 샘플링 - 20000, 100
## 10:1 샘플링 - 5000, 1000
## 5:1 샘플링 - 5000, 500
x = SMOTE(after48_loc3 ~., data.train, perc.over = 75000, perc.under = 5000)
table(x$after48_loc3)


x$after48_loc3 = as.integer(as.character(x$after48_loc3))

data = rbind(x, data.test) %>% as.data.table()
#data = rbind(data.train, data.test) %>% as.data.table()



data$after48_loc3 = as.integer(data$after48_loc3)
#data = rbind(data.train, data.test) %>% as.data.table()


train_sparse = Matrix(as.matrix(data[!is.na(after48_loc3), varnames, with = F]), sparse = TRUE)
test_sparse = Matrix(as.matrix(data[is.na(after48_loc3), varnames, with = F]), sparse = TRUE)

y_train = data[!is.na(after48_loc3), after48_loc3]
#test_ids = data[is.na(target), id]

lgb.train = lgb.Dataset(data = train_sparse, label = y_train)

lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.01, num_leaves = 25,
                      num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                      eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)

best.iter = lgb.model.cv$best_iter
print(best.iter)

# train final model

lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.01,
                      num_leaves = 25, num_threads = 10, nrounds = best.iter,
                      eval_freq = 20, eval = lgb.normalizedgini)



##  (특이도 0.) 


#tree.imp = lgb.importance(lgb.model, percentage = T)
#lgb.plot.importance(tree.imp, top_n = 15L, measure = "Gain")



X48H_COND_LOC_PROB = predict(lgb.model, test_sparse)
preds = data.table(id = data.test$id, X48H_COND_LOC = ifelse(X48H_COND_LOC_PROB >= 0.5, 1, 0), X48H_COND_LOC_PROB = X48H_COND_LOC_PROB)

preds$real_value = sol_key

preds$real_value = as.factor(preds$real_value)
preds$X48H_COND_LOC = as.factor(preds$X48H_COND_LOC)

confusionMatrix(preds$real_value, preds$X48H_COND_LOC)










## grid 서치.
list_df = list()
for(i in c(0.01, 0.03, 0.05, 0.07)) {
  for(j in c(25, 45, 100, 200)) {
    
    
    set.seed(602)
    
    parts = createDataPartition(select_data$after48_loc3, p = .8)
    data.train = select_data[parts$Resample1, ]
    data.test = select_data[-parts$Resample1, ]
    
    sol_key = data.test$after48_loc3
    test_ids = data.test$id
    
    data.test$id = NULL
    data.test$after48_loc3 = NA
    
    
    varnames = setdiff(colnames(data.test), c("after48_loc3", "id"))
    
    data.train$id = NULL
    
    data.train$after48_loc3 = as.factor(data.train$after48_loc3)
    
    
    x = SMOTE(after48_loc3 ~., data.train, perc.over = 75000, perc.under = 5000)
    table(x$after48_loc3)
    
    
    x$after48_loc3 = as.integer(as.character(x$after48_loc3))
    
    data = rbind(x, data.test) %>% as.data.table()
    
    
    
    data$after48_loc3 = as.integer(data$after48_loc3)
    
    
    train_sparse = Matrix(as.matrix(data[!is.na(after48_loc3), varnames, with = F]), sparse = TRUE)
    test_sparse = Matrix(as.matrix(data[is.na(after48_loc3), varnames, with = F]), sparse = TRUE)
    
    y_train = data[!is.na(after48_loc3), after48_loc3]
    
    lgb.train = lgb.Dataset(data = train_sparse, label = y_train)
    
    lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = i, num_leaves = j,
                          num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                          eval_freq = 20, eval = lgb.normalizedgini, nfold = 10, stratified = TRUE)
    
    best.iter = lgb.model.cv$best_iter
    print(best.iter)
    
    
    lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = i,
                          num_leaves = j, num_threads = 10, nrounds = best.iter,
                          eval_freq = 20, eval = lgb.normalizedgini)
    
    
    
    
    X48H_COND_LOC_PROB = predict(lgb.model, test_sparse)
    preds = data.table(id = data.test$id, X48H_COND_LOC = ifelse(X48H_COND_LOC_PROB >= 0.5, 1, 0), X48H_COND_LOC_PROB = X48H_COND_LOC_PROB)
    
    preds$real_value = sol_key
    
    
    preds$real_value = as.factor(preds$real_value)
    preds$X48H_COND_LOC = as.factor(preds$X48H_COND_LOC)
    
    
    
    list_df = append(list_df, confusionMatrix(preds$real_value, preds$X48H_COND_LOC))
    
    
  }
}



list_df


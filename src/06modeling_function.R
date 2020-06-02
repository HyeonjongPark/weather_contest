
lgb.grid = list(objective = "binary",
                metric = "auc",
                min_sum_hessian_in_leaf = 1,
                feature_fraction = 0.7,
                bagging_fraction = 0.7,
                bagging_freq = 5,
                min_data = 100,
                max_bin = 50,
                lambda_l1 = 8,
                lambda_l2 = 1.3,
                min_data_in_bin = 100,
                min_gain_to_split = 10,
                min_data_in_leaf = 30,
                is_unbalance = TRUE
)

# setting up Gini Eval function

lgb.normalizedgini = function(preds, dtrain) {
  actual = getinfo(dtrain, "label")
  score = NormalizedGini(preds, actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}



test = fread("data/steel/plant_test2.csv") %>% as.data.frame()
test %>% head

test$V1 = NULL
#test$id = NA
test$tem_diff = test$plant_test.tem_in - test$plant_test.tem_out_loc1
test$tem_diff_abs = abs(test$tem_diff)


test$plant_test.mea_ddhr = ymd_hm(test$plant_test.mea_ddhr)
test$month = month(test$plant_test.mea_ddhr)
test$day = day(test$plant_test.mea_ddhr)
test$hour = hour(test$plant_test.mea_ddhr)
test$minute = minute(test$plant_test.mea_ddhr)

test$id = 1:nrow(test)


test %>% head
train2_loc1 %>% head







## 24h later prediction


h24_function = function(dtrain, dtest, plant_num, loc_num) {
  
  
  test_filter = dtest %>% filter(plant_test.plant == plant_num & plant_test.loc == loc_num)
  
  test_filter$plant_test.mea_ddhr = NULL
  test_filter$plant_test.plant = NULL
  test_filter$plant_test.loc = NULL
  test_filter$plant_test.x24h_tma = NULL
  test_filter$plant_test.x48h_tma = NULL
  test_filter$plant_test.x48h_cond_loc = NULL
  
  
  dtrain$plant_test.planet = NULL
  dtrain$plant_test.loc = NULL
  dtrain$after24h = NULL
  dtrain$after48h = NULL
  
  
  if(plant_num == 1){
    dtrain$month = month(dtrain$plant1_train.mea_ddhr)
    dtrain$day = day(dtrain$plant1_train.mea_ddhr)
    dtrain$hour = hour(dtrain$plant1_train.mea_ddhr)
    dtrain$minute = minute(dtrain$plant1_train.mea_ddhr)
    dtrain$id = NA
    
    dtrain$plant1_train.mea_ddhr = NULL
    
    if(loc_num == 1) dtrain$after48_loc1 = NULL
    else if(loc_num == 2) dtrain$after48_loc2 = NULL
    else dtrain$after48_loc3 = NULL
  }
  
  else if(plant_num == 2){
    dtrain$month = month(dtrain$plant2_train.mea_ddhr)
    dtrain$day = day(dtrain$plant2_train.mea_ddhr)
    dtrain$hour = hour(dtrain$plant2_train.mea_ddhr)
    dtrain$minute = minute(dtrain$plant2_train.mea_ddhr)
    dtrain$id = NA
    
    dtrain$plant2_train.mea_ddhr = NULL
    
    if(loc_num == 1) dtrain$after48_loc1 = NULL
    else if(loc_num == 2) dtrain$after48_loc2 = NULL
    else dtrain$after48_loc3 = NULL
  }
  colnames(dtrain) = colnames(test_filter)
  
  
  data = rbind(dtrain, test_filter) %>% as.data.table()
  
  set.seed(601)
  
  # create LGB dataset
  varnames = setdiff(colnames(data), c("plant_test.x24h_cond_loc", "id"))
  train_sparse = Matrix(as.matrix(data[!is.na(plant_test.x24h_cond_loc), varnames, with = F]), sparse = TRUE)
  test_sparse = Matrix(as.matrix(data[is.na(plant_test.x24h_cond_loc), varnames, with = F]), sparse = TRUE)
  
  y_train = data[!is.na(plant_test.x24h_cond_loc), plant_test.x24h_cond_loc]
  #test_ids = data[is.na(target), id]
  
  lgb.train = lgb.Dataset(data = train_sparse, label = y_train)
  
  lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.02, num_leaves = 25,
                        num_threads = 2 , nrounds = 5000, early_stopping_rounds = 50,
                        eval_freq = 20, eval = lgb.normalizedgini, nfold = 5, stratified = TRUE)
  
  best.iter = lgb.model.cv$best_iter
  print(best.iter)
  
  # train final model
  
  lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.02,
                        num_leaves = 25, num_threads = 10, nrounds = best.iter,
                        eval_freq = 20, eval = lgb.normalizedgini)
  
  X24H_COND_LOC_PROB = predict(lgb.model, test_sparse)
  preds = data.table(id = test_filter$id, X24H_COND_LOC = ifelse(X24H_COND_LOC_PROB >= 0.5, 1, 0), X24H_COND_LOC_PROB = X24H_COND_LOC_PROB)
  
  return(preds)
  
}

sub24_1_1 = h24_function(train1_loc1, test, 1, 1)
sub24_1_2 = h24_function(train1_loc2, test, 1, 2)
sub24_1_3 = h24_function(train1_loc3, test, 1, 3)
sub24_2_1 = h24_function(train2_loc1, test, 2, 1)
sub24_2_2 = h24_function(train2_loc2, test, 2, 2)
sub24_2_3 = h24_function(train2_loc3, test, 2, 3)















## 48h later prediction



h48_function = function(dtrain, dtest, plant_num, loc_num) {
  
  
  test_filter = dtest %>% filter(plant_test.plant == plant_num & plant_test.loc == loc_num)
  
  test_filter$plant_test.mea_ddhr = NULL
  test_filter$plant_test.plant = NULL
  test_filter$plant_test.loc = NULL
  test_filter$plant_test.x24h_tma = NULL
  test_filter$plant_test.x24h_cond_loc = NULL
  test_filter$plant_test.x48h_tma = NULL
  
  
  
  dtrain$plant_test.planet = NULL
  dtrain$plant_test.loc = NULL
  dtrain$after24h = NULL
  dtrain$after48h = NULL
  
  
  if(plant_num == 1){
    dtrain$month = month(dtrain$plant1_train.mea_ddhr)
    dtrain$day = day(dtrain$plant1_train.mea_ddhr)
    dtrain$hour = hour(dtrain$plant1_train.mea_ddhr)
    dtrain$minute = minute(dtrain$plant1_train.mea_ddhr)
    dtrain$id = NA
    
    dtrain$plant1_train.mea_ddhr = NULL
    
    if(loc_num == 1) dtrain$after24_loc1 = NULL
    else if(loc_num == 2) dtrain$after24_loc2 = NULL
    else dtrain$after24_loc3 = NULL
  }
  
  else if(plant_num == 2){
    dtrain$month = month(dtrain$plant2_train.mea_ddhr)
    dtrain$day = day(dtrain$plant2_train.mea_ddhr)
    dtrain$hour = hour(dtrain$plant2_train.mea_ddhr)
    dtrain$minute = minute(dtrain$plant2_train.mea_ddhr)
    dtrain$id = NA
    
    dtrain$plant2_train.mea_ddhr = NULL
    
    if(loc_num == 1) dtrain$after24_loc1 = NULL
    else if(loc_num == 2) dtrain$after24_loc2 = NULL
    else dtrain$after24_loc3 = NULL
  }
  colnames(dtrain) = colnames(test_filter)
  
  
  data = rbind(dtrain, test_filter) %>% as.data.table()
  
  set.seed(601)
  
  # create LGB dataset
  varnames = setdiff(colnames(data), c("plant_test.x48h_cond_loc", "id"))
  train_sparse = Matrix(as.matrix(data[!is.na(plant_test.x48h_cond_loc), varnames, with = F]), sparse = TRUE)
  test_sparse = Matrix(as.matrix(data[is.na(plant_test.x48h_cond_loc), varnames, with = F]), sparse = TRUE)
  
  y_train = data[!is.na(plant_test.x48h_cond_loc), plant_test.x48h_cond_loc]
  #test_ids = data[is.na(target), id]
  
  lgb.train = lgb.Dataset(data = train_sparse, label = y_train)
  
  lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.02, num_leaves = 25,
                        num_threads = 10 , nrounds = 5000, early_stopping_rounds = 50,
                        eval_freq = 20, eval = lgb.normalizedgini, nfold = 5, stratified = TRUE)
  
  best.iter = lgb.model.cv$best_iter
  print(best.iter)
  
  # train final model
  
  lgb.model = lgb.train(params = lgb.grid, data = lgb.train, learning_rate = 0.02,
                        num_leaves = 25, num_threads = 10, nrounds = best.iter,
                        eval_freq = 20, eval = lgb.normalizedgini)
  
  
  X48H_COND_LOC_PROB = predict(lgb.model, test_sparse)
  preds = data.table(id = test_filter$id, X48H_COND_LOC = ifelse(X48H_COND_LOC_PROB >= 0.5, 1, 0), X48H_COND_LOC_PROB = X48H_COND_LOC_PROB)
  
  return(preds)
  
}

sub48_1_1 = h48_function(train1_loc1, test, 1, 1)
sub48_1_2 = h48_function(train1_loc2, test, 1, 2)
sub48_1_3 = h48_function(train1_loc3, test, 1, 3)
sub48_2_1 = h48_function(train2_loc1, test, 2, 1)
sub48_2_2 = h48_function(train2_loc2, test, 2, 2)
sub48_2_3 = h48_function(train2_loc3, test, 2, 3)


sub_1_1 = merge(sub24_1_1, sub48_1_1)
sub_1_2 = merge(sub24_1_2, sub48_1_2)
sub_1_3 = merge(sub24_1_3, sub48_1_3)
sub_2_1 = merge(sub24_2_1, sub48_2_1)
sub_2_2 = merge(sub24_2_2, sub48_2_2)
sub_2_3 = merge(sub24_2_3, sub48_2_3)

submission = rbind(sub_1_1, sub_1_2, sub_1_3, sub_2_1, sub_2_2, sub_2_3)
submission = submission %>% arrange(id)

submission %>% head

test_submission = test
test_submission %>% head

test_submission = test_submission[,c(1,2,3,9,10,14,11,12,15)]
test_submission %>% head

colnames(test_submission) = c("MEA_DDHR",
                              "PLANT",
                              "LOC",
                              "X24H_TMA",
                              "X24H_COND_LOC",
                              "X24H_COND_LOC_PROB",
                              "X48H_TMA",
                              "X48H_COND_LOC",
                              "X48H_COND_LOC_PROB")


test_submission$X24H_COND_LOC_PROB = NA
test_submission$X48H_COND_LOC_PROB = NA

test_submission$X24H_COND_LOC = submission$X24H_COND_LOC
test_submission$X24H_COND_LOC_PROB = submission$X24H_COND_LOC_PROB
test_submission$X48H_COND_LOC = submission$X48H_COND_LOC
test_submission$X48H_COND_LOC_PROB = submission$X48H_COND_LOC_PROB

test_submission %>% head


getwd()

fwrite(test_submission, "./submission/submission1.csv")







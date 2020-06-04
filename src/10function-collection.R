
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
                min_data_in_leaf = 30, # 30 -> 2 수정 
                is_unbalance = TRUE
)

# setting up Gini Eval function

lgb.normalizedgini = function(preds, dtrain) {
  actual = getinfo(dtrain, "label")
  score = NormalizedGini(preds, actual)
  return(list(name = "gini", value = score, higher_better = TRUE))
}

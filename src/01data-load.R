rm(list = ls())
getwd()

## library load

library(data.table)
library(dplyr)
library(lubridate)

## data load

### plant1 
plant1_train = fread("./data/steel/plant1_train2.csv") %>% as.data.frame()

plant1_row = plant1_train %>% nrow()

# V1 제거
plant1_train$V1 = NULL

# 날짜형 데이터타입으로 변경
plant1_train$plant1_train.mea_ddhr = ymd_hm(plant1_train$plant1_train.mea_ddhr)

plant1_train$after24h = plant1_train$plant1_train.mea_ddhr %>% update(mday = 2)
plant1_train$after48h = plant1_train$plant1_train.mea_ddhr %>% update(mday = 3)

# 24시간 후 데이터
plant1_cond_after24 = plant1_train[,c("plant1_train.mea_ddhr", "plant1_train.cond_loc1", "plant1_train.cond_loc2", "plant1_train.cond_loc3")]
plant1_cond_after24 = plant1_cond_after24[-c(1:8),]
plant1_cond_after24$plant1_train.mea_ddhr = plant1_cond_after24$plant1_train.mea_ddhr -(60 * 60 * 24)
plant1_cond_after24 %>% tail
colnames(plant1_cond_after24) = c("plant1_train.mea_ddhr", "after24_loc1", "after24_loc2", "after24_loc3")


# 48시간 후 데이터
plant1_cond_after48 = plant1_train[,c("plant1_train.mea_ddhr", "plant1_train.cond_loc1", "plant1_train.cond_loc2", "plant1_train.cond_loc3")]
plant1_cond_after48 = plant1_cond_after48[-c(1:16),]
plant1_cond_after48$plant1_train.mea_ddhr = plant1_cond_after48$plant1_train.mea_ddhr -(60 * 60 * 24 * 2)
plant1_cond_after48 %>% tail()
colnames(plant1_cond_after48) = c("plant1_train.mea_ddhr", "after48_loc1", "after48_loc2", "after48_loc3")


# 데이터 결합
train1 = merge(plant1_train, plant1_cond_after24, all = T)
train1 = merge(train1, plant1_cond_after48, all = T)
train1 %>% dim() # 59346
train1 = na.omit(train1)
dim(train1) # 56848

colSums(is.na(train1))


table(train1$plant1_train.cond_loc1) # 0 : 56575, 1 : 273
table(train1$plant1_train.cond_loc2) # 0 : 56367, 1 : 481
table(train1$plant1_train.cond_loc3) # 0 : 56225, 1 : 623

train1 %>% tail()

train1$plant_test.planet = 1
train1$plant_test.loc = NA
colnames(train1)



# 구역별로 데이터 리폼
train1_loc1 = train1[,c("plant1_train.mea_ddhr", "plant_test.planet", "plant_test.loc", "plant1_train.tem_in_loc1", "plant1_train.hum_in_loc1",
                        "plant1_train.tem_coil_loc1", "plant1_train.tem_out_loc1", "plant1_train.hum_out_loc1",
                        "after24h", "after24_loc1", "after48h", "after48_loc1"
)]
train1_loc1$plant_test.loc = 1
train1_loc1$tem_diff = train1_loc1$plant1_train.tem_in_loc1 - train1_loc1$plant1_train.tem_out_loc1
train1_loc1$tem_diff_abs = abs(train1_loc1$tem_diff)

train1_loc2 = train1[,c("plant1_train.mea_ddhr", "plant_test.planet", "plant_test.loc", "plant1_train.tem_in_loc2", "plant1_train.hum_in_loc2",
                        "plant1_train.tem_coil_loc2", "plant1_train.tem_out_loc1", "plant1_train.hum_out_loc1",
                        "after24h", "after24_loc2", "after48h", "after48_loc2"
)]
train1_loc2$plant_test.loc = 2
train1_loc2$tem_diff =train1_loc2$plant1_train.tem_in_loc2 - train1_loc2$plant1_train.tem_out_loc1 
train1_loc2$tem_diff_abs = abs(train1_loc2$tem_diff)


train1_loc3 = train1[,c("plant1_train.mea_ddhr", "plant_test.planet", "plant_test.loc", "plant1_train.tem_in_loc3", "plant1_train.hum_in_loc3",
                        "plant1_train.tem_coil_loc3", "plant1_train.tem_out_loc1", "plant1_train.hum_out_loc1",
                        "after24h", "after24_loc3", "after48h", "after48_loc3"
)]
train1_loc3$plant_test.loc = 3
train1_loc3$tem_diff = train1_loc3$plant1_train.tem_in_loc3 - train1_loc3$plant1_train.tem_out_loc1
train1_loc3$tem_diff_abs = abs(train1_loc3$tem_diff)


train1_loc3
getwd()
#fwrite(train1_loc1, "./output/train1_loc1.csv")
#fwrite(train1_loc2, "./output/train1_loc2.csv")
#fwrite(train1_loc3, "./output/train1_loc3.csv")



train1_loc1 %>% head












# plant 2

plant2_train = fread("./data/steel/plant2_train2.csv") %>% as.data.frame()

plant2_row = plant2_train %>% nrow()

# V1 제거
plant2_train$V1 = NULL

# 날짜형 데이터타입으로 변경
plant2_train$plant2_train.mea_ddhr = ymd_hm(plant2_train$plant2_train.mea_ddhr)

plant2_train$after24h = plant2_train$plant2_train.mea_ddhr %>% update(mday = 2)
plant2_train$after48h = plant2_train$plant2_train.mea_ddhr %>% update(mday = 3)

# 24시간 후 데이터
plant2_cond_after24 = plant2_train[,c("plant2_train.mea_ddhr", "plant2_train.cond_loc1", "plant2_train.cond_loc2", "plant2_train.cond_loc3")]
plant2_cond_after24 = plant2_cond_after24[-c(1:8),]
plant2_cond_after24$plant2_train.mea_ddhr = plant2_cond_after24$plant2_train.mea_ddhr -(60 * 60 * 24)
plant2_cond_after24 %>% tail
colnames(plant2_cond_after24) = c("plant2_train.mea_ddhr", "after24_loc1", "after24_loc2", "after24_loc3")


# 48시간 후 데이터
plant2_cond_after48 = plant2_train[,c("plant2_train.mea_ddhr", "plant2_train.cond_loc1", "plant2_train.cond_loc2", "plant2_train.cond_loc3")]
plant2_cond_after48 = plant2_cond_after48[-c(1:16),]
plant2_cond_after48$plant2_train.mea_ddhr = plant2_cond_after48$plant2_train.mea_ddhr -(60 * 60 * 24 * 2)
plant2_cond_after48 %>% tail()
colnames(plant2_cond_after48) = c("plant2_train.mea_ddhr", "after48_loc1", "after48_loc2", "after48_loc3")


# 데이터 결합
train2 = merge(plant2_train, plant2_cond_after24, all = T)
train2 = merge(train2, plant2_cond_after48, all = T)
train2 %>% dim() # 58473
train2 = na.omit(train2)
dim(train2) # 56838

colSums(is.na(train2))


table(train2$plant2_train.cond_loc1) # 0 : 56568, 1 : 270
table(train2$plant2_train.cond_loc2) # 0 : 56566, 1 : 272
table(train2$plant2_train.cond_loc3) # 0 : 56808, 1 : 30

train2 %>% tail()

train2$plant_test.planet = 2
train2$plant_test.loc = NA
colnames(train2)



# 구역별로 데이터 리폼
train2_loc1 = train2[,c("plant2_train.mea_ddhr", "plant_test.planet", "plant_test.loc", "plant2_train.tem_in_loc1", "plant2_train.hum_in_loc1",
                        "plant2_train.tem_coil_loc1", "plant2_train.tem_out_loc1", "plant2_train.hum_out_loc1",
                        "after24h", "after24_loc1", "after48h", "after48_loc1"
)]
train2_loc1$plant_test.loc = 1
train2_loc1$tem_diff = train2_loc1$plant2_train.tem_in_loc1 - train2_loc1$plant2_train.tem_out_loc1
train2_loc1$tem_diff_abs = abs(train2_loc1$tem_diff)



train2_loc2 = train2[,c("plant2_train.mea_ddhr", "plant_test.planet", "plant_test.loc", "plant2_train.tem_in_loc2", "plant2_train.hum_in_loc2",
                        "plant2_train.tem_coil_loc2", "plant2_train.tem_out_loc1", "plant2_train.hum_out_loc1",
                        "after24h", "after24_loc2", "after48h", "after48_loc2"
)]
train2_loc2$plant_test.loc = 2
train2_loc2$tem_diff = train2_loc2$plant2_train.tem_in_loc2 - train2_loc2$plant2_train.tem_out_loc1
train2_loc2$tem_diff_abs = abs(train2_loc2$tem_diff)



train2_loc3 = train2[,c("plant2_train.mea_ddhr", "plant_test.planet", "plant_test.loc", "plant2_train.tem_in_loc3", "plant2_train.hum_in_loc3",
                        "plant2_train.tem_coil_loc3", "plant2_train.tem_out_loc1", "plant2_train.hum_out_loc1",
                        "after24h", "after24_loc3", "after48h", "after48_loc3"
)]
train2_loc3$plant_test.loc = 3
train2_loc3$tem_diff = train2_loc3$plant2_train.tem_in_loc3 - train2_loc3$plant2_train.tem_out_loc1
train2_loc3$tem_diff_abs = abs(train2_loc3$tem_diff)



getwd()
#fwrite(train2_loc1, "./output/train2_loc1.csv")
#fwrite(train2_loc2, "./output/train2_loc2.csv")
#fwrite(train2_loc3, "./output/train2_loc3.csv")



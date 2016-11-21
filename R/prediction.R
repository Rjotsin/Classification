library(MASS)
library(class)
library(plyr)
library(reshape2)
library(dplyr)
library(tidyr)

set.seed(1017)

TRAIN_DATA_PATH="/Users/josephlugo/Google Drive/School/4B/STAT 441/Group Project/combined.csv"
TEST_DATA_PATH="/Users/josephlugo/Google Drive/School/4B/STAT 441/Group Project/test.csv"
AVARS_DATA_PATH="/Users/josephlugo/Google Drive/School/4B/STAT 441/Group Project/avars1.csv"
SAMPLE_DATA_PATH="/Users/josephlugo/Google Drive/School/4B/STAT 441/Group Project/sample_submission.csv"

sample_data <- read.csv(SAMPLE_DATA_PATH, 
                          header=TRUE, 
                          sep=",", 
                          stringsAsFactors=F)

original_train_data <- read.csv(TRAIN_DATA_PATH, 
                          header=TRUE, 
                          sep=",", 
                          stringsAsFactors=F)

original_test_data <- read.csv(TEST_DATA_PATH, 
                          header=TRUE, 
                          sep=",", 
                          stringsAsFactors=F)

avars_data <- read.csv(AVARS_DATA_PATH, 
                      header=TRUE, 
                      sep=",", 
                      stringsAsFactors=F)

colnames(avars_data) <- c("id","gender","position","year_birth","age_member","age_cat","age_head","num_members",
  "num_children","partner","civil_status","dom_sit","dwell_type","urban_char","occ","gross_monthly_income",
  "gross_monthly_income_imputed","net_monthly_income","net_monthly_income_capped","net_monthly_income_imputed",
  "gross_monthly_income_cat","net_monthly_income_cat","gross_household_income","net_household_income",
  "edu","edu_diploma","edu_cat","is_member","recruitment","origin","have_simPC")

logLoss <- function(pred, actual){
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}


f_measure <- function(data_frame){
  tp <- 0
  fp <- 0
  fn <- 0
  
  for (i in 1:nrow(data_frame)){
    tp <- tp + cm[i,i]
    fp <- fp + sum(cm[, i])-cm[i, i]
    fn <- fn + sum(cm[i,])-cm[i, i]
  }

  precision <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  f <- 2*(precision*recall)/(precision + recall)
  
  return(f)
}

parse_datetimes <- function(data_frame, delimeter,col){

  splits <- strsplit(data_frame[,c(col)],delimeter)

  split_1 <- unlist(lapply(1:nrow(data_frame), function(x) paste(splits[[x]][1],sep="")))
  split_2 <- unlist(lapply(1:nrow(data_frame), function(x) paste(splits[[x]][2],sep="")))
  split_3 <- unlist(lapply(1:nrow(data_frame), function(x) paste(splits[[x]][3],sep="")))

  output <- list(c(split_1), c(split_2), c(split_3))

  return(output)

}

cleanup <- function(data_frame, test = FALSE){

  if (test == FALSE){
    data_frame <- na.omit(data_frame)
    data_frame$interesting <- as.factor(data_frame$interesting)
    data_frame$difficult <- as.factor(data_frame$difficult)
    data_frame$clear <- as.factor(data_frame$clear)
    data_frame$thinking <- as.factor(data_frame$thinking)
    data_frame$enjoy <- as.factor(data_frame$enjoy)
    # data_frame$obs <- NULL
  }

  data_frame$year <- as.factor(substr(data_frame$year_month_m,0,4))
  data_frame$month <- as.factor(substr(data_frame$year_month_m,5,6))
  data_frame[data_frame$core == "",]$core <- "leisure"
  data_frame$core <- as.factor(data_frame$core)

  start_dates <- parse_datetimes(data_frame,"-", "startdate")
  start_dates[[3]] <- unlist(lapply(1:nrow(data_frame), function(x) if(start_dates[[3]][x] == "08"){start_dates[[3]][x] <- "2008"} else {start_dates[[3]][x]}))
  start_dates[[3]] <- unlist(lapply(1:nrow(data_frame), function(x) if(start_dates[[3]][x] == "09"){start_dates[[3]][x] <- "2009"} else {start_dates[[3]][x]}))

  data_frame$start_day <- as.factor(as.numeric(start_dates[[1]]))
  data_frame$start_month <- as.factor(as.numeric(start_dates[[2]]))
  data_frame$start_year <- as.factor(as.numeric(start_dates[[3]]))

  end_dates <- parse_datetimes(data_frame,"-", "enddate")
  end_dates[[3]] <- unlist(lapply(1:nrow(data_frame), function(x) if(end_dates[[3]][x] == "08"){end_dates[[3]][x] <- "2008"} else {end_dates[[3]][x]}))
  end_dates[[3]] <- unlist(lapply(1:nrow(data_frame), function(x) if(end_dates[[3]][x] == "09"){end_dates[[3]][x] <- "2009"} else {end_dates[[3]][x]}))

  data_frame$end_day <- as.factor(as.numeric(end_dates[[1]]))
  data_frame$end_month <- as.factor(as.numeric(end_dates[[2]]))
  data_frame$end_year <- as.factor(as.numeric(end_dates[[3]]))

  start_times <- parse_datetimes(data_frame,":","starttime")

  data_frame$start_hr <- as.factor(as.numeric(start_times[[1]]))
  data_frame$start_min <- as.factor(as.numeric(start_times[[2]]))
  data_frame$start_sec <- as.factor(floor(as.numeric(start_times[[3]])))
  data_frame$start_am <- as.factor(as.numeric(data_frame$start_hr) < 12)

  end_times <- parse_datetimes(data_frame,":","endtime")

  data_frame$end_hr <- as.factor(as.numeric(end_times[[1]]))
  data_frame$end_min <- as.factor(as.numeric(end_times[[2]]))
  data_frame$end_sec <- as.factor(floor(as.numeric(end_times[[3]])))
  data_frame$end_am <- as.factor(as.numeric(data_frame$end_hr) < 12)

  new_start <- unlist(lapply(1:nrow(data_frame), function(x) paste(start_dates[[1]][x],"-",start_dates[[2]][x],"-",start_dates[[3]][x],sep="")))
  new_end <- unlist(lapply(1:nrow(data_frame), function(x) paste(end_dates[[1]][x],"-",end_dates[[2]][x],"-",end_dates[[3]][x],sep="")))
  data_frame$startdate <- new_start
  data_frame$enddate <- new_end

  data_frame$startdate_epoch <- floor(as.integer(as.POSIXct(data_frame$startdate, format = "%d-%M-%Y"))/86400)
  data_frame$enddate_epoch <- floor(as.integer(as.POSIXct(data_frame$enddate, format = "%d-%M-%Y"))/86400)
  
  count_table <- as.data.frame(table(data_frame$id))
  colnames(count_table) <- c("id","num_surveys")
  data_frame <- merge(data_frame, count_table, all.x = TRUE,by = "id")
  
  # core_table <- count(data_frame, c("id","core"))
  core_table <- as.data.frame(table(data_frame$id,data_frame$core))
  
  # core_table_wide <- dcast(core_table,formula = id~core,value.var="freq")
  core_table_wide <- dcast(core_table,formula = Var1~Var2,value.var="Freq")
  colnames(core_table_wide) <- c("id","num_health","num_income","num_leisure")
  
  data_frame <- merge(data_frame, core_table_wide, all.x = TRUE,by = "id")
  
  # Finding the number of surveys up to that point
  data_frame <- data_frame[with(data_frame, order(id,startdate_epoch)),]
  data_frame <- data_frame %>% group_by(id) %>% mutate(past_surveys=0:(n()-1))

  # Finding the number of past surveys done for each core 
  data_frame_leisure <- data_frame[data_frame$core == "leisure",]
  data_frame_leisure$past_health_surveys <- NA
  data_frame_leisure$past_income_surveys <- NA
  data_frame_leisure <- data_frame_leisure %>% group_by(id) %>% mutate(past_leisure_surveys=0:(n()-1))

  data_frame_health <- data_frame[data_frame$core == "health",]
  data_frame_health$past_leisure_surveys <- NA
  data_frame_health$past_income_surveys <- NA
  data_frame_health <- data_frame_health %>% group_by(id) %>% mutate(past_health_surveys=0:(n()-1))

  data_frame_income <- data_frame[data_frame$core == "income",]
  data_frame_income$past_health_surveys <- NA
  data_frame_income$past_leisure_surveys <- NA
  data_frame_income <- data_frame_income %>% group_by(id) %>% mutate(past_income_surveys=0:(n()-1))

  data_frame <- rbind(data_frame_health, data_frame_income)
  data_frame <- rbind(data_frame, data_frame_leisure)

  data_frame <- data_frame[with(data_frame, order(id,startdate_epoch)),]
  data_frame <- data_frame %>% group_by(id) %>% fill(past_leisure_surveys)

  data_frame <- data_frame[with(data_frame, order(id,startdate_epoch)),]
  data_frame <- data_frame %>% group_by(id) %>% fill(past_health_surveys)

  data_frame <- data_frame[with(data_frame, order(id,startdate_epoch)),]
  data_frame <- data_frame %>% group_by(id) %>% fill(past_income_surveys)

  data_frame[is.na(data_frame$past_income_surveys),]$past_income_surveys <- 0
  data_frame[is.na(data_frame$past_health_surveys),]$past_health_surveys <- 0
  data_frame[is.na(data_frame$past_leisure_surveys),]$past_leisure_surveys <- 0
  
  duration_means <- ddply(data_frame,~id,summarise,mean=mean(duration))
  colnames(duration_means) <- c("id","duration_mean") 
  data_frame <- merge(data_frame, duration_means, all.x = TRUE,by = "id")

  factors <- c("gender","position","year_birth","age_member","age_cat","age_head","num_members", "num_children","partner","civil_status",
            "dom_sit","dwell_type","urban_char","occ", "gross_monthly_income_cat","net_monthly_income_cat", "edu","edu_diploma",
            "edu_cat","is_member","recruitment","origin","have_simPC")
         
  combined <- merge(x=data_frame,y=avars_data,by="id",all.x=TRUE)
  combined_clean <- combined
  combined_clean$train <- NULL
  combined_clean$year_month_m <- as.factor(combined_clean$year_month_m)
  combined_clean$startdate <- as.factor(combined_clean$startdate)
  combined_clean$enddate <- as.factor(combined_clean$enddate)
  combined_clean$starttime <- NULL
  combined_clean$endtime <- NULL
  combined_clean$core <- as.factor(combined_clean$core)

  for (i in 1:length(factors)) {
    combined_clean[,factors[i]] <- as.factor(combined_clean[,factors[i]])
  }   
  
  combined_clean <- subset(combined_clean, select=-c(gross_monthly_income,net_monthly_income,net_monthly_income_capped))

  return(combined_clean)

}

get_means <- function(data_frame){
  data_frame_enjoy <- data_frame %>% group_by(id) %>% summarise(enjoy_mean=as.factor(round(mean(as.numeric(enjoy)))))
  data_frame_difficult <- data_frame %>% group_by(id) %>% summarise(difficult_mean=as.factor(round(mean(as.numeric(difficult)))))
  data_frame_thinking <- data_frame %>% group_by(id) %>% summarise(thinking_mean=as.factor(round(mean(as.numeric(thinking)))))
  data_frame_clear <- data_frame %>% group_by(id) %>% summarise(clear_mean=as.factor(round(mean(as.numeric(clear)))))
  data_frame_interesting <- data_frame %>% group_by(id) %>% summarise(interesting_mean=as.factor(round(mean(as.numeric(interesting)))))

  data_frame <- merge(x=data_frame,y=data_frame_enjoy,by="id")
  data_frame <- merge(x=data_frame,y=data_frame_difficult,by="id")
  data_frame <- merge(x=data_frame,y=data_frame_thinking,by="id")
  data_frame <- merge(x=data_frame,y=data_frame_clear,by="id")
  data_frame <- merge(x=data_frame,y=data_frame_interesting,by="id")

  data_frame <- subset(data_frame, select=-c(enjoy,thinking,clear,difficult))

  return(data_frame)
}

subset_train <- function(data_frame){

  data_frame <- cleanup(data_frame, test=FALSE)

  # TESTING ON OUR KNOWN DATA
  train_percentage <- 0.8
  train_index <- sample(1:nrow(data_frame), floor(nrow(data_frame)*train_percentage))
  train_data <- data_frame[train_index, ]
  row.names(train_data) <- NULL

  test_data <- data_frame[-train_index, ]
  test_index <- as.numeric(row.names(test_data))
  row.names(test_data) <- NULL

  train_data <- get_means(train_data)
  train_data$id <- as.factor(train_data$id)
  
  test_data$id <- as.factor(test_data$id)
  test_data <- subset(test_data, select=-c(enjoy,thinking,clear,difficult))
  test_data <- merge(x=test_data, 
                     y=unique(train_data[,c("id","thinking_mean","enjoy_mean","difficult_mean","clear_mean","interesting_mean")]),
                     by="id",
                     all.x=TRUE)
  
  train_data$id <- NULL
  train_data$obs <- NULL

  data <- list(train_data, test_data)

  return(data)
}

# CLEANING UP THE TRAIN_DATA
# think about grouping by id
# how many surveys have they done
# how many surveys have they done up to that point 
# average duration 
train_data <- cleanup(original_train_data, test=FALSE)
train_data$obs <- NULL

# GETTING THE MEANS BY ID - THIS MADE THE BIGGEST DIFFERENCE
train_data <- get_means(train_data)

# TRYING TO IMPUTE THE TRAIN DATA
# library(mice)

# nums_og <- sapply(data_frame, is.numeric)
# ints_og <- sapply(data_frame, is.integer)
# temp_data_og <- original_data[,nums_og|ints_og]
# temp_data_og$startdate_epoch <- NULL
# temp_data_og$enddate_epoch <- NULL
# transform_og <- mice(temp_data_og,m=2,maxit=1,meth="rf")
# completed_data_og <- complete(transform_og,1)
# og_temp <- original_data[ , -which(names(original_data) %in% colnames(completed_data_og))]
# imputed_data_og <- cbind(completed_data_og, og_temp)

# original_data <- upSample(original_data, original_data$interesting)

# CLEANING UP THE TEST_DATA
test_data <- cleanup(original_test_data, test=TRUE)
test_data <- merge(x=test_data, 
                   y=unique(train_data[,c("id","thinking_mean","enjoy_mean","difficult_mean","clear_mean","interesting_mean")]),
                   by="id",
                   all.x=TRUE)

# TRYING TO IMPUTE THE TEST DATA
# test_data$thinking_mean <- as.numeric(test_data$thinking_mean)
# test_data$enjoy_mean <- as.numeric(test_data$enjoy_mean)
# test_data$difficult_mean <- as.numeric(test_data$difficult_mean)
# test_data$clear_mean <- as.numeric(test_data$clear_mean)
# test_data$interesting_mean <- as.numeric(test_data$interesting_mean)
# nums <- sapply(test_data, is.numeric)
# ints <- sapply(test_data, is.integer)

# temp_data_test <- test_data[,nums|ints]
# temp_data_test$obs <- NULL
# temp_data_test$id <- NULL
# temp_data_test$startdate_epoch <- NULL
# temp_data_test$enddate_epoch <- NULL
# transform_test <- mice(temp_data_test,m=2,maxit=1,meth="rf")
# completed_data_test <- complete(transform_test,1)
# test_temp <- test_data[ , -which(names(test_data) %in% colnames(completed_data_test))]
# imputed_data_test <- cbind(completed_data_test, test_temp)

# test_data$thinking_mean <- as.factor(test_data$thinking_mean)
# test_data$enjoy_mean <- as.factor(test_data$enjoy_mean)
# test_data$difficult_mean <- as.factor(test_data$difficult_mean)
# test_data$clear_mean <- as.factor(test_data$clear_mean)
# test_data$interesting_mean <- as.factor(test_data$interesting_mean)

# SUBSETTING THE TRAIN_DATA
train_list <- subset_train(original_train_data)
train_data <- train_list[[1]]
test_data <- train_list[[2]]

# > table(original_data$interesting)

#     1     2     3     4     5 
#  4079  7748 30086 24117 14262  
			   
# BOOSTING
library(gbm)
library(caret)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 5,
  repeats = 2)

gbmFit1 <- train(interesting ~ .,
                 data = train_data[sample(1:nrow(train_data),500),],
                 na.action = na.omit,
                 method = "gbm", 
                 trControl = fitControl,
                 metric = "Accuracy",
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

boosting_results <- resamples(list(gbm = gbmFit1,gbm = gbmFit1))

difficult_gbm_fit <- gbm(difficult_mean ~ ., 
               cv.folds = 2,
               # weights = train_data$weights,
               n.trees = 65,
               data = train_data[,-3], 
               distribution = "multinomial",
               shrinkage = 0.1, 
               interaction.depth = 3, 
               n.minobsinnode = 10)

best.iter <- gbm.perf(difficult_gbm_fit,method="OOB")
best.iter <- gbm.perf(difficult_gbm_fit,method="cv")

difficult_gbm_predict_prob <- as.data.frame(predict.gbm(difficult_gbm_fit, test_data, n.trees=best.iter, type="response"))
difficult_gbm_predict_guesses <- as.factor(max.col(difficult_gbm_predict_prob))

thinking_gbm_fit <- gbm(thinking_mean ~ ., 
               cv.folds = 2,
               # weights = train_data$weights,
               n.trees = 50,
               data = train_data[,-3], 
               distribution = "multinomial",
               shrinkage = 0.1, 
               interaction.depth = 3, 
               n.minobsinnode = 10)

best.iter <- gbm.perf(thinking_gbm_fit,method="OOB")
best.iter <- gbm.perf(thinking_gbm_fit,method="cv")

thinking_gbm_predict_prob <- as.data.frame(predict.gbm(thinking_gbm_fit, test_data, n.trees=best.iter, type="response"))
thinking_gbm_predict_guesses <- as.factor(max.col(thinking_gbm_predict_prob))

clear_gbm_fit <- gbm(clear_mean ~ ., 
               cv.folds = 2,
               # weights = train_data$weights,
               n.trees = 50,
               data = train_data[,-3], 
               distribution = "multinomial",
               shrinkage = 0.1, 
               interaction.depth = 3, 
               n.minobsinnode = 10)

best.iter <- gbm.perf(clear_gbm_fit,method="OOB")
best.iter <- gbm.perf(clear_gbm_fit,method="cv")

clear_gbm_predict_prob <- as.data.frame(predict.gbm(clear_gbm_fit, test_data, n.trees=best.iter, type="response"))
clear_gbm_predict_guesses <- as.factor(max.col(clear_gbm_predict_prob))

enjoy_gbm_fit <- gbm(enjoy_mean ~ ., 
               cv.folds = 2,
               # weights = train_data$weights,
               n.trees = 50,
               data = train_data[,-3], 
               distribution = "multinomial",
               shrinkage = 0.1, 
               interaction.depth = 3, 
               n.minobsinnode = 10)

best.iter <- gbm.perf(enjoy_gbm_fit,method="OOB")
best.iter <- gbm.perf(enjoy_gbm_fit,method="cv")

enjoy_gbm_predict_prob <- as.data.frame(predict.gbm(enjoy_gbm_fit, test_data, n.trees=best.iter, type="response"))
enjoy_gbm_predict_guesses <- as.factor(max.col(enjoy_gbm_predict_prob))

interesting_gbm_fit <- gbm(interesting_mean ~ ., 
               cv.folds = 2,
               # weights = train_data$weights,
               n.trees = 70,
               data = train_data[,-3], 
               distribution = "multinomial",
               shrinkage = 0.1, 
               interaction.depth = 3, 
               n.minobsinnode = 10)

best.iter <- gbm.perf(interesting_gbm_fit,method="OOB")
best.iter <- gbm.perf(interesting_gbm_fit,method="cv")

interesting_gbm_predict_prob <- as.data.frame(predict.gbm(interesting_gbm_fit, test_data, n.trees=best.iter, type="response"))
interesting_gbm_predict_guesses <- as.factor(max.col(interesting_gbm_predict_prob))

test_data$difficult_pred <- difficult_gbm_predict_guesses
test_data$thinking_pred <- thinking_gbm_predict_guesses
test_data$clear_pred <- clear_gbm_predict_guesses
test_data$enjoy_pred <- enjoy_gbm_predict_guesses
test_data$interesting_pred <- interesting_gbm_predict_guesses

test_data[is.na(test_data$thinking_mean),]$thinking_mean <- test_data[is.na(test_data$thinking_mean),]$thinking_pred
test_data[is.na(test_data$difficult_mean),]$difficult_mean <- test_data[is.na(test_data$difficult_mean),]$difficult_pred
test_data[is.na(test_data$enjoy_mean),]$enjoy_mean <- test_data[is.na(test_data$enjoy_mean),]$enjoy_pred
test_data[is.na(test_data$clear_mean),]$clear_mean <- test_data[is.na(test_data$clear_mean),]$clear_pred
test_data[is.na(test_data$interesting_mean),]$interesting_mean <- test_data[is.na(test_data$interesting_mean),]$interesting_pred

test_data <- subset(test_data, select=-c(difficult_pred,thinking_pred,clear_pred,enjoy_pred,interesting_pred))

gbm_fit <- gbm(interesting ~ ., 
               cv.folds = 2,
               # weights = train_data$weights,
               n.trees = 50,
               data = train_data, 
               distribution = "multinomial",
               shrinkage = 0.1, 
               interaction.depth = 3, 
               n.minobsinnode = 10)

best.iter <- gbm.perf(gbm_fit,method="OOB")
best.iter <- gbm.perf(gbm_fit,method="cv")

par(las=2) # make label perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
summary(gbm_fit, cBars = 10)

# GETTING PREDICTIONS
gbm_predict_prob <- as.data.frame(predict.gbm(gbm_fit, test_data, n.trees=best.iter, type="response"))
gbm_predict <- cbind(test_data$obs,gbm_predict_prob)
colnames(gbm_predict) <- c("obs","interesting1","interesting2","interesting3","interesting4","interesting5")

write.table (gbm_predict, col.names=T, row.names=F, quote=F, sep=",", file="sumbission5.csv")

gbm_predict_guesses <- as.factor(max.col(gbm_predict_prob))

# GETTING CONFUSION MATRIX AND F-MEASURE
cm <- table(test_data$interesting, gbm_predict_guesses)
f_boost <- f_measure(cm)

# GETTING LOG LOSS VALUE
ll <- logLoss(gbm_predict_prob, test_data$interesting)

# SVM
library(e1071)
library(kernlab)

folds <- cut(seq(1,nrow(train_data)),breaks=4,labels=FALSE)
train_1 <- train_data[which(folds == 1),]
train_2 <- train_data[which(folds == 2),]
train_3 <- train_data[which(folds == 3),]
train_4 <- train_data[which(folds == 4),]
# train_5 <- train_data[which(folds == 5),]

svm_test <- svm(difficult_mean ~ ., data = test_data, kernel = "linear")
svm_pred <- predict(svm_test, sample_data)

svm_test <- ksvm(interesting_mean~.,
           data=test_data,
           type="C-bsvc",
           kernel=polydot(degree = 2),
           C=5,
           prob.model = TRUE)

s1 <- ksvm(interesting~.,
           data=train_1,
           type="C-bsvc",
           kernel=polydot(degree = 2),
           C=5,
           prob.model = TRUE)
s2 <- ksvm(interesting~.,
           data=train_2,
           type="C-bsvc",
           kernel=polydot(degree = 2),
           C=5,
           prob.model = TRUE)
s3 <- ksvm(interesting~.,
           data=train_3,
           type="C-bsvc",
           kernel=polydot(degree = 2),
           C=5,
           prob.model = TRUE)
s4 <- ksvm(interesting~.,
           data=train_4,
           type="C-bsvc",
           kernel=polydot(degree = 2),
           C=5,
           prob.model = TRUE)
# s1 <- ksvm(interesting~.,
#            data=train_1,
#            type="C-bsvc",
#            kernel=polydot(degree = 2),
#            C=5,
#            prob.model = TRUE)

svm_predict <- as.data.frame(predict(svm_test, test_data[,-66], type = "probabilities"))

svm_predict1 <- as.data.frame(predict(s1, test_set[,-2], type = "probabilities"))
svm_predict2 <- as.data.frame(predict(s2, test_set[,-2], type = "probabilities"))
svm_predict3 <- as.data.frame(predict(s3, test_set[,-2], type = "probabilities"))
svm_predict4 <- as.data.frame(predict(s4, test_set[,-2], type = "probabilities"))
# svm_predict5 <- predict(s5, test_set)

svm_predict <- svm_predict1
svm_predict[,1] <- (svm_predict1[,1] + svm_predict2[,1] + svm_predict3[,1])/3 # + svm_predict4$1)/4
svm_predict[,2] <- (svm_predict1[,2] + svm_predict2[,2] + svm_predict3[,2])/3 # + svm_predict4$2)/4
svm_predict[,3] <- (svm_predict1[,3] + svm_predict2[,3] + svm_predict3[,3])/3 # + svm_predict4$3)/4
svm_predict[,4] <- (svm_predict1[,4] + svm_predict2[,4] + svm_predict3[,4])/3 # + svm_predict4$4)/4
svm_predict[,5] <- (svm_predict1[,5] + svm_predict2[,5] + svm_predict3[,5])/3 # + svm_predict4$5)/4

table(test_set$interesting, svm_predict)

# NOTES 
# problem
# objective
# correlation - get rid of the redundant variables
# matching ids and predicting the missing variables 
# weekday weekend
# morning afternoon 
# log loss - figure out what this is and how to get it down 
# imputation in the caret package 
# upsampling the data 
# impute the means
# think of another way to get those columns in there
# f 0.53(imputed)
# ll 1.18(imputed), 1.2(floor), 1.17(round)

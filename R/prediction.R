library(MASS)
library(class)
library(plyr)
library(reshape2)

TRAIN_DATA_PATH="/Users/josephlugo/Google Drive/School/4B/STAT 441/Group Project/combined.csv"
TEST_DATA_PATH="/Users/josephlugo/Google Drive/School/4B/STAT 441/Group Project/test.csv"
AVARS_DATA_PATH="/Users/josephlugo/Google Drive/School/4B/STAT 441/Group Project/avars1.csv"
SAMPLE_DATA_PATH="/Users/josephlugo/Google Drive/School/4B/STAT 441/Group Project/sample_submission.csv"

sample_data <- read.csv(SAMPLE_DATA_PATH, 
                          header=TRUE, 
                          sep=",", 
                          stringsAsFactors=F)

original_data <- read.csv(TRAIN_DATA_PATH, 
                          header=TRUE, 
                          sep=",", 
                          stringsAsFactors=F)

test_data <- read.csv(TEST_DATA_PATH, 
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
  }

  data_frame$year <- as.factor(substr(data_frame$year_month_m,0,4))
  data_frame$month <- as.factor(substr(data_frame$year_month_m,5,6))
  data_frame[data_frame$core == "",]$core <- "leisure"
  data_frame$core <- as.factor(data_frame$core)
  data_frame$duration_cat <- 4
  data_frame[data_frame$duration <= 1643,]$duration_cat <- 3
  data_frame[data_frame$duration <= 1061,]$duration_cat <- 2
  data_frame[data_frame$duration <= 720,]$duration_cat <- 1
  data_frame$duration_cat <- as.factor(data_frame$duration_cat)

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

  end_times <- parse_datetimes(data_frame,":","endtime")

  data_frame$end_hr <- as.factor(as.numeric(end_times[[1]]))
  data_frame$end_min <- as.factor(as.numeric(end_times[[2]]))
  data_frame$end_sec <- as.factor(floor(as.numeric(end_times[[3]])))

  new_start <- unlist(lapply(1:nrow(data_frame), function(x) paste(start_dates[[1]][x],"-",start_dates[[2]][x],"-",start_dates[[3]][x],sep="")))
  new_end <- unlist(lapply(1:nrow(data_frame), function(x) paste(end_dates[[1]][x],"-",end_dates[[2]][x],"-",end_dates[[3]][x],sep="")))
  data_frame$startdate <- new_start
  data_frame$enddate <- new_end

  data_frame$startdate_epoch <- floor(as.integer(as.POSIXct(data_frame$startdate, format = "%d-%M-%Y"))/86400)
  data_frame$enddate_epoch <- floor(as.integer(as.POSIXct(data_frame$enddate, format = "%d-%M-%Y"))/86400)
  data_frame$sameday <- as.factor(as.integer(data_frame$startdate == data_frame$enddate))
  data_frame$delta_days <- data_frame$enddate_epoch - data_frame$startdate_epoch
  
  count_table <- as.data.frame(table(data_frame$id))
  colnames(count_table) <- c("id","num_surveys")
  data_frame <- merge(data_frame, count_table, all.x = TRUE,by = "id")
  
  core_table <- count(data_frame, c("id","core"))
  
  core_table_wide <- dcast(core_table,formula = id~core,value.var="freq")
  colnames(core_table_wide) <- c("id","num_health","num_income","num_leisure")
  core_table_wide[is.na(core_table_wide$num_health),]$num_health <- 0
  core_table_wide[is.na(core_table_wide$num_income),]$num_income <- 0
  core_table_wide[is.na(core_table_wide$num_leisure),]$num_leisure <- 0
  
  data_frame <- merge(data_frame, core_table_wide, all.x = TRUE,by = "id")
  
  duration_means <- ddply(data_frame,~id,summarise,mean=mean(duration))
  colnames(duration_means) <- c("id","duration_mean") 
  data_frame <- merge(data_frame, duration_means, all.x = TRUE,by = "id")

  factors <- c("gender","position","year_birth","age_member","age_cat","age_head","num_members", "num_children","partner","civil_status",
             "dom_sit","dwell_type","urban_char","occ", "gross_monthly_income_cat","net_monthly_income_cat", "edu","edu_diploma",
             "edu_cat","is_member","recruitment","origin","have_simPC")
         
  combined <- merge(x=data_frame,y=avars_data,by="id",all.x=TRUE)
  # combined <- merge(x=balanced,y=avars_data,by="id",all.x=TRUE)
  combined_clean <- combined
  # combined_clean$id <- NULL
  # combined_clean$interesting <- NULL
  combined_clean$difficult <- NULL
  combined_clean$clear <- NULL
  combined_clean$thinking <- NULL
  combined_clean$enjoy <- NULL
  combined_clean$train <- NULL
  # combined_clean$obs <- NULL
  combined_clean$year_month_m <- as.factor(combined_clean$year_month_m)
  combined_clean$startdate <- as.factor(combined_clean$startdate)
  combined_clean$enddate <- as.factor(combined_clean$enddate)
  combined_clean$starttime <- NULL
  combined_clean$endtime <- NULL
  combined_clean$core <- as.factor(combined_clean$core)
  # combined_clean$weights <- 1/5

  for (i in 1:length(factors)) {
    combined_clean[,factors[i]] <- as.factor(combined_clean[,factors[i]])
  }   

  return(combined_clean)

}

# think about grouping by id
# how many surveys have they done
# how many surveys have they done up to that point 
# average duration 
original_data <- cleanup(original_data, test=FALSE)
original_data$obs <- NULL
original_data$id <- NULL

test_data <- cleanup(test_data, test=TRUE)

# > table(original_data$interesting)

#     1     2     3     4     5 
#  4079  7748 30086 24117 14262 

one <- (original_data[original_data$interesting == 1,])
one_index <- sample(1:nrow(one), 4079)
one <- one[one_index,]

two <- (original_data[original_data$interesting == 2,])
two_index <- sample(1:nrow(two), 6000)
two <- two[two_index,]

three <- (original_data[original_data$interesting == 3,])
three_index <- sample(1:nrow(three), 6000)
three <- three[three_index,]

four <- (original_data[original_data$interesting == 4,])
four_index <- sample(1:nrow(four), 6000)
four <- four[four_index,]

five <- (original_data[original_data$interesting == 5,])
five_index <- sample(1:nrow(five), 6000)
five <- five[five_index,]

balanced <- rbind(one,two,three,four,five)	   

set.seed(1017)
train_percentage <- 0.8
train_index <- sample(1:nrow(original_data), floor(nrow(original_data)*train_percentage))
train_data <- original_data[train_index, ]
row.names(train_data) <- NULL

test_set <- original_data[-train_index, ]
test_index <- as.numeric(row.names(test_set))
row.names(test_set) <- NULL
			   
# Boosting
library(gbm)
library(caret)

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 2,
  ## repeated ten times
  repeats = 2)

gbmFit1 <- train(interesting ~ .,
                 data = train_data,
                 na.action = na.omit,
                 method = "gbm", 
                 trControl = fitControl,
                 metric = "Accuracy",
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = TRUE)

boosting_results <- resamples(gbm = gbmFit1)

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

gbm_predict <- as.data.frame(predict.gbm(gbm_fit, test_set, n.trees=best.iter, type="response"))
gbm_predict <- as.factor(max.col(gbm_predict))

table(test_set$interesting, gbm_predict)

library("mboost")

glm1 <- glmboost(interesting ~ ., data = original_data)

# SVM
library(e1071)

s1 <- svm(interesting ~ ., data = train_data, kernel = "linear", cost = 10)
svm_predict <- predict(s1, as.data.frame(test_set$interesting))

table(test_set$interesting, svm_predict)

# LDA
train_lda <- lda(interesting ~ year + month + sameday + norm_duration + core, train_data)
lda_predict <- predict(train_lda, original_data[-train_index, ])$class
lda_table <- table(test_set$interesting, lda_predict)

test_set$lda_pred <- lda_predict
lda_acc <- nrow(test_set[test_set$interesting == test_set$lda_pred,])/nrow(test_set)

# QDA
train_qda <- qda(interesting ~ year_month_m + difficult + clear + thinking + enjoy + duration + core, train_data)
qda_predict <- predict(train_qda, original_data[-train_index, ])$class
qda_table <- table(test_set$interesting, lda_predict)

test_set$qda_pred <- qda_predict
qda_acc <- nrow(test_set[test_set$interesting == test_set$qda_pred,])/nrow(test_set)

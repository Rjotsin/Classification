library(MASS)
library(class)
library(plyr)
library(reshape2)
library(dplyr)
library(tidyr)
library(chron)
library(caret)
library(e1071)
library(gbm)
library(MLmetrics)
library(randomForest)
library(foreach)
library(parallel)
library(doParallel)
library(nnet)
library(RCurl)
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
  
  new_start <- unlist(lapply(1:nrow(data_frame), function(x) paste(start_dates[[1]][x],"-",start_dates[[2]][x],"-",start_dates[[3]][x],sep="")))
  new_end <- unlist(lapply(1:nrow(data_frame), function(x) paste(end_dates[[1]][x],"-",end_dates[[2]][x],"-",end_dates[[3]][x],sep="")))
  data_frame$startdate <- new_start
  data_frame$enddate <- new_end
  
  data_frame$startdate_epoch <- floor(as.integer(as.POSIXct(data_frame$startdate, format = "%d-%M-%Y"))/86400)
  data_frame$enddate_epoch <- floor(as.integer(as.POSIXct(data_frame$enddate, format = "%d-%M-%Y"))/86400)
  
  count_table <- as.data.frame(table(data_frame$id))
  colnames(count_table) <- c("id","num_surveys")
  data_frame <- merge(data_frame, count_table, all.x = TRUE,by = "id")
  
  core_table <- as.data.frame(table(data_frame$id,data_frame$core))

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
  data_frame_enjoy <- data_frame %>% group_by(id) %>% summarise(enjoy_mean=round(mean(as.numeric(enjoy))))
  data_frame_difficult <- data_frame %>% group_by(id) %>% summarise(difficult_mean=round(mean(as.numeric(difficult))))
  data_frame_thinking <- data_frame %>% group_by(id) %>% summarise(thinking_mean=round(mean(as.numeric(thinking))))
  data_frame_clear <- data_frame %>% group_by(id) %>% summarise(clear_mean=round(mean(as.numeric(clear))))
  data_frame_interesting <- data_frame %>% group_by(id) %>% summarise(interesting_mean=round(mean(as.numeric(interesting))))
  
  data_frame <- merge(x=data_frame,y=data_frame_enjoy,by="id")
  data_frame <- merge(x=data_frame,y=data_frame_difficult,by="id")
  data_frame <- merge(x=data_frame,y=data_frame_thinking,by="id")
  data_frame <- merge(x=data_frame,y=data_frame_clear,by="id")
  data_frame <- merge(x=data_frame,y=data_frame_interesting,by="id")
  
  data_frame$enjoy_mean <- as.factor(data_frame$enjoy_mean)
  data_frame$difficult_mean <- as.factor(data_frame$difficult_mean)
  data_frame$thinking_mean <- as.factor(data_frame$thinking_mean)
  data_frame$interesting_mean <- as.factor(data_frame$interesting_mean)
  data_frame$clear_mean <- as.factor(data_frame$clear_mean)
  
  data_frame <- subset(data_frame, select=-c(enjoy,thinking,clear,difficult))
  
  return(data_frame)
}


subset_train <- function(data_frame){
  # TESTING ON OUR KNOWN DATA
  
  train_percentage <- 0.8
  train_index <- sample(1:nrow(data_frame), floor(nrow(data_frame)*train_percentage))
  train_data <- data_frame[train_index, ]
  row.names(train_data) <- NULL
  
  test_set <- data_frame[-train_index, ]
  test_index <- as.numeric(row.names(test_set))
  row.names(test_set) <- NULL
  
  train_data <- get_means(train_data)
  train_data$id <- as.factor(train_data$id)
  
  test_set$id <- as.factor(test_set$id)
  test_set <- subset(test_set, select=-c(enjoy,thinking,clear,difficult))
  test_set <- merge(x=test_set, 
                    y=unique(train_data[,c("id","thinking_mean","enjoy_mean","difficult_mean","clear_mean","interesting_mean")]),
                    by="id",
                    all.x=TRUE)
  
  data <- list(train_data, test_set)
  
  return(data)
}

scale_data <- function(data_frame, test = FALSE)
{
  data_frame$duration <- log(data_frame$duration)
  data_frame$gross_monthly_income_imputed <- log(data_frame$gross_monthly_income_imputed + 1)
  data_frame$net_monthly_income_imputed <- log(data_frame$net_monthly_income_imputed + 1)
  data_frame$gross_household_income <- log(data_frame$gross_household_income + 1)
  data_frame$net_household_income <- log(data_frame$net_household_income + 1)
  
  nums_og_train <- sapply(data_frame, is.numeric)
  ints_og_train <- sapply(data_frame, is.integer)
  
  if(test == TRUE){
    id <- data_frame$id
    obs <- data_frame$obs
    data_frame[,nums_og_train | ints_og_train] <- scale(data_frame[,nums_og_train | ints_og_train])
    data_frame$id <- id
    data_frame$obs <- obs
  }
  
  else{
    id <- data_frame$id
    obs <- data_frame$obs
    data_frame[,nums_og_train | ints_og_train] <- scale(data_frame[,nums_og_train | ints_og_train])
    data_frame$id <- id
    data_frame$obs <- obs
  }
  
  return(data_frame)
}

# CLEANING UP THE TRAIN_DATA
# think about grouping by id
# how many surveys have they done
# how many surveys have they done up to that point 
# average duration 
train_data <- cleanup(original_train_data, test=FALSE)

trainlist <- subset_train(train_data)

newtrain <- trainlist[[1]]

newtest <- trainlist[[2]]

train_data <- get_means(train_data)

train_data <- scale_data(train_data,test=FALSE)

# CLEANING UP THE TEST_DATA
test_data <- cleanup(original_test_data, test=TRUE)
test_data <- merge(x=test_data, 
                   y=unique(train_data[,c("id","thinking_mean","enjoy_mean","difficult_mean","clear_mean","interesting_mean")]),
                   by="id",
                   all.x=TRUE)

test_data <- scale_data(test_data, test=TRUE)

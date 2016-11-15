library(randomForest)
library(chron)
setwd("U:/STAT441/Project/Data")
set.seed(100)

original_data <- read.csv(file="combined.csv", na.strings=c("","NA"," "), stringsAsFactors = FALSE)
test_data <- read.csv(file="test.csv", header=T,  na.strings=c("","NA"," "), stringsAsFactors = FALSE)
avars_data <- read.csv(file="avars1.csv", header=T,  na.strings=c("","NA"," "), stringsAsFactors = FALSE)

colnames(avars_data) <- c("id","gender","position","year_birth","age_member","age_cat","age_head","num_members",
                          "num_children","partner","civil_status","dom_sit","dwell_type","urban_char","occ","gross_monthly_income",
                          "gross_monthly_income_imputed","net_monthly_income","net_monthly_income_capped","net_monthly_income_imputed",
                          "gross_monthly_income_cat","net_monthly_income_cat","gross_household_income","net_household_income",
                          "edu","edu_diploma","edu_cat","is_member","recruitment","origin","have_simPC")

factors <- c("year_month_m", "startdate", "starttime", "enddate", "endtime", "core", "year","gender","position","year_birth","age_member","age_cat","age_head","num_members", "num_children","partner","civil_status",
             "dom_sit","dwell_type","urban_char","occ", "gross_monthly_income_cat","net_monthly_income_cat", "edu","edu_diploma",
             "edu_cat","is_member","recruitment","origin","have_simPC")

original_data$year <- substr(original_data$year_month_m,0,4)
test_data$year <- substr(test_data$year_month_m,0,4)

original_data$interesting <- as.factor(original_data$interesting)

parse_datetimes <- function(delimeter,col){
  
  splits <- strsplit(original_data[,c(col)],delimeter)
  
  split_1 <- unlist(lapply(1:nrow(original_data), function(x) paste(splits[[x]][1],sep="")))
  split_2 <- unlist(lapply(1:nrow(original_data), function(x) paste(splits[[x]][2],sep="")))
  split_3 <- unlist(lapply(1:nrow(original_data), function(x) paste(splits[[x]][3],sep="")))
  
  output <- list(c(split_1), c(split_2), c(split_3))
  
  return(output)
  
}

start_dates <- parse_datetimes("-", "startdate")

original_data$start_day <- as.factor(as.numeric(start_dates[[1]]))
original_data$start_month <- as.factor(as.numeric(start_dates[[2]]))
original_data$start_year <- as.factor(as.numeric(start_dates[[3]]))

end_dates <- parse_datetimes("-", "enddate")

original_data$end_day <- as.factor(as.numeric(end_dates[[1]]))
original_data$end_month <- as.factor(as.numeric(end_dates[[2]]))
original_data$end_year <- as.factor(as.numeric(end_dates[[3]]))

start_times <-parse_datetimes(":","starttime")

original_data$start_hr <- as.factor(as.numeric(start_times[[1]]))
original_data$start_min <- as.factor(as.numeric(start_times[[2]]))
original_data$start_sec <- as.factor(floor(as.numeric(start_times[[3]])))

end_times <- parse_datetimes(":","endtime")

original_data$end_hr <- as.factor(as.numeric(end_times[[1]]))
original_data$end_min <- as.factor(as.numeric(end_times[[2]]))
original_data$end_sec <- as.factor(floor(as.numeric(end_times[[3]])))

parse_datetimes2 <- function(delimeter,col){
  
  splits <- strsplit(test_data[,c(col)],delimeter)
  
  split_1 <- unlist(lapply(1:nrow(test_data), function(x) paste(splits[[x]][1],sep="")))
  split_2 <- unlist(lapply(1:nrow(test_data), function(x) paste(splits[[x]][2],sep="")))
  split_3 <- unlist(lapply(1:nrow(test_data), function(x) paste(splits[[x]][3],sep="")))
  
  output <- list(c(split_1), c(split_2), c(split_3))
  
  return(output)
  
}

start_dates <- parse_datetimes2("-", "startdate")

test_data$start_day <- as.factor(as.numeric(start_dates[[1]]))
test_data$start_month <- as.factor(as.numeric(start_dates[[2]]))
test_data$start_year <- as.factor(as.numeric(start_dates[[3]]))

end_dates <- parse_datetimes2("-", "enddate")

test_data$end_day <- as.factor(as.numeric(end_dates[[1]]))
test_data$end_month <- as.factor(as.numeric(end_dates[[2]]))
test_data$end_year <- as.factor(as.numeric(end_dates[[3]]))

start_times <-parse_datetimes2(":","starttime")

test_data$start_hr <- as.factor(as.numeric(start_times[[1]]))
test_data$start_min <- as.factor(as.numeric(start_times[[2]]))
test_data$start_sec <- as.factor(floor(as.numeric(start_times[[3]])))

end_times <- parse_datetimes2(":","endtime")

test_data$end_hr <- as.factor(as.numeric(end_times[[1]]))
test_data$end_min <- as.factor(as.numeric(end_times[[2]]))
test_data$end_sec <- as.factor(floor(as.numeric(end_times[[3]])))

start <- strsplit(original_data$startdate,"-")
end <- strsplit(original_data$enddate,"-")
new_start <- unlist(lapply(1:nrow(original_data), function(x) paste(start[[x]][1],"-",start[[x]][2],"-",original_data[x,]$year,sep="")))
new_end <- unlist(lapply(1:nrow(original_data), function(x) paste(end[[x]][1],"-",end[[x]][2],"-",original_data[x,]$year,sep="")))
original_data$startdate <- new_start
original_data$enddate <- new_end

start <- strsplit(test_data$startdate,"-")
end <- strsplit(test_data$enddate,"-")
new_start <- unlist(lapply(1:nrow(test_data), function(x) paste(start[[x]][1],"-",start[[x]][2],"-",test_data[x,]$year,sep="")))
new_end <- unlist(lapply(1:nrow(test_data), function(x) paste(end[[x]][1],"-",end[[x]][2],"-",test_data[x,]$year,sep="")))
test_data$startdate <- new_start
test_data$enddate <- new_end

original_data$startdate <- floor(as.integer(as.POSIXct(original_data$startdate, format = "%d-%M-%Y"))/86400)
test_data$startdate <- floor(as.integer(as.POSIXct(test_data$startdate, format = "%d-%M-%Y"))/86400)

original_data$enddate <- floor(as.integer(as.POSIXct(original_data$enddate, format = "%d-%M-%Y"))/86400)
test_data$enddate <- floor(as.integer(as.POSIXct(test_data$enddate, format = "%d-%M-%Y"))/86400)

combined <- merge(x=original_data,y=avars_data,by="id",all.x=TRUE)
combined.test <- merge(x=test_data,y=avars_data,by="id",all.x=TRUE)

combined$id <- NULL
combined$train <- NULL
combined$obs <- NULL
combined$difficult <- NULL
combined$clear <- NULL
combined$thinking <- NULL
combined$enjoy <- NULL

combined.test$id <- NULL
combined.test$train <- NULL
combined.test$obs <- NULL

for (i in 1:length(factors)) {
  combined[,factors[i]] <- as.factor(combined[,factors[i]])
}

for (i in 1:length(factors)) {
  combined.test[,factors[i]] <- as.factor(combined.test[,factors[i]])
}

remove = c()
for (i in 1:ncol(combined)) {
  if (length(unique(combined[,i])) > 54) {
    remove <- c(remove,i)
  }
}

combined<- combined[,-remove]

remove = c()
for (i in 1:ncol(combined.test)) {
  if (length(unique(combined.test[,i])) > 54) {
    remove <- c(remove,i)
  }
}

combined.test<- combined.test[,-remove]

interesting <- combined$interesting
combined$interesting <- NULL
combined <- cbind(combined,interesting)

for (i in 1:ncol(combined.test)) {
  levels(combined.test[,i]) <- levels(combined[,i])
}

rf <-randomForest(interesting ~.,data=combined, importance=TRUE, na.action = na.roughfix, ntree = 10)

predrf <- predict(rf, newdata = combined.test, type = "prob", na.action = na.roughfix)

oritest <- read.csv(file="test + avars1.csv", na.strings=c("","NA"," "), stringsAsFactors=TRUE)

rfres <- as.data.frame(predrf)

rfres <- cbind(oritest$obs, rfres)


write.table(predrf, file = "rf test.csv", col.names = c("obs","interesting1","interesting2","interesting3","interesting4","interesting5"))

str(combined)
str(combined.test)

library(MASS)
library(class)

TRAIN_DATA_PATH="/Users/josephlugo/Google Drive/School/4B/STAT 441/Group Project/combined.csv"
TEST_DATA_PATH="/Users/josephlugo/Google Drive/School/4B/STAT 441/Group Project/test.csv"
AVARS_DATA_PATH="/Users/josephlugo/Google Drive/School/4B/STAT 441/Group Project/avars1.csv"

original_data <- read.csv(TRAIN_DATA_PATH, 
                          header=TRUE, 
                          sep=",", 
                          stringsAsFactors=F)

original_data$interesting <- as.factor(original_data$interesting)
original_data$difficult <- as.factor(original_data$difficult)
original_data$clear <- as.factor(original_data$clear)
original_data$thinking <- as.factor(original_data$thinking)
original_data$enjoy <- as.factor(original_data$enjoy)
original_data <- na.omit(original_data)
original_data$year <- as.factor(substr(original_data$year_month_m,0,4))
original_data$month <- as.factor(substr(original_data$year_month_m,5,6))
original_data$core <- as.factor(original_data$core)
original_data$duration_cat <- 4
original_data[original_data$duration <= 1643,]$duration_cat <- 3
original_data[original_data$duration <= 1061,]$duration_cat <- 2
original_data[original_data$duration <= 720,]$duration_cat <- 1
original_data$duration_cat <- as.factor(original_data$duration_cat)



# start <- strsplit(original_data$startdate,"-")
# end <- strsplit(original_data$enddate,"-")
# new_start <- unlist(lapply(1:nrow(original_data), function(x) paste(start[[x]][1],"-",start[[x]][2],"-",original_data[x,]$year,sep="")))
# new_end <- unlist(lapply(1:nrow(original_data), function(x) paste(end[[x]][1],"-",end[[x]][2],"-",original_data[x,]$year,sep="")))
# original_data$startdate <- new_start
# original_data$enddate <- new_end

# original_data$startdate <- floor(as.integer(as.POSIXct(original_data$startdate, format = "%d-%M-%Y"))/86400)
#original_data$enddate <- floor(as.integer(as.POSIXct(original_data$enddate, format = "%d-%M-%Y"))/86400)
original_data$sameday <- as.factor(as.integer(original_data$startdate == original_data$enddate))
#original_data$norm_duration <- scale(original_data$duration)

set.seed(1005)
train_percentage <- 0.8
train_index <- sample(1:nrow(original_data), floor(nrow(original_data)*train_percentage))
train_data <- original_data[train_index, ]
row.names(train_data) <- NULL

test_set <- original_data[-train_index, ]
test_index <- as.numeric(row.names(test_set))
row.names(test_set) <- NULL

d <- density(original_data$duration)
plot(d, main="Duration Distribution",xlab="Duration")
polygon(d, col="red", border="blue")

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

# avars_data[(avars_data$gross_monthly_income == -15) | (avars_data$gross_monthly_income == -13),]$gross_monthly_income <- NA
# avars_data[(avars_data$net_monthly_income == -14),]$net_monthly_income <- NA
# avars_data[(avars_data$net_monthly_income_capped == -15) | (avars_data$net_monthly_income_capped == -13),]$net_monthly_income_capped <- NA

combined <- merge(x=original_data,y=avars_data,by="id",all.x=TRUE)
combined_clean <- combined
combined_clean$id <- NULL
# combined_clean$interesting <- NULL
combined_clean$difficult <- NULL
combined_clean$clear <- NULL
combined_clean$thinking <- NULL
combined_clean$enjoy <- NULL
combined_clean$train <- NULL
combined_clean$obs <- NULL
combined_clean$startdate <- as.factor(combined_clean$startdate)
combined_clean$starttime <- NULL
combined_clean$enddate <- as.factor(combined_clean$enddate)
combined_clean$endtime <- NULL
combined_clean$core <- as.factor(combined_clean$core)
combined_clean$weights <- 1/5

# Boosting
set.seed(1005)
train_percentage <- 0.8
train_index <- sample(1:nrow(combined_clean), floor(nrow(combined_clean)*train_percentage))
train_data <- combined_clean[train_index, ]
row.names(train_data) <- NULL

test_set <- combined_clean[-train_index, ]
test_index <- as.numeric(row.names(test_set))
row.names(test_set) <- NULL

library(gbm)
library(caret)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 2,
  ## repeated ten times
  repeats = 2)

gbmFit1 <- train(interesting ~ .,
                 data = test_set,
                 na.action = na.omit,
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = TRUE)

p <- ncol(combined_clean)
gbm_fit <- gbm(interesting ~.,
               #weights = train_data$weights,
               n.trees = 150,
               data=train_data, 
               distribution = "multinomial",
               shrinkage = 0.01, 
               interaction.depth = 3, 
               bag.fraction = 0.5,
               n.minobsinnode = 10)

gbm_predict <- as.data.frame(predict.gbm(gbm_fit, test_set,n.trees=150, type="response"))
gbm_predict <- as.factor(max.col(gbm_predict))

table(test_set$interesting, gbm_predict)

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

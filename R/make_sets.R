MakeSets <- function(train.df, test.df) {
  
  obs <- train.df$obs
  
  train.df$startdate <- NULL
  train.df$enddate <- NULL
  train.df$id <- NULL
  
  test.df$startdate <- NULL
  test.df$enddate <- NULL
  test.df$id <- NULL
  
  interesting <- as.integer(train.df$interesting)
  
  train.df$interesting <- NULL
  
  for (i in 1:length(interesting)) {
    interesting[i] = paste("interesting",interesting[i], sep = "")
  }
  
  interesting <- as.factor(interesting)
  
  train.df <- cbind(train.df, interesting)
  
  train1 <- train.df[complete.cases(train.df),]
  
  train2 <- train.df[ , !(names(train.df) %in% c("gross_monthly_income_imputed","net_monthly_income_imputed",
                                                         "gross_monthly_income_cat", "net_monthly_income_cat",
                                                         "gross_household_income","net_household_income"))]
  
  train2 <- train2[complete.cases(train2),]
  
  
  train3 <- train.df[ , !(names(train.df) %in% c("gender","position","year_birth","age_member","age_cat",
                                                         "age_head","num_members", "num_children","partner","civil_status",
                                                         "dom_sit","dwell_type","urban_char","occ",
                                                         "gross_monthly_income_imputed","net_monthly_income_imputed",
                                                         "gross_monthly_income_cat","net_monthly_income_cat",
                                                         "gross_household_income","net_household_income","edu",
                                                         "edu_diploma","edu_cat","is_member","recruitment","origin",
                                                         "have_simPC"))]
  
  train3 <- train3[complete.cases(train3),]
  
  train4 <- train.df[ , !(names(train.df) %in% c("gender","position","year_birth","age_member","age_cat",
                                                 "age_head","num_members", "num_children","partner","civil_status",
                                                 "dom_sit","dwell_type","urban_char","occ",
                                                 "gross_monthly_income_imputed","net_monthly_income_imputed",
                                                 "gross_monthly_income_cat","net_monthly_income_cat",
                                                 "gross_household_income","net_household_income","edu",
                                                 "edu_diploma","edu_cat","is_member","recruitment","origin",
                                                 "have_simPC","interesting_mean","enjoy_mean","difficult_mean",
                                                 "thinking_mean","clear_mean"))]
  
  train4 <- train4[complete.cases(train4),]
  
  train1$obs <- NULL
  train2$obs <- NULL
  train3$obs <- NULL
  train4$obs <- NULL
  
  ####################################################################################################################################
  
  completeFun <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
  }
  
  test1 <- test.df[complete.cases(test.df),]
  
  test2 <- test.df[ , !(names(test.df) %in% c("gross_monthly_income_imputed","net_monthly_income_imputed",
                                                 "gross_monthly_income_cat", "net_monthly_income_cat",
                                                 "gross_household_income","net_household_income"))]
  
  test2 <- test2[complete.cases(test2),]
  
  test2 <- test2[ ! test2$obs %in% unique(c(test1$obs)),]
  
  
  test3 <- test.df[ , !(names(test.df) %in% c("gender","position","year_birth","age_member","age_cat",
                                                 "age_head","num_members", "num_children","partner","civil_status",
                                                 "dom_sit","dwell_type","urban_char","occ",
                                                 "gross_monthly_income_imputed","net_monthly_income_imputed",
                                                 "gross_monthly_income_cat","net_monthly_income_cat",
                                                 "gross_household_income","net_household_income","edu",
                                                 "edu_diploma","edu_cat","is_member","recruitment","origin",
                                                 "have_simPC"))]
  
  test3 <- test3[complete.cases(test3),]
  
  test3 <- test3[ ! test3$obs %in% unique(c(test1$obs, test2$obs)),]
  
  test4 <- test.df[ , !(names(test.df) %in% c("gender","position","year_birth","age_member","age_cat",
                                                 "age_head","num_members", "num_children","partner","civil_status",
                                                 "dom_sit","dwell_type","urban_char","occ",
                                                 "gross_monthly_income_imputed","net_monthly_income_imputed",
                                                 "gross_monthly_income_cat","net_monthly_income_cat",
                                                 "gross_household_income","net_household_income","edu",
                                                 "edu_diploma","edu_cat","is_member","recruitment","origin",
                                                 "have_simPC","interesting_mean","enjoy_mean","difficult_mean",
                                                 "thinking_mean","clear_mean"))]
  
  test4 <- test4[complete.cases(test4),]
  
  test4 <- test4[ ! test.df$obs %in% unique(c(test1$obs,test2$obs,test3$obs)),]

  train_list <- list(train1, train2, train3, train4)
  test_list <- list(test1, test2, test3, test4)

  return(list(train_list, test_list))

}

all_sets_split <- MakeSets(newtrain, newtest)
train_list_split <- all_sets_split[[1]]
test_list_split <- all_sets_split[[2]]

all_sets <- MakeSets(train_data, test_data)
train_list <- all_sets[[1]]
test_list <- all_sets[[2]]

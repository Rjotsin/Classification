# PREDICTION FUNCTIONS FOR GBM, MN, RF
# input: list of training data, list of test data
# output: list of probability data frames for each subset and combined probability data frame

gbm_preds <- function(train_list,test_list)
    {
        # GBM
        library(gbm)

        gbm_fit1 <- gbm(interesting ~ ., 
                        cv.folds = 2,
                        n.trees = 90,
                        data = train_list[[1]], 
                        distribution = "multinomial",
                        shrinkage = 0.1, 
                        interaction.depth = 3, 
                        n.minobsinnode = 10)
        best.iter1 <- gbm.perf(gbm_fit1,method="OOB")
        best.iter1 <- gbm.perf(gbm_fit1,method="cv")

        gbm_fit2 <- gbm(interesting ~ ., 
                        cv.folds = 2,
                        n.trees = 100,
                        data = train_list[[2]], 
                        distribution = "multinomial",
                        shrinkage = 0.1, 
                        interaction.depth = 3, 
                        n.minobsinnode = 10)
        best.iter2 <- gbm.perf(gbm_fit2,method="OOB")
        best.iter2 <- gbm.perf(gbm_fit2,method="cv")

        gbm_fit3 <- gbm(interesting ~ ., 
                        cv.folds = 2,
                        n.trees = 120,
                        data = train_list[[3]], 
                        distribution = "multinomial",
                        shrinkage = 0.1, 
                        interaction.depth = 3, 
                        n.minobsinnode = 10)
        best.iter3 <- gbm.perf(gbm_fit3,method="OOB")
        best.iter3 <- gbm.perf(gbm_fit3,method="cv")

        gbm_fit4 <- gbm(interesting ~ ., 
                        cv.folds = 2,
                        n.trees = 50,
                        data = train_list[[4]], 
                        distribution = "multinomial",
                        shrinkage = 0.1, 
                        interaction.depth = 3, 
                        n.minobsinnode = 10)
        best.iter4 <- gbm.perf(gbm_fit4,method="OOB")
        best.iter4 <- gbm.perf(gbm_fit4,method="cv")

        gbm_predict_prob1 <- as.data.frame(predict.gbm(gbm_fit1, test_list[[1]], n.trees=best.iter1, type="response"))
        gbm_predict_prob1 <- cbind(test_list[[1]]$obs, gbm_predict_prob1)
        gbm_predict_prob2 <- as.data.frame(predict.gbm(gbm_fit2, test_list[[2]], n.trees=best.iter2, type="response"))
        gbm_predict_prob2 <- cbind(test_list[[2]]$obs, gbm_predict_prob2)
        gbm_predict_prob3 <- as.data.frame(predict.gbm(gbm_fit3, test_list[[3]], n.trees=best.iter3, type="response"))
        gbm_predict_prob3 <- cbind(test_list[[3]]$obs, gbm_predict_prob3)
        gbm_predict_prob4 <- as.data.frame(predict.gbm(gbm_fit4, test_list[[4]], n.trees=best.iter4, type="response"))
        gbm_predict_prob4 <- cbind(test_list[[4]]$obs, gbm_predict_prob4)

        colnames(gbm_predict_prob1) <- c("obs","interesting1","interesting2","interesting3","interesting4","interesting5")
        colnames(gbm_predict_prob2) <- c("obs","interesting1","interesting2","interesting3","interesting4","interesting5")
        colnames(gbm_predict_prob3) <- c("obs","interesting1","interesting2","interesting3","interesting4","interesting5")
        colnames(gbm_predict_prob4) <- c("obs","interesting1","interesting2","interesting3","interesting4","interesting5")

        gbm_predict_prob_final <- rbind(gbm_predict_prob1,gbm_predict_prob2,gbm_predict_prob3,gbm_predict_prob4)

        pred_prob_list <- list(gbm_predict_prob1,gbm_predict_prob2,gbm_predict_prob3,gbm_predict_prob4)

        return(list(pred_prob_list, gbm_predict_prob_final))
}

gbm_results_split <- gbm_preds(train_list_split, test_list_split)
write.table (gbm_results_split[[1]][1], col.names=T, row.names=F, quote=F, sep=",", file="gbm_prob_split1.csv")
write.table (gbm_results_split[[1]][2], col.names=T, row.names=F, quote=F, sep=",", file="gbm_prob_split2.csv")
write.table (gbm_results_split[[1]][3], col.names=T, row.names=F, quote=F, sep=",", file="gbm_prob_split3.csv")
write.table (gbm_results_split[[1]][4], col.names=T, row.names=F, quote=F, sep=",", file="gbm_prob_split4.csv")
write.table (gbm_results_split[[2]], col.names=T, row.names=F, quote=F, sep=",", file="gbm_prob_split_final.csv")

gbm_results <- gbm_preds(train_list, test_list)
write.table (gbm_results[[1]][1], col.names=T, row.names=F, quote=F, sep=",", file="gbm_prob1.csv")
write.table (gbm_results[[1]][2], col.names=T, row.names=F, quote=F, sep=",", file="gbm_prob2.csv")
write.table (gbm_results[[1]][3], col.names=T, row.names=F, quote=F, sep=",", file="gbm_prob3.csv")
write.table (gbm_results[[1]][4], col.names=T, row.names=F, quote=F, sep=",", file="gbm_prob4.csv")
write.table (gbm_results[[2]], col.names=T, row.names=F, quote=F, sep=",", file="gbm_prob_final.csv")

multnom_preds <- function(train_list, test_list)
    {
        # MULTINOMIAL
        remove = c()
        for (i in 1:ncol(train_list[[1]])) {
          if (is.factor(train_list[[1]][,i])){
            if (length(unique(train_list[[1]][,i])) > 23) {
              remove <- c(remove,i)
            }
          }
        }

        train1_mult<- train_list[[1]][,-remove]

        remove = c()
        for (i in 1:ncol(train_list[[2]])) {
          if (is.factor(train_list[[2]][,i])){
            if (length(unique(train_list[[2]][,i])) > 23) {
              remove <- c(remove,i)
            }
          }
        }

        train2_mult <- train_list[[2]][,-remove]

        remove = c()
        for (i in 1:ncol(train_list[[3]])) {
          if (is.factor(train_list[[3]][,i])){
            if (length(unique(train_list[[3]][,i])) > 23) {
              remove <- c(remove,i)
            }
          }
        }

        train3_mult <- train_list[[3]][,-remove]

        remove = c()
        for (i in 1:ncol(train_list[[4]])) {
          if (is.factor(train_list[[4]][,i])){
            if (length(unique(train_list[[4]][,i])) > 23) {
              remove <- c(remove,i)
            }
          }
        }

        train4_mult <- train_list[[4]][,-remove]

        train1_mult$year_month_m <- NULL
        train2_mult$year_month_m <- NULL
        train3_mult$year_month_m <- NULL
        train4_mult$year_month_m <- NULL

        control <- trainControl(method="none", classProbs = TRUE, summaryFunction = multiClassSummary, allowParallel=TRUE)

        model1 <- train(interesting ~ ., data=train1_mult, method="multinom", trControl=control, tuneGrid = data.frame(decay = 0.1), maxit = 500)
        model2 <- train(interesting ~ ., data=train2_mult, method="multinom", trControl=control, tuneGrid = data.frame(decay = 0.1), maxit = 500)
        model3 <- train(interesting ~ ., data=train3_mult, method="multinom", trControl=control, tuneGrid = data.frame(decay = 0.1), maxit = 500)
        model4 <- train(interesting ~ ., data=train4_mult, method="multinom", trControl=control, tuneGrid = data.frame(decay = 0.1), maxit = 500)

        preds1 <- predict(model1, type = 'prob', newdata = test_list[[1]], na.action = na.roughfix)
        preds2 <- predict(model2, type = 'prob', newdata = test_list[[2]], na.action = na.roughfix)
        preds3 <- predict(model3, type = 'prob', newdata = test_list[[3]], na.action = na.roughfix)
        preds4 <- predict(model4, type = 'prob', newdata = test_list[[4]], na.action = na.roughfix)

        obs <- test_list[[1]]$obs
        preds1 <- as.data.frame(cbind(obs, preds1))
        obs <- test_list[[2]]$obs
        preds2 <- as.data.frame(cbind(obs, preds2))
        obs <- test_list[[3]]$obs
        preds3 <- as.data.frame(cbind(obs, preds3))
        obs <- test_list[[4]]$obs
        preds4 <- as.data.frame(cbind(obs, preds4))

        mult_predict_final <- rbind(preds1,preds2,preds3,preds4)

        multnom_prob_list <- list(preds1,preds2,preds3,preds4)

        return(list(multnom_prob_list, mult_predict_final))

    }

mn_results <- multnom_preds(train_list_split, test_list_split)

mn_results <- multnom_preds(train_list, test_list)

rf_preds <- function(train_list, test_list)
    {
        # RANDOM FOREST
        library(doMC)
        cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
        registerDoParallel(cluster)
        getDoParWorkers()
          
        control <- trainControl(method="none", classProbs = TRUE, summaryFunction = multiClassSummary, allowParallel=TRUE)
          
        fit.rf.1 <- train(interesting~., data=train_list[[1]], method="parRF", trControl=control, tuneGrid = data.frame(mtry = 39))
        fit.rf.2 <- train(interesting~., data=train_list[[2]], method="parRF", trControl=control, tuneGrid = data.frame(mtry = 31))
        fit.rf.3 <- train(interesting~., data=train_list[[3]], method="parRF", trControl=control, tuneGrid = data.frame(mtry = 31))
        fit.rf.4 <- train(interesting~., data=train_list[[4]], method="parRF", trControl=control, tuneGrid = data.frame(mtry = 39))

        predrf1 <- predict(fit.rf.1, newdata = test_list[[1]], type = "prob")
        predrf2 <- predict(fit.rf.2, newdata = test_list[[2]], type = "prob")
        predrf3 <- predict(fit.rf.3, newdata = test_list[[3]], type = "prob")
        predrf4 <- predict(fit.rf.4, newdata = test_list[[4]], type = "prob")

        rftest1 <- as.data.frame(predrf1)
        rftest1 <- cbind(test_list[[1]]$obs, rftest1)

        rftest2 <- as.data.frame(predrf2)
        rftest2 <- cbind(test_list[[2]]$obs, rftest2)

        rftest3 <- as.data.frame(predrf3)
        rftest3 <- cbind(test_list[[3]]$obs, rftest3)

        rftest4 <- as.data.frame(predrf4)
        rftest4 <- cbind(test_list[[4]]$obs, rftest4)

        colnames(rftest1) <- c("obs","interesting1","interesting2","interesting3","interesting4","interesting5")
        colnames(rftest2) <- c("obs","interesting1","interesting2","interesting3","interesting4","interesting5")
        colnames(rftest3) <- c("obs","interesting1","interesting2","interesting3","interesting4","interesting5")
        colnames(rftest4) <- c("obs","interesting1","interesting2","interesting3","interesting4","interesting5")

        rfreults <- rbind(rftest1,rftest2,rftest3,rftest4)

        rf_prob_list <- list(rftest1,rftest2,rftest3,rftest4)

        return(list(rf_prob_list, rfreults))

    }
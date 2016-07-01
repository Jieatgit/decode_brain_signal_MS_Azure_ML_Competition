
    erp_train = f3_train
    erp_valid = f3_valid

    # parameter search with cerat
    set.seed(10)
    ncols   <- ncol(erp_train)
    good_col <- (erp_train[1,]!=0)
    good_col[1] <- FALSE
    good_col[67] <- FALSE
    erp_train$Stimulus_Type <- as.factor(erp_train$Stimulus_Type)
    erp_valid$Stimulus_Type <- as.factor(erp_valid$Stimulus_Type)
    erp_train <- erp_train[,good_col]
    erp_valid <- erp_valid[,good_col]

    gbmGrid <- expand.grid(interaction.depth = c(1:4),
                         n.trees = seq(500,2000,by=200),
                         shrinkage = c( 0.001*(1:5), 0.01),
                         n.minobsinnode = 10)
    gbmCtr  <- trainControl(method = "cv", number = 5, repeats = 1)
    gbmFit1 <- train(as.factor(Stimulus_Type) ~ ., 
                   data = erp_train,
                   method = "gbm", distribution = "multinomial",
                   trControl = gbmCtr,
                   tuneGrid = gbmGrid)
    gbmFit1
    gbmFit1$bestTune
    plot(gbmFit1)
    res <- gbmFit1$result
    res[which(res$Accuracy == max(res$Accuracy)),]

    gbmFit1.predict <- predict(gbmFit1, erp_valid)
    gbmFit1.predict[is.na(gbmFit1.predict)] <- 2
    print(paste("Validation accuracy", mean((gbmFit1.predict==erp_valid$Stimulus_Type)), sep = " "))

    # parameters found by caret
    gbm.fit <- gbm(Stimulus_Type ~ ., 
                   data = erp_train,
                   distribution="gaussian",
                   n.trees=1500,
                   interaction.depth=1, 
                   shrinkage = 0.004)
   valid_pred <- predict(gbm.fit, erp_valid, n.tree=1500)
   valid_pred[valid_pred >= 1.5] <- 2
   valid_pred[valid_pred  < 1.5] <- 1
   index <- erp_valid$Stimulus_Type == valid_pred
   print(paste("Validation accuracy = ", round(sum(index)/length(valid_pred)*100,4)), sep="")
   table(valid_pred,erp_valid$Stimulus_Type)



    f_train <- cbind(f3_train[,1:129], f2_train[,2:67])
    f_valid <- cbind(f3_valid[,1:129], f2_valid[,2:67])

    lasso_pred    <- lasso_model(   erp_train, erp_valid)
    logistic_pred <- logistic_model(erp_train, erp_valid)
    knn_pred      <- knn_model(     erp_train, erp_valid)
    lda_pred      <- lda_model(     erp_train, erp_valid)
    qda_pred      <- qda_model(     erp_train, erp_valid)
    tree_pred     <- tree_model(    erp_train, erp_valid)
    gbm_pred      <- gbm_model(     erp_train, erp_valid)
    rf_pred       <- rf_model(      erp_train, erp_valid)
    svm_pred      <- svm_model(     erp_train, erp_valid)


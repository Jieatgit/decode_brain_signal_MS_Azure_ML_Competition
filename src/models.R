library(glmnet) # glmnet (lasso/ridge/elastic net/logistic)
library("MASS") # lda, qda
library(class)  # knn
library(e1071)  # svm
library(tree)   # tree
library(randomForest) # rf
library(gbm)    # gbm
library(caret)  # caret

# lm
# lasso/ridge
# logistic
# svm
# nnt
# rf
# gbm


knn_model <-function(erp_train, erp_valid){
  ## knn model
  set.seed(1)
  col_names <- colnames(erp_train)
  ncols <- length(col_names)
  X_train <- erp_train[,2:(ncols-2)]
  Y_train <- erp_train[,ncols-1]
  X_valid <- erp_valid[,2:(ncols-2)]
  valid_pred <- knn(X_train, X_valid, Y_train, k=5)
  index <- erp_valid[,ncols-1] == valid_pred
  print(paste("Validation accuracy = ", round(sum(index)/length(valid_pred)*100,4)), sep="")
  
  # Step 6. Save the model object to a local rda file 
  # save(valid_pred,  file = lda_predict_file)
  return(valid_pred)
}

lda_model <-function(erp_train, erp_valid){
  ## lda model
  ncols <- length(erp_train)
  good_col <- (erp_train[1,]!=0)
  good_col[1] <- FALSE
  good_col[67] <- FALSE
  lda.fit <- lda(Stimulus_Type ~ ., data = erp_train[,good_col], na.action = na.omit)
  valid_pred <- predict(lda.fit, erp_valid[,good_col])$class
  index <- erp_valid[,ncols-1] == valid_pred
  print(paste("Validation accuracy = ", round(sum(index)/length(valid_pred)*100,4)), sep="")
  
  # Step 6. Save the model object to a local rda file 
  # save(valid_pred,  file = lda_predict_file)
  return(valid_pred)
}

qda_model <-function(erp_train, erp_valid){
  ## qda model
  col_names <- colnames(erp_train)
  ncols <- length(col_names)
  formula <- paste(col_names[2:(length(col_names)-2)], collapse="+")
  formula <- paste("Stimulus_Type ~ ", formula, sep="")
  qda.fit <- qda(Stimulus_Type ~ .-PatientID-Stimulus_ID, data = erp_train, na.action = na.omit)
  valid_pred <- predict(qda.fit, erp_valid)$class
  index <- erp_valid[,ncols-1] == valid_pred
  print(paste("Validation accuracy = ", round(sum(index)/length(valid_pred)*100,4)), sep="")
  
  # Step 6. Save the model object to a local rda file 
  # save(valid_pred,  file = qda_predict_file)
  return(valid_pred)
}

linear_model <-function(erp_train, erp_valid){
    ## Linear model
    col_names <- colnames(erp_train)
    ncols <- length(col_names)
    formula <- paste(col_names[2:(length(col_names)-2)], collapse="+")
    formula <- paste("Stimulus_Type ~ ", formula, sep="")
    lm.fit <- lm(formula = formula, data = erp_train)
    valid_pred <- predict(lm.fit, erp_valid)
    valid_pred[valid_pred >= 1.65] <- 2
    valid_pred[valid_pred < 1.65]  <- 1
    index <- erp_valid[,ncols-1] == valid_pred
    print(paste("Validation accuracy = ", round(sum(index)/length(valid_pred)*100,4)), sep="")
    
    # Step 6. Save the model object to a local rda file 
    # save(valid_pred,  file = lm_predict_file)
    return(valid_pred)
}

lasso_model <- function(erp_train, erp_valid){
    ## Lasso model
    # Step 5.5. Train a logistic regression model on training data, and valid it on validation data
    col_names <- colnames(erp_train)
    ncols <- length(col_names)
    erp_train[,ncols-1] <- erp_train[,ncols-1] - 1 # convert label 1 and 2 to 0 and 1 for logistic regression
    erp_valid[,ncols-1] <- erp_valid[,ncols-1] - 1 # convert label 1 and 2 to 0 and 1 for logistic regression
    formula <- paste(col_names[2:(length(col_names)-2)], collapse="+")
    formula <- paste("Stimulus_Type ~ ", formula, sep="")
    # glmnetmodel <- glmnet(x=as.matrix(erp_train[,2:(ncols-2)]), y=erp_train[,ncols-1], alpha=1, nlambda=1, lambda=0.01,family='binomial') #train a LASSO model, where lambda=0.01
    glmnetmodel <- glmnet(x=as.matrix(erp_train[,2:(ncols-2)]), y=erp_train[,ncols-1], alpha=1, nlambda=1, lambda=0.01) #train a LASSO model, where lambda=0.01
    summary(glmnetmodel)
    erp_train[,ncols-1] <- erp_train[,ncols-1] + 1 # convert label 1 and 2 to 0 and 1 for logistic regression
    erp_valid[,ncols-1] <- erp_valid[,ncols-1] + 1 # convert label 1 and 2 to 0 and 1 for logistic regression
    
    valid_pred <- predict(glmnetmodel, newx = as.matrix(erp_valid[,2:(ncols-2)]), type="response")
    valid_pred[valid_pred >= 0.65] <- 2
    valid_pred[valid_pred < 0.65]  <- 1
    
    index <- erp_valid[,ncols-1] == valid_pred
    print(paste("Validation accuracy = ", round(sum(index)/length(valid_pred)*100,4)), sep="")
    
    # Step 6. Save the model object to a local rda file 
    # save(glmnetmodel, file = model_rda_file)
    # save(valid_pred,  file = lasso_predict_file)
    # save(valid_pred,  file = logistic_predict_file)
    return(valid_pred)
}


logistic_model <- function(erp_train, erp_valid){
  ## Lasso model
  # Step 5.5. Train a logistic regression model on training data, and valid it on validation data
  col_names <- colnames(erp_train)
  ncols <- length(col_names)
  erp_train[,ncols-1] <- erp_train[,ncols-1] - 1 # convert label 1 and 2 to 0 and 1 for logistic regression
  erp_valid[,ncols-1] <- erp_valid[,ncols-1] - 1 # convert label 1 and 2 to 0 and 1 for logistic regression
  formula <- paste(col_names[2:(length(col_names)-2)], collapse="+")
  formula <- paste("Stimulus_Type ~ ", formula, sep="")
  glmnetmodel <- glmnet(x=as.matrix(erp_train[,2:(ncols-2)]), y=erp_train[,ncols-1], alpha=1, nlambda=1, lambda=0.01,family='binomial') #train a LASSO model, where lambda=0.01
  summary(glmnetmodel)
  erp_train[,ncols-1] <- erp_train[,ncols-1] + 1 # convert label 1 and 2 to 0 and 1 for logistic regression
  erp_valid[,ncols-1] <- erp_valid[,ncols-1] + 1 # convert label 1 and 2 to 0 and 1 for logistic regression
  
  #valid_pred <- predict(logitmodel, newdata = erp_valid, type="response")
  valid_pred <- predict(glmnetmodel, newx = as.matrix(erp_valid[,2:(ncols-2)]), type="response")
  valid_pred[valid_pred >= 0.55] <- 2
  valid_pred[valid_pred < 0.55]  <- 1
  
  index <- erp_valid[,ncols-1] == valid_pred
  print(paste("Validation accuracy = ", round(sum(index)/length(valid_pred)*100,4)), sep="")
  
  # Step 6. Save the model object to a local rda file 
  # save(glmnetmodel, file = model_rda_file)
  # save(valid_pred,  file = logistic_predict_file)
  return(valid_pred)
}

svm_model <-function(erp_train, erp_valid){
  ## lda model
  set.seed(1)
  ncols <- length(erp_train)
  erp_train[,ncols-1] <- as.factor(erp_train[,ncols-1])
  erp_valid[,ncols-1] <- as.factor(erp_valid[,ncols-1])
  
  # svm.fit <- svm(Stimulus_Type ~ .-PatientID-Stimulus_ID, data = erp_train,kernel="radial",cost=10,scale=F)
  tune.out=tune(svm, Stimulus_Type ~ .-PatientID-Stimulus_ID, data = erp_train,kernel="radial",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
  summary(tune.out)
  svm.fit=tune.out$best.model
  summary(svm.fit)
  
  valid_pred <- predict(svm.fit, erp_valid)
  index <- erp_valid[,ncols-1] == valid_pred
  print(paste("Validation accuracy = ", round(sum(index)/length(valid_pred)*100,4)), sep="")
  
  # Step 6. Save the model object to a local rda file 
  # save(valid_pred,  file = lda_predict_file)
  return(valid_pred)
}

tree_model <-function(erp_train, erp_valid){
  ## lda model
  set.seed(1)
  ncols <- length(erp_train)
  erp_train[,ncols-1] <- as.factor(erp_train[,ncols-1])
  erp_valid[,ncols-1] <- as.factor(erp_valid[,ncols-1])
  
  tree.fit <- randomForest(Stimulus_Type ~ .-PatientID-Stimulus_ID, data = erp_train)
  
#   cv.tree.fit <- cv.tree(tree.fit, FUN = prune.misclass)
#   par(mfrow=c(1,2))
#   plot(cv.tree.fit$size, cv.tree.fit$dev,type = "b")
#   plot(cv.tree.fit$k, cv.tree.fit$dev,type = "b")
#   prune.fit=prune.misclass(tree.fit,best=25)
#   plot(prune.fit)
#   text(prune.fit,pretty=0)
  
  valid_pred <- predict(tree.fit, erp_valid, type = "class")
  index <- erp_valid[,ncols-1] == valid_pred
  print(paste("Validation accuracy = ", round(sum(index)/length(valid_pred)*100,4)), sep="")
  table(valid_pred,erp_valid[,ncols-1])
  
  # Step 6. Save the model object to a local rda file 
  # save(valid_pred,  file = lda_predict_file)
  return(valid_pred)
}

rf_model <-function(erp_train, erp_valid){
  ## lda model
  set.seed(1)
  ncols <- length(erp_train)
  erp_train[,ncols-1] <- as.factor(erp_train[,ncols-1])
  erp_valid[,ncols-1] <- as.factor(erp_valid[,ncols-1])
  
  rf.fit <- randomForest(Stimulus_Type ~ .-PatientID-Stimulus_ID, data = erp_train,mtry=30,ntree=1000,importance=TRUE)
  
  valid_pred <- predict(rf.fit, erp_valid)
  index <- erp_valid[,ncols-1] == valid_pred
  print(paste("Validation accuracy = ", round(sum(index)/length(valid_pred)*100,4)), sep="")
  table(valid_pred,erp_valid[,ncols-1])
  
  # Step 6. Save the model object to a local rda file 
  # save(valid_pred,  file = lda_predict_file)
  return(valid_pred)
}

gbm_model <-function(erp_train, erp_valid){
  ## lda model
  set.seed(1)
  ncols <- length(erp_train)

  gbm.fit <- gbm(Stimulus_Type ~ .-PatientID-Stimulus_ID, data = erp_train,,distribution="gaussian",n.trees=5000,interaction.depth=4, shrinkage = 0.01)
  
  valid_pred <- predict(gbm.fit, erp_valid,n.trees=5000)
  valid_pred[valid_pred >= 1.5] <- 2
  valid_pred[valid_pred  < 1.5] <- 1
  index <- erp_valid[,ncols-1] == valid_pred
  print(paste("Validation accuracy = ", round(sum(index)/length(valid_pred)*100,4)), sep="")
  table(valid_pred,erp_valid[,ncols-1])
  
  # Step 6. Save the model object to a local rda file 
  # save(valid_pred,  file = lda_predict_file)
  return(valid_pred)
}

gbm_model_by_caret <- function(erp_train, erp_valid) {
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
                         n.trees = seq(10,100,by=10),
                         shrinkage = c( 0.001*(1:5), 0.01),
                         n.minobsinnode = 10)
  gbmCtr  <- trainControl(method = "cv", number = 5, repeats = 1)
  gbmFit1 <- train(as.factor(Stimulus_Type) ~ ., 
                   data = erp_train,
                   method = "gbm", distribution = "multinomial",
                   trControl = gbmCtr,
                   tuneGrid = gbmGrid)
  
  # save(gbmFit1,file = paste(dataDir, "caretGbmFit.RDATA", sep = ""))
  gbmFit1.predict <- predict(gbmFit1, erp_valid)
  gbmFit1.predict[is.na(gbmFit1.predict)] <- 2
  print(paste("Validation accuracy", mean((gbmFit1.predict==erp_valid$Stimulus_Type)), sep = " ")) # 1.958711
}



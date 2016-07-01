
# train different models for different patients

f_train <- cbind(f1_train[,1:129], f2_train[,2:67])
f_valid <- cbind(f1_valid[,1:129], f2_valid[,2:67])

unique_patients <- unique(f2_train[,1]) 
num_patients    <- length(unique_patients) 
num_cols        <- ncol(f2_train)
pred <- NULL
i=1
erp_train <- f_train
erp_valid <- f_valid
erp_train_i   <- erp_train[erp_train[,1]==unique_patients[i],]
erp_valid_i   <- erp_valid[erp_valid[,1]==unique_patients[i],]
pred_i        <- logistic_model(   erp_train_i, erp_valid_i)
pred <- rbind(pred, matrix(pred_i,length(pred_i),1))
# erp_valid$predict[erp_valid[,1]==unique_patients[i]] <- pred
i=2
erp_train <- f_train
erp_valid <- f_valid
erp_train_i   <- erp_train[erp_train[,1]==unique_patients[i],]
erp_valid_i   <- erp_valid[erp_valid[,1]==unique_patients[i],]
pred_i        <- gbm_model(   erp_train_i, erp_valid_i)
pred <- rbind(pred, matrix(pred_i,length(pred_i),1))
# erp_valid$predict[erp_valid[,1]==unique_patients[i]] <- pred
i=3
erp_train <- f_train
erp_valid <- f_valid
erp_train_i   <- erp_train[erp_train[,1]==unique_patients[i],]
erp_valid_i   <- erp_valid[erp_valid[,1]==unique_patients[i],]
pred_i        <- logistic_model(   erp_train_i, erp_valid_i)
pred <- rbind(pred, matrix(pred_i,length(pred_i),1))
# erp_valid$predict[erp_valid[,1]==unique_patients[i]] <- pred
i=4
erp_train <- f2_train
erp_valid <- f2_valid
erp_train_i   <- erp_train[erp_train[,1]==unique_patients[i],]
erp_valid_i   <- erp_valid[erp_valid[,1]==unique_patients[i],]
pred_i        <- gbm_model(   erp_train_i, erp_valid_i)
pred <- rbind(pred, matrix(pred_i,length(pred_i),1))
# erp_valid$predict[erp_valid[,1]==unique_patients[i]] <- pred

erp_valid$predict <- pred
mean(erp_valid$Stimulus_Type==erp_valid$predict)




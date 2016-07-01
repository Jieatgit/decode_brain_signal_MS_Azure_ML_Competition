# validation accuracy
#
#  |                |f1 |  f2 |f2+f1                 |f3         |
#  |:-------------- |:--|:--  |:---------------------|:--        |
#  | p1             |   |     |   88 logistic/rf     | 88gbm     |
#  | p2             |   |     |   88 gbm/rf          | 94rf      |
#  | p3             |   |92log|   98 logistic        |           |
#  | p4             |   |   88|      gbm/rf          | 92logistic|  
#  | all            |   |     |   90                 |           |


# train different models for different patients

f_train <- cbind(f3_train[,1:129], f2_train[,2:67])
f_valid <- cbind(f3_valid[,1:129], f2_valid[,2:67])

unique_patients <- unique(f2_train[,1]) 
num_patients    <- length(unique_patients) 
num_cols        <- ncol(f2_train)
pred <- NULL
i=1
erp_train <- f3_train
erp_valid <- f3_valid
erp_train_i   <- erp_train[erp_train[,1]==unique_patients[i],]
erp_valid_i   <- erp_valid[erp_valid[,1]==unique_patients[i],]
pred_i        <- gbm_model(   erp_train_i, erp_valid_i)
pred <- rbind(pred, matrix(pred_i,length(pred_i),1))
# erp_valid$predict[erp_valid[,1]==unique_patients[i]] <- pred
i=2
erp_train <- f3_train
erp_valid <- f3_valid
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
erp_train <- f3_train
erp_valid <- f3_valid
erp_train_i   <- erp_train[erp_train[,1]==unique_patients[i],]
erp_valid_i   <- erp_valid[erp_valid[,1]==unique_patients[i],]
pred_i        <- logistic_model(   erp_train_i, erp_valid_i)
pred <- rbind(pred, matrix(pred_i,length(pred_i),1))
# erp_valid$predict[erp_valid[,1]==unique_patients[i]] <- pred

erp_valid$predict <- pred
mean(erp_valid$Stimulus_Type==erp_valid$predict)




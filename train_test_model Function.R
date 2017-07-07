library(Matrix)
library(xgboost)
library(data.table)

train_test_model<-function(train_model,test_model,booster="dart",eta = .02,max_depth = 6,subsample = .7,colsample_bytree = .7){

  
  varnames <- setdiff(colnames(train_model), c("id", "price_doc", "timestamp"))
  
  #varnames <- setdiff(colnames(train_model), c("id", "price_doc"))
  
  train_sparse <- Matrix(as.matrix(sapply(train_model[!is.na(train_model$price_doc), varnames],as.numeric)), sparse=TRUE)
  test_sparse <- Matrix(as.matrix(sapply(test_model[!is.na(test_model$price_doc), varnames],as.numeric)), sparse=TRUE)
  y_train <- log1p(train_model[!is.na(train_model$price_doc), "price_doc"])
  test_ids <- test_model[!is.na(test_model$price_doc), "id"]
  train <- xgb.DMatrix(data=train_sparse, label=y_train)
  test <- xgb.DMatrix(data=test_sparse)
  gc()
  
  # Params for xgboost
  param <- list(booster="dart",
                objective="reg:linear",
                eval_metric = "rmse",
                eta = .02,
                # gamma = 1,
                max_depth = 6,
                min_child_weight = 1,
                subsample = .7,
                colsample_bytree = .7
  )
  

  set.seed(7)
  rounds <- 100
  
  # Train model
  
  xgb_model <- xgb.train(data = train,
                         params = param,
                         watchlist = list(train = train),
                         nrounds = rounds,
                         verbose = 1,
                         print_every_n = 5
  );gc()
  
  # Predict and output csv

  train_preds <- predict(xgb_model,train)
  train_preds <- expm1(train_preds)
  
  test_preds <- predict(xgb_model,test)
  test_preds <- expm1(test_preds)
  
  preds<-list(train_preds=train_preds,train=train_model[!is.na(train_model$price_doc),]$price_doc,test_preds=test_preds,test=test_model[!is.na(test_model$price_doc),]$price_doc)
  
  return(preds)
  
}


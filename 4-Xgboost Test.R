library(Matrix)
library(xgboost)

var<-c(importance_matrix$Feature[1:50],"price_doc","id","year")
var<-c(importance_matrix$Feature[1:50],"price_doc","id","year", "life_sq_pc","floor_diff","build_age","green_zone_m","indust_m","children_pc","harm_km", area_names )
# To sparse matrix

dtotal_model<-dtotal[!is.na(dtotal$price_doc) & dtotal$year>=2014,]
dtotal_model<-dtotal[!is.na(dtotal$price_doc) & dtotal$year>=2014,-which(names(dtotal) %in% c("price_doc_old","cpi")) ]
dtotal_model<-dtotal[!is.na(dtotal$price_doc) & dtotal$year>=2014, which(names(dtotal) %in% var )]

varnames <- setdiff(colnames(dtotal_model), c("id", "price_doc", "timestamp"))

train_sparse <- Matrix(as.matrix(sapply(dtotal_model[dtotal_model$year==2014, varnames],as.numeric)), sparse=TRUE)
test_sparse <- Matrix(as.matrix(sapply(dtotal_model[dtotal_model$year>2014 , varnames],as.numeric)), sparse=TRUE)
y_train <- log1p(dtotal_model[dtotal_model$year==2014, "price_doc"])
test_ids <- dtotal_model[dtotal_model$year>2014, "id"]
train <- xgb.DMatrix(data=train_sparse, label=y_train)
test <- xgb.DMatrix(data=test_sparse)
gc()

# Params for xgboost
param <- list(booster="dart",
              objective="reg:linear",
              eval_metric = "rmse",
              eta = .1,
              gamma = 1,
              max_depth = 5,
              min_child_weight = 1,
              subsample = .7,
              colsample_bytree = .7
)


set.seed(7)
rounds <- 300

# Train model

xgb_model <- xgb.train(data = train,
                       params = param,
                       watchlist = list(train = train),
                       nrounds = rounds,
                       verbose = 1,
                       print_every_n = 5
);gc()

#calculating prediction
pred <- predict(xgb_model,test)
pred <- expm1(pred)

##fixing for inflation
preds<-pred*(dtotal[!is.na(dtotal_model$price_doc) & dtotal_model$year>2014,]$cpi/405.9)
preds<-pred*dtotal[!is.na(dtotal$price_doc) & dtotal$year>2014,]$cpi

#Error
error<-data.frame(fitted=preds,real=dtotal_model[!is.na(dtotal_model$price_doc) & dtotal_model$year>2014,]$price_doc) 

error$log<-(log(error$fitted+1)-log(error$real+1))^2

final_error<-sqrt(sum(error$log,na.rm = TRUE)/length(error$log))

final_error

plot(error$fitted,error$real)


# Feature importance
cat("Plotting feature importance")
names <- dimnames(train_sparse)[[2]]
importance_matrix <- xgb.importance(names,model=xgb_model)
xgb.plot.importance(importance_matrix[1:20,])


importance_matrix$Feature[1:50]


#  unclean, no macro, no inflation -> 0.4153100233
#  clean, no macro, no inflation -> 0.4121719453
#  clean,  macro(selected), no inflation -> 0.4149921702
#  clean + full_sq*100+full_sq/100, no macro, no inflation -> 0.4171993928
#  clean + full_sq*100 + no macro, no inflation ->0.4132242184
#  clean,  no macro, inflation -> 0.4536928976
#  clean,  no macro, no inflation, feature-> 0.4133735209
#  clean,  no macro, no inflation, feature, 100 importance -> 0.4147924643
#  clean,  no macro, no inflation, feature, 50 importance -> 0.4121359754
#  clean,  no macro, no inflation, feature, 50 importance+feature -> 0.4117866342
#  clean,  no macro,  inflation, feature, 50 importance+feature -> 0.4178846098
#  clean, no macro, no inflation, area binary -> 0.4136512034
#  clean, no macro, no inflation, 50 importance+feature+area binary -> 0.4138763627



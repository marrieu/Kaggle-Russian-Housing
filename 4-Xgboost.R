

var<-c("price_doc",
      # "id",
       "micex_cbi_tr",
       "usdrub",                                    
       "micex",
       "brent",                                     
       "eurrub",
       "micex_rgbi_tr",
       "cpi",
       "rts",                                       
       "ppi",
       "year",
       "full_sq",
       "life_sq",
       "life_sq_m",
       "floor",
       "max_floor",
       "material",
       "build_year",
       "num_room",
       "kitch_sq",
       "state",
       "product_type",
       "sub_area",
       "area_m",
       "raion_popul",
       "indust_m",
       "green_zone_m",
       "full_all",
       "children",
       "young_all",
       "work_all",
       "ekder_all",
       "harm_km",
       "poor",
       "super_rich",
       "rich",
       "build_count_brick",
       "metro_min_walk",
       "metro_min_avto",
       "kindergarten_km",
       "school_km",
       "park_km",
       "railroad_station_walk_km",
       "railroad_station_avto_km",
       "public_transport_station_km",
       "zd_vokzaly_avto_km",
       "ttk_km",
       "sadovoe_km",
       "big_road1_km",
       "railroad_km",
       "ID_railroad",
       "ID_bus",
       "nuclear_reactor_km",
       "fitness_km",
       "ice_rink_km",
       "swim_pool_km",
       "detention_facility_km",
       "public_healthcare_km",
       "university_km",
       "workplaces_km",
       "office_km",
       "additional_education_km",
       "catering_km",
       "preschool_km",
       "big_church_km",
       "mosque_km",
       "office_count_500",
       "trc_count_500",
       "cafe_count_500",
       "cafe_avg_price_500",
       "leisure_count_500",
       "cafe_count_5000",
       "market_count_5000")

var<-c("price_doc",
     #  "id",
       "year",
  importance_var
  # "full_state",
  # "full_year",
  # "build_age",
  # "ID_railroad",
  # "ID_bus",
  # "harm_km",
  # "poor",
  # "super_rich",
  # "rich",
  # "life_sq_m",
  # "cpi"
)
  

    
# To sparse matrix

dtotal_model<-dtotal[dtotal$year>=2014,-which(names(dtotal) %in% c("price_doc_old","cpi")) ]

dtotal_model<-dtotal[dtotal$year>=2014, var]


varnames <- setdiff(colnames(dtotal_model), c("id", "price_doc", "timestamp"))

varnames <- setdiff(colnames(dtotal_model), c("id", "price_doc"))

train_sparse <- Matrix(as.matrix(sapply(dtotal_model[!is.na(dtotal_model$price_doc), varnames],as.numeric)), sparse=TRUE)
test_sparse <- Matrix(as.matrix(sapply(dtotal_model[is.na(dtotal_model$price_doc), varnames],as.numeric)), sparse=TRUE)
y_train <- log1p(dtotal_model[!is.na(dtotal_model$price_doc), "price_doc"])
test_ids <- dtotal_model[is.na(dtotal_model$price_doc), "id"]
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

# Params for xgboost
param <- list(booster="gblinear",
              objective="reg:linear"
             # eval_metric = "rmse"
              #eta = .05,
              #gamma = 1,
              #max_depth = 4,
             # min_child_weight = 1,
             # subsample = .7,
              #colsample_bytree = .7
)


set.seed(7)
rounds <- 450

# Train model

xgb_model <- xgb.train(data = train,
                       params = param,
                       watchlist = list(train = train),
                       nrounds = rounds,
                       verbose = 1,
                       print_every_n = 5
);gc()


# all var + max_deph=10 =>0.335
# all var + max_deph=6 =>0.325


# booster="gbtree",
# train-rmse:0.471797 
#train-rmse:0.368096 => 0.33155 for year >=2014, 
#train-rmse:0.358701 => 0.33586
#train-rmse:0.377312 => 0.33362
#train-rmse:0.377312 *1.035966544 =>


# booster="dart",
#train-rmse:0.341700 =>  0.33524 for year >=2014, "dart" max_depth = 7,
#train-rmse:0.377312 *1.035966544 =>

# booster="gblinear",
# train-rmse:0.469192


# Feature importance
cat("Plotting feature importance")
names <- dimnames(train_sparse)[[2]]
importance_matrix <- xgb.importance(names,model=xgb_model)
xgb.plot.importance(importance_matrix[1:14,])

importance_var<-importance_matrix$Feature[1:14]

# Predict and output csv
cat("Predictions")
preds <- predict(xgb_model,test)
preds <- expm1(preds)

##fixing for inflation
preds<-preds*1.037
preds<-preds*(dtotal[is.na(dtotal_model$price_doc),]$cpi/405.9)*1.037
preds<-ifelse(dtotal[is.na(dtotal_model$price_doc),]$year > 2015, preds*(dtotal[is.na(dtotal_model$price_doc),]$cpi/405.9)*1.037,preds*(dtotal[is.na(dtotal_model$price_doc),]$cpi/405.9))

write.table(data.table(id=test_ids, price_doc=preds), "submission6.csv", sep=",", dec=".", quote=FALSE, row.names=FALSE)

submission<-data.table(id=test_ids, price_doc=preds)

0.32658
0.32533


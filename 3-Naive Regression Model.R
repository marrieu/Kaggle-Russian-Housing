
source("2-Data Cleaning and Feature Engineering.R")

var<-c("price_doc",
       "full_sq",
       "life_sq_m",
       "floor",
       "max_floor",
       "material",
       "build_year",
       "num_room",
       "kitch_sq",
       "state",
       "product_type",
       #"sub_area",
       "ID_railroad",
       "ID_bus",
       "harm_km",
       "super_rich",
       "rich",
       "poor",
        "ttk_km",
       "cafe_count_2000",
      "build_year",
      "additional_education_km",
      "nuclear_reactor_km",
    #  "cafe_count_1500_price_2500",
      "public_healthcare_km",
      "industrial_km",
      "cafe_count_1000",
      "cemetery_km",
      #"preschool_km",
      "green_part_1000",           
      # "life_sq_pc",
      # "kitch_sq_pc",
       "floor_pc",
      # "floor_diff",
       "build_age",
       "year",
       "yearmonth",
      "micex_cbi_tr",
      "usdrub",                                    
    #  "micex",
      "brent",                                     
      "eurrub",
      "micex_rgbi_tr",
      "cpi",
    #  "rts",                                       
      "ppi"
         )


#################################
#Regression
##################################

#dtotal_model<-dtotal[!is.na(dtotal$price_doc), -which(names(dtotal) %in% c(var,"timestamp","id"))]

dtotal_model<-dtotal[!is.na(dtotal$price_doc),var]

# Test model
m_test<-lm( log(price_doc) ~ . , data= dtotal_model[dtotal_model$year>=2014,])

summary(m_test)
# Evaluating model

pred_test<-predict.lm(m_test,dtotal_model[dtotal_model$year==2015,])

error_test<-data.frame(fitted=pred_test,real=dtotal_model[dtotal_model$year==2015,]$price_doc) 

error_test$log<-(log(exp(error_test$fitted)+1)-log(error_test$real+1))^2

final_error_test<-sqrt(sum(error_test$log,na.rm = TRUE)/sum(!is.na(error_test$fitted)))

final_error_test

plot(error_test$fitted,error_test$real)


# Final model


m<-lm( log(price_doc) ~ . , data= dtotal_model)

summary(m)

pred<-predict.lm(m,dtotal_model)

error<-data.frame(fitted=pred,real=dtotal_model$price_doc) 

error$log<-(log(exp(error$fitted)+1)-log(error$real+1))^2

final_error<-sqrt(sum(error_test$log,na.rm = TRUE)/sum(!is.na(error$fitted)))

final_error

plot(error$fitted,error$real)

#dev.off()

##################################
# Test Data
##################################

pred<-predict.lm(m,dtotal[is.na(dtotal$price_doc),])

submission<-data.frame(id=dtotal[is.na(dtotal$price_doc),]$id,price_doc=exp(pred))

write.csv(submission,file="submission.csv",row.names=FALSE)

str(submission)

# 0.35535


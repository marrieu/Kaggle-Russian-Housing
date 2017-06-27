######################################
####### Libraries and Importing datasets
######################################

# microsoft professional program data science 


library(data.table)
library(tidyverse) #ggplot %>% sample_n
library(lubridate) #year /month
library(scales) #scale dollar on ggplot
library(corrplot) #corrplot
library(DT)
library(rgdal)


#importing train set
#dtrain <- fread('train.csv', stringsAsFactors=TRUE)
dtrain <- read.csv('/Users/mariaclaravarga/R/Russian Bank/train.csv', stringsAsFactors=TRUE)
dtest <- read.csv('/Users/mariaclaravarga/R/Russian Bank/test.csv', stringsAsFactors=TRUE)
dmacro <- read.csv('/Users/mariaclaravarga/R/Russian Bank/macro.csv', stringsAsFactors=TRUE)


#creatind the price doc to combine datasets
dtest$price_doc<-NA

#combining train and tes
dtotal<-rbind(dtrain,dtest)


###########################
#Date Variable
###########################

#Transforming variable data
dtotal$timestamp<-as.Date(dtotal$timestamp)
dmacro$timestamp<-as.Date(dmacro$timestamp)

#creating variable year and month
dtotal$year<-year(dtotal$timestamp)
dtotal$month<-month(dtotal$timestamp)
dtotal$yearmonth<-ifelse(dtotal$month<10,paste0(dtotal$year,0,dtotal$month),paste0(dtotal$year,dtotal$month))
dtotal$yearmonth<-as.numeric(dtotal$yearmonth)

############################################################
#################### MERGING #########################
############################################################

#################### MACRO #########################
#Suggested by a post
# var_macro<-c( "timestamp",
#               "balance_trade",                      
#               "balance_trade_growth",               
#               "eurrub",                             
#               "average_provision_of_build_contract",
#               "micex_rgbi_tr",                      
#               "micex_cbi_tr",                       
#               "deposits_rate",                      
#               "mortgage_value",                     
#               "mortgage_rate",                      
#               "income_per_cap",                     
#               "rent_price_4.room_bus",              
#               "museum_visitis_per_100_cap",         
#               "apartment_build",
#               "cpi")

var_macro<-c("timestamp","cpi")

# variable importance
var_macro<-c("timestamp",
             "micex_cbi_tr",
             "usdrub",                                    
              "micex",
              "brent",                                     
              "eurrub",
              "micex_rgbi_tr",
              "cpi",
              "rts",                                       
              "ppi")

dtotal<- merge(dtotal, dmacro[,var_macro], by="timestamp", all.x=TRUE)
dtotal<- merge(dtotal, dmacro, by="timestamp", all.x=TRUE)

#################### EXTERNAL DATA #########################

#copy this from https://www.kaggle.com/philippsp/an-updated-collection-of-new-features
shp <- readOGR(dsn = "administrative-divisions-of-moscow", layer = "moscow_adm")

centroids <- coordinates(shp)
sub_area <- shp$RAION
okrug <- shp$OKRUGS
location_data <- data.frame(sub_area = sub_area, okrug = okrug, longitude=centroids[,1], latitude=centroids[,2])

dtotal <- dtotal %>%
  left_join(location_data,by="sub_area")

# Calculate distance from Kremlin for each sub_area
kremlin = data.frame(longitude = 37.617664,latitude = 55.752121)
dtotal <- dtotal %>%
  group_by(sub_area) %>%
  top_n(n = 1, wt=id) %>%
  ungroup %>%
  mutate(distance_from_kremlin = distm(.[c("longitude","latitude")], kremlin, fun=distHaversine)) %>%
  select(sub_area, distance_from_kremlin) %>%
  right_join(dtotal, by="sub_area")



############################################################
#################### DATA CLEANING #########################
############################################################


##############################
# Build Year
##############################

dtotal$build_year[dtotal$build_year == 20052009] <- 2005
dtotal$build_year[dtotal$build_year == 0] <- 1969
dtotal$build_year[dtotal$build_year == 1] <- 1974
dtotal$build_year[dtotal$build_year == 2] <- NA
dtotal$build_year[dtotal$build_year == 20] <- 2000
dtotal$build_year[dtotal$build_year == 215] <- 2015
dtotal$build_year[dtotal$build_year == 3] <- 1964
dtotal$build_year[dtotal$build_year == 4965] <- NA
dtotal$build_year[dtotal$build_year == 71] <- 1860
# I saw this mistake because for some reason some entries of kitch_sq was wrongly filled with build_year
dtotal$build_year[dtotal$id == 13120]<-1970



##############################
#State
##############################

dtotal$state[dtotal$state == 33] <- 3


##############################
#full_sq
##############################

# I've realized that full_sq bigger than 250 can actually be a typo mistake, the graph looks much better with this problem fixed
dtotal$full_sq[dtotal$full_sq == 5326]<-53.26
dtotal$full_sq<-ifelse(dtotal$full_sq> 400, dtotal$full_sq/10, dtotal$full_sq )

dtotal$full_sq[dtotal$full_sq < 2]<-NA
#dtotal$full_sq<-ifelse(dtotal$full_sq< 10, dtotal$full_sq*10, dtotal$full_sq )


##############################
#life_sq
##############################
# same as before for the life_sq variable
dtotal$life_sq[dtotal$life_sq == 7478]<-74.78
dtotal$life_sq<-ifelse(dtotal$life_sq> 400, dtotal$life_sq/10, dtotal$life_sq )
#fixing for small values
dtotal$life_sq[dtotal$life_sq < 2]<-NA
#again, it might be a typo mistake
#dtotal$life_sq<-ifelse(dtotal$life_sq > dtotal$full_sq & dtotal$life_sq > 100, dtotal$life_sq/10 , dtotal$life_sq )


##############################
#kitch_sq
##############################
dtotal$kitch_sq[dtotal$kitch_sq > 500]<-NA
dtotal$kitch_sq<-ifelse(dtotal$kitch_sq > dtotal$life_sq, dtotal$kitch_sq/10 , dtotal$kitch_sq )

##############################
#max_floor
##############################

#max_floor cannot be lower than floor
dtotal$max_floor2<-ifelse(dtotal$max_floor>=dtotal$floor,dtotal$max_floor,NA)


############################################################
################ PREDICTIVE IMPUTATION #####################
############################################################

#setting dataset

var_internal<-c("price_doc",
                "full_sq",
                "life_sq",
                "floor",
                "max_floor",
                "material",
                "build_year",
                "num_room",
                "kitch_sq",
                "state",
                "product_type",
                "sub_area")

dtotal_mice<-dtotal[,var_internal]


# #####################################
# # Using MICE to fill missing values in age variable
# #####################################
 library('mice') # imputation
 # Set a random seed
 set.seed(7)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(dtotal_mice, method='rf')


# Save the complete output
mice_output <- complete(mice_mod)

write.csv(mice_output,file="mice_output.csv")


sum(is.na(dtotal_mice$full_sq))
sum(is.na(mice_output$full_sq))

# Checking if the distribution still similar
par(mfrow=c(1,2))
hist(dtotal_mice$full_sq, freq=F, main='Full_sq: Original Data',
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$full_sq , freq=F, main='Full_sq: MICE Output',
     col='lightgreen', ylim=c(0,0.04))


#reading mice output
#mice_output <- read.csv('mice_output.csv', stringsAsFactors=TRUE)

##############################
#assigning mice values
##############################

dtotal$full_sq<-mice_output$full_sq
dtotal$life_sq<-mice_output$life_sq
dtotal$floor<-mice_output$floor
dtotal$max_floor<-mice_output$max_floor
dtotal$material<-mice_output$material
dtotal$build_year<-mice_output$build_year
dtotal$num_room<-mice_output$num_room
dtotal$kitch_sq<-mice_output$kitch_sq
dtotal$state<-mice_output$state
dtotal$product_type<-mice_output$product_type
dtotal$sub_area<-mice_output$sub_area

############################################################
################## FEATURE ENGINEERING #####################
############################################################

#Interior Variables
dtotal$life_sq_pc<-dtotal$life_sq/dtotal$full_sq
dtotal$life_sq_m<-dtotal$full_sq-dtotal$life_sq
dtotal$full_state<-dtotal$full_sq*dtotal$state
dtotal$full_year<-dtotal$full_sq*dtotal$year
dtotal$kitch_sq_pc<-dtotal$kitch_sq/dtotal$life_sq
dtotal$floor_pc<-ifelse(dtotal$max_floor==0,0,dtotal$floor/dtotal$max_floor)
dtotal$floor_diff<-dtotal$max_floor-dtotal$floor
dtotal$build_age<-dtotal$year-dtotal$build_year
dtotal$full_age<-dtotal$full_sq*dtotal$build_age

#area variables
dtotal$pop_density_raion<- dtotal$raion_popul/dtotal$area_m
dtotal$green_zone_m<-dtotal$area_m*dtotal$green_zone_part
dtotal$indust_m<-dtotal$area_m*dtotal$indust_part

#population variable
dtotal$children<-dtotal$children_preschool+dtotal$children_school
dtotal$ratio_preschool<-dtotal$children_preschool/dtotal$preschool_quota
dtotal$ratio_school<- dtotal$children_school/dtotal$school_quota


#ID variable
dtotal$ID_railroad<-ifelse(dtotal$ID_railroad_terminal==5,1,ifelse(dtotal$ID_railroad_terminal==113 | dtotal$ID_railroad_terminal==83 ,3,2))
dtotal$ID_bus<-ifelse(dtotal$ID_bus_terminal %in% c(13,4,5,2,1),1,0)

#harm variable
dtotal$harm_km<- dtotal$cemetery_km+
                  dtotal$incineration_km+
                  dtotal$oil_chemistry_km+
                  dtotal$nuclear_reactor_km+
                  dtotal$radiation_km+
                  dtotal$power_transmission_line_km+
                  dtotal$thermal_power_plant_km+
                  dtotal$ts_km+
                  dtotal$hospice_morgue_km+
                  dtotal$detention_facility_km

#spliting sub_area variable
area<-model.matrix(~-1+sub_area, data=dtotal, 
                contrasts.arg = list(sub_area=contrasts(dtotal$sub_area, contrasts=F)))

area<-as.data.frame(area)

area$poor <-ifelse(area$`sub_areaPoselenie Novofedorovskoe`==1 |
                     area$`sub_areaPoselenie Klenovskoe`==1 |
                     area$`sub_areaMolzhaninovskoe`==1 |
                     area$`sub_areaPoselenie Shhapovskoe`==1 |
                     area$`sub_areaPoselenie Marushkinskoe`==1 |
                     area$`sub_areaPoselenie Krasnopahorskoe`==1 |
                     area$`sub_areaPoselenie Filimonkovskoe` ==1 | 
                     area$`sub_areaPoselenie Mihajlovo-Jarcevskoe`==1 |
                     area$`sub_areaPoselenie Kokoshkino`==1 |
                     area$`sub_areaPoselenie Voronovskoe`==1, 1,0)

area$super_rich<-ifelse(area$sub_areaHamovniki==1,1,0) 

area$rich<-ifelse(area$`sub_areaZamoskvorech'e`==1 |
                    area$sub_areaArbat==1 | 
                    area$sub_areaPresnenskoe==1 | 
                    area$sub_areaObruchevskoe==1 | 
                    area$sub_areaKurkino==1 |
                    area$sub_areaLomonosovskoe==1 | 
                    area$`sub_areaProspekt Vernadskogo`==1 | 
                    area$sub_areaGagarinskoe==1 |
                    area$sub_areaDorogomilovo==1 |
                    area$sub_areaKrylatskoe==1,1,0)


area_names<-names(area)

dtotal<-cbind(dtotal,area[,c(147:149)])

##################################
# Price adjusted for inflation
##################################

#"2014-01-09"
dtotal$price_doc_old<-dtotal$price_doc
dtotal$price_doc<-dtotal$price_doc_old*(405.9/dtotal$cpi)
dtotal$price_doc<-dtotal$price_doc_old/dtotal$cpi

#"2011-08-20"
#dtotal$price_doc_new<-dtotal$price_doc*(354/dtotal$cpi)


datatrain <- read.csv("dataclean_noNA.csv")
datatest <- read.csv("test.csv")

datatrainnew <- read.csv("datacleannew.csv", stringsAsFactors = F)
datatrain <- datatrainnew
 
names(datatrainnew)
 
datause <- datatrain %>%
  mutate(floor_divvide_maxfloor = floor/max_floor) %>%
  select(price_doc, id, avg_price_area_persq, build_year, cafe_count_5000_na_price, cafe_count_5000_price_1000, cafe_count_5000_price_1500,cafe_count_5000_price_2500, cafe_count_5000_price_4000, cafe_count_5000_price_500, cafe_count_5000_price_high, 
         church_count_5000, ecology, floor_divvide_maxfloor, full_sq, kitch_sq, leisure_count_5000, market_count_5000, metro_km_avto, mkad_km, month, mosque_count_5000, num_room, product_type, railroad_station_avto_km, sport_count_5000, state, sub_area, usedyear, year)

write.csv(datause,"train_feature_selected_noNA.csv")



datatest <- separate(datatest, timestamp, c("year", "month", "day"), sep = "-")

datatest$year <- as.numeric(datatest$year)

useyear <- function(x, y) {
  a <- c(1:length(y))
  for(i in 1:length(y)) {
    if(is.na(y[i])){
      a[i] <- NA
    } else {
      a[i] <- x[i]-y[i]
    }
  }
  return(a)
}

datatest$sub_area <- as.character(datatest$sub_area)

datatrain1 <- 
  datatest %>%
  mutate(usedyear = useyear(year, build_year)) %>%
  left_join(area, by=c("sub_area"="sub_area"))

area <- datause %>%
  group_by(sub_area) %>%
  summarise(avg_price_area_persq = max(avg_price_area_persq))

area$sub_area <- as.character(area$sub_area)

datause1 <- datatrain1 %>%
  mutate(floor_divvide_maxfloor = floor/max_floor) %>%
  select(id, avg_price_area_persq, build_year, cafe_count_5000_na_price, cafe_count_5000_price_1000, cafe_count_5000_price_1500,cafe_count_5000_price_2500, cafe_count_5000_price_4000, cafe_count_5000_price_500, cafe_count_5000_price_high, 
         church_count_5000, ecology, floor_divvide_maxfloor, full_sq, kitch_sq, leisure_count_5000, market_count_5000, metro_km_avto, mkad_km, month, mosque_count_5000, num_room, product_type, railroad_station_avto_km, sport_count_5000, state, sub_area, usedyear, year)

write.csv(datause1,"test_feature_selected.csv")


datause01 <- datause %>%
  select(price_doc, id, avg_price_area_persq, build_year, cafe_count_5000_na_price, cafe_count_5000_price_1000, cafe_count_5000_price_1500,cafe_count_5000_price_2500, cafe_count_5000_price_4000, cafe_count_5000_price_500, cafe_count_5000_price_high, 
         church_count_5000, ecology, floor_divvide_maxfloor, full_sq, kitch_sq, leisure_count_5000, market_count_5000, metro_km_avto, mkad_km, month, mosque_count_5000, num_room, railroad_station_avto_km, sport_count_5000, state, usedyear, year)
datause011 <- datause1%>%
  select(id, avg_price_area_persq, build_year, cafe_count_5000_na_price, cafe_count_5000_price_1000, cafe_count_5000_price_1500,cafe_count_5000_price_2500, cafe_count_5000_price_4000, cafe_count_5000_price_500, cafe_count_5000_price_high, 
         church_count_5000, ecology, floor_divvide_maxfloor, full_sq, kitch_sq, leisure_count_5000, market_count_5000, metro_km_avto, mkad_km, month, mosque_count_5000, num_room, railroad_station_avto_km, sport_count_5000, state, usedyear, year)

xgb <- xgboost(data = data.matrix(datause01[,3:28]), 
               label = datause01[,1], 
               #eta = 0.1,
               #max_depth = 15, 
               nround=25, 
               #subsample = 0.5,
               #colsample_bytree = 0.5,
               #seed = 1,
               #eval_metric = "merror",
               #objective = "multi:softprob",
               #nthread = 3
)
 y_pred <- predict(xgb, data.matrix(datause011[,-1]))
y <- as.data.frame(y_pred)
write.csv(y, "predictxgboost.csv")

set.seed(1)
index <- sample(1:nrow(datause01), 6095)
test <- datause01[index, ]
train <- datause01[-index,]
dtrain <- xgb.DMatrix(data.matrix(train[,-2]), label=train[,1])
dtest <- xgb.DMatrix(data.matrix(test[,-2]), label=test[,1])
watchlist <- list(train=dtrain, test=dtest)


bst <- xgb.train(data=dtrain, max.depth=10, eta=0.05, nthread = 2, nround=240, watchlist=watchlist, objective = "reg:linear", subsample = 1, min_child_weight=2)
 
y_predadv <- predict(bst, data.matrix(datause011[,-1]))              
y1 <- data.frame(y_predadv)
write.csv(y, "predictxgboostadv.csv")

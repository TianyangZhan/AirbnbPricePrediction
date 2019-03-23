library(ggplot2)

#### Read in data ####
lsts <- read.csv("listings.csv", row.names = 1, header= TRUE)
features <- c("price","property_type","room_type","bed_type", "accommodates", "bathrooms", "bedrooms", "beds", "extra_people", "longitude", "neighbourhood_cleansed")
lsts <- lsts[features]


#### Removing error values and '$' and ',' sign in data ####
lsts[lsts == ""] <- NA
stonum <- c("price", "accommodates", "bathrooms", "bedrooms", "beds", "extra_people","longitude")
lsts[stonum] <- suppressWarnings(lapply(lsts[stonum], function(x) as.numeric(gsub("[,$]", "", x))))


#### Getting information about the missing data in the training set ####
index <- sapply(lsts,function(x) sum(is.na(x)))
smry <- data.frame(index = names(lsts),val=index)
#as.matrix(smry[smry$val < 100,])


##### Replacing NA #####
lsts$bathrooms[is.na(lsts$bathrooms)] <- 1
lsts$bedrooms[is.na(lsts$bedrooms)] <- 1
lsts$beds[is.na(lsts$beds)] <- 1


#### Data Visualization ###
plot(density(lsts$price), xlim = c(0,2000))
x<-seq(0,1000,by=50)
curve(dgamma(x,3.5,0.02), from=0, to=2000, n=500, col="red", add = TRUE)

ggplot(lsts, aes(y = price, x = accommodates*(beds+bedrooms+bathrooms))) + geom_point()
ggplot(lsts, aes(y = price, x = accommodates*beds*bedrooms*bathrooms))+ coord_fixed(ratio = 1, xlim = c(0,2000), ylim = c(0,2000)) + geom_point()

#### Factorization ####
lsts$room_type <- as.numeric(lsts$room_type)
lsts$bed_type <- as.numeric(lsts$bed_type)

poscode <- as.integer(lsts$property_type)
lsts$house <- as.integer(poscode == 2 | poscode == 6 | poscode == 16 | poscode == 24  | poscode == 10)
lsts$other <- as.integer(poscode == 14 | poscode == 17 | poscode == 19 | poscode == 22)

poscode <- as.integer(lsts$neighbourhood_cleansed)

lsts$area1 <- as.integer(poscode == 4 | poscode == 27 | poscode == 18 | poscode == 13 | poscode == 15 | poscode == 26 | poscode == 20) #~260
lsts$area2 <- as.integer(poscode == 35 | poscode == 17 | poscode == 10 | poscode == 3 | poscode == 32 | poscode == 24 | poscode == 23 | poscode == 2 | poscode == 9 | poscode == 11 | poscode == 12 | poscode == 19 | poscode == 33 | poscode == 36 | poscode == 6 | poscode == 16 | poscode == 25 | poscode == 29 | poscode == 30 | poscode == 31) #~above


#### Sampling traning set and testing set from data ####
set.seed(9999)
idxs <- sample(1:nrow(lsts),1000)
tstdta <- lsts[idxs,]
trndta <- lsts[-idxs,]

threshold1 <- 0
threshold2 <- qgamma(0.99,3.5,0.02)
trndta <- trndta[trndta$price > threshold1 & trndta$price < threshold2,] # remove noisy data


#### Constructing model ####
model <- lm(price ~ accommodates*(beds+bedrooms+bathrooms) + (accommodates*beds*bedrooms*bathrooms) + longitude + extra_people + house + other + room_type + bed_type + area1 + area2, data = trndta)
#summary(model)

#### Prediction: values, error, and visualization ####
prediction <- predict(model, tstdta)
actuals_preds <- data.frame(cbind(actuals=tstdta$price, predicteds=prediction))
mae <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))) # mean absolute prediction error
mape <- 100 * mean(abs((actuals_preds$predicteds - actuals_preds$actuals)/actuals_preds$actuals)) # mean absolute percentage error
sprintf("mean absolute prediction error: %s", mae)
sprintf("mean absolute percentage error: %s%%", mape)

ggplot(actuals_preds, aes(y = actuals, x = predicteds)) + coord_fixed(ratio = 1, xlim = c(0,1500), ylim = c(0,1500)) + geom_point() + geom_abline(intercept=0,slope=1,size=1,colour='#0000FF')

############# EDA: LOAN CASE STUDY ###############################
# Submitted by: Mandar Jamble [Applicant ID = APFE1785873]

################# ASSUMPTIONS ####################################
# 1. This code is developed & run on Windows 10 platform
# 2. Working directory is correctly set
# 3. All files are present in the working directory
# 4. All file names are maintained as per UpGrad website

################# Business Problem #####################
# 1. Understand factors on which car pricing in US market
# 2. Which variables are significant in predicting car prices
# 3. How well those variables describe price of car
# 4. Model the car price with significant predictor variables 

install.packages("MASS") # For StepAIC
install.packages("car") # For VIF function

library("MASS")
library("car")

library(reshape2)
library(ggplot2)
library(scales)
library(stringr)

# Load car dataset
cars_data <- read.csv("CarPrice_Assignment.csv", header = T, na.strings = c("", "NA"), stringsAsFactors = F)
str(cars_data)  # Some categorical variables need to be coerced into factors

# Split CarName
cars_data$brand <- str_split_fixed(cars_data$CarName, pattern = " ", 2)[,1]
cars_data$brand <- tolower(cars_data$brand)
unique(cars_data$brand) # Multiple spellings found for porche, toyota, volkswagen

# Data cleaning for car-maker names
cars_data$brand <- gsub("toyouta", "toyota", cars_data$brand)
cars_data$brand <- gsub("porcshce", "porsche", cars_data$brand)
cars_data$brand <- gsub("maxda", "mazda", cars_data$brand)
cars_data$brand <- gsub("vokswagen", "vw", cars_data$brand)
cars_data$brand <- gsub("volkswagen", "vw", cars_data$brand)

# Coerse categorical variables into factors
cars_data[c(2, 4:9, 15:16, 18, 27)] <- lapply(cars_data[c(2, 4:9, 15:16, 18, 27)], factor) 
str(cars_data) 

# Cursory check for duplicates on trips
sum(duplicated(cars_data$car_ID)) # No duplicates

# NA check
sum(is.na(cars_data)) # No NA

# Blank check
sapply(cars_data, function(x) length(which(x == ""))) # No blanks

# Outlier identification
cars_data_melt <- melt(cars_data, value.name = "value")
cars_data_melt <- subset(cars_data_melt, cars_data_melt$variable!="car_ID")
cars_data_melt <- subset(cars_data_melt, cars_data_melt$variable!="price")
outlier_plots <- ggplot(data = cars_data_melt, aes(x=variable, y=value)) + geom_boxplot(aes(fill=value)) + facet_wrap( ~ variable, scales="free")
outlier_plots # outliers with many variables

# Outlier treatment 
quantile(cars_data$enginesize, seq(0,1,0.01))
cars_data$enginesize[which(cars_data$enginesize>183.0)] <- 183.0

quantile(cars_data$stroke, seq(0,1,0.01))
cars_data$stroke[which(cars_data$stroke<2.64)] <- 2.64 

quantile(cars_data$stroke, seq(0,1,0.01))
cars_data$stroke[which(cars_data$stroke>3.64)] <- 3.64 

quantile(cars_data$highwaympg, seq(0,1,0.01))
cars_data$highwaympg[which(cars_data$highwaympg>46.92)] <- 46.92

# Scatter plot car-prices to determine noise in the dataset
prices_dist_plot <- ggplot(data = cars_data, aes(car_ID, price)) + geom_point()
prices_dist_plot

# Scatter plot car-prices to determine any inherent pattern 
prices_trend_plot <- ggplot(data = cars_data, aes(car_ID, price)) + geom_line(aes(colour = "blue" )) +
 scale_x_continuous(name = "Car-ID", breaks = seq(0,205,20), limits = c(0,205)) + 
 scale_y_continuous(name = "Price", breaks = seq(0,50000,5000), limits = c(0,50000))
prices_trend_plot # No pattern identified in car prices other than spikes due to car-maker brand.

# No Derived metrics required
# Binning for continuous not required

# Dummy coding for categorical variables with 2 levels
levels(cars_data$fueltype) <- c(1,0)
cars_data$fueltype <- as.numeric(levels(cars_data$fueltype))[cars_data$fueltype]

levels(cars_data$aspiration) <- c(1,0)
cars_data$aspiration <- as.numeric(levels(cars_data$aspiration))[cars_data$aspiration]

levels(cars_data$doornumber) <- c(1,0)
cars_data$doornumber <- as.numeric(levels(cars_data$doornumber))[cars_data$doornumber]

levels(cars_data$enginelocation) <- c(1,0)
cars_data$enginelocation <- as.numeric(levels(cars_data$enginelocation))[cars_data$enginelocation]

# Dummy coding for categorical variables with > 2 levels
dummy_1 <- data.frame(model.matrix(~carbody, data=cars_data))
dummy_1 <- dummy_1[,-1]
cars_data_1 <- cbind(cars_data[,-7],dummy_1)

dummy_1 <- data.frame(model.matrix(~drivewheel, data=cars_data))
dummy_1 <- dummy_1[,-1]
cars_data_1 <- cbind(within(cars_data_1, rm(drivewheel)),dummy_1)

dummy_1 <- data.frame(model.matrix(~enginetype, data=cars_data))
dummy_1 <- dummy_1[,-1]
cars_data_1 <- cbind(within(cars_data_1, rm(enginetype)),dummy_1)

dummy_1 <- data.frame(model.matrix(~cylindernumber, data=cars_data))
dummy_1 <- dummy_1[,-1]
cars_data_1 <- cbind(within(cars_data_1, rm(cylindernumber)),dummy_1)

dummy_1 <- data.frame(model.matrix(~fuelsystem, data=cars_data))
dummy_1 <- dummy_1[,-1]
cars_data_1 <- cbind(within(cars_data_1, rm(fuelsystem)),dummy_1)

dummy_1 <- data.frame(model.matrix(~brand, data=cars_data))
dummy_1 <- dummy_1[,-1]
cars_data_1 <- cbind(within(cars_data_1, rm(brand)),dummy_1)

dummy_1 <- data.frame(model.matrix(~symboling, data=cars_data))
dummy_1 <- dummy_1[,-1]
cars_data_1 <- cbind(within(cars_data_1, rm(symboling)),dummy_1)

cars_data_1 <- within(cars_data_1, rm(CarName))
cars_data_1 <- within(cars_data_1, rm(car_ID))

# Set seed
set.seed(100)

# Build train & test datasets
trainindices= sample(1:nrow(cars_data_1), 0.7*nrow(cars_data_1))
cars_train = cars_data_1[trainindices,]
cars_test = cars_data_1[-trainindices,]

# Build Linear Regression Models using backward approach (app predictor veriables)
model_1.0 <- lm(formula = price~., data = cars_train)
summary(model_1.0) # Too many variables for model building

# Run stepAIC to remove redundant independent variables
step <- stepAIC(model_1.0, direction="both")
summary(step)

vif(step)

# Iterative process to build models by removing one insignificant variable at a time
# Removing boreratio
model_1.1 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + carheight + curbweight + stroke + 
                  horsepower + citympg + carbodyhardtop + carbodyhatchback + 
                  carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
                  enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + brandbmw + brandbuick + branddodge + 
                  brandhonda + brandjaguar + brandmazda + brandmercury + brandmitsubishi + 
                  brandnissan + brandplymouth + brandrenault + brandsaab + 
                  brandtoyota + brandvolvo + brandvw + symboling1, data = cars_train)
summary(model_1.1)
vif(model_1.1)

# Removing citympg
model_1.2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + carheight + curbweight + stroke + 
                  horsepower + carbodyhardtop + carbodyhatchback + 
                  carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
                  enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + brandbmw + brandbuick + branddodge + 
                  brandhonda + brandjaguar + brandmazda + brandmercury + brandmitsubishi + 
                  brandnissan + brandplymouth + brandrenault + brandsaab + 
                  brandtoyota + brandvolvo + brandvw + symboling1, data = cars_train)
summary(model_1.2)
vif(model_1.2)

# Removing carheight
model_1.3 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + curbweight + stroke + 
                  horsepower + carbodyhardtop + carbodyhatchback + 
                  carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
                  enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + brandbmw + brandbuick + branddodge + 
                  brandhonda + brandjaguar + brandmazda + brandmercury + brandmitsubishi + 
                  brandnissan + brandplymouth + brandrenault + brandsaab + 
                  brandtoyota + brandvolvo + brandvw + symboling1, data = cars_train)
summary(model_1.3)
vif(model_1.3)

# Removing brandvolvo
model_1.4 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + curbweight + stroke + 
                  horsepower + carbodyhardtop + carbodyhatchback + 
                  carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
                  enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + brandbmw + brandbuick + branddodge + 
                  brandhonda + brandjaguar + brandmazda + brandmercury + brandmitsubishi + 
                  brandnissan + brandplymouth + brandrenault + brandsaab + 
                  brandtoyota + brandvw + symboling1, data = cars_train)
summary(model_1.4)
vif(model_1.4)

# Removing enginetypedohcv
model_1.5 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + curbweight + stroke + 
                  horsepower + carbodyhardtop + carbodyhatchback + 
                  carbodysedan + carbodywagon + drivewheelrwd + 
                  enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + brandbmw + brandbuick + branddodge + 
                  brandhonda + brandjaguar + brandmazda + brandmercury + brandmitsubishi + 
                  brandnissan + brandplymouth + brandrenault + brandsaab + 
                  brandtoyota + brandvw + symboling1, data = cars_train)
summary(model_1.5)
vif(model_1.5)

# Removing carbodyhardtop
model_1.6 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                  carwidth + curbweight + stroke + 
                  horsepower + carbodyhatchback + 
                  carbodysedan + carbodywagon + drivewheelrwd + 
                  enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + brandbmw + brandbuick + branddodge + 
                  brandhonda + brandjaguar + brandmazda + brandmercury + brandmitsubishi + 
                  brandnissan + brandplymouth + brandrenault + brandsaab + 
                  brandtoyota + brandvw + symboling1, data = cars_train)
summary(model_1.6)
vif(model_1.6)


# Removing carlength
model_1.7 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + stroke + 
                  horsepower + carbodyhatchback + 
                  carbodysedan + carbodywagon + drivewheelrwd + 
                  enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + brandbmw + brandbuick + branddodge + 
                  brandhonda + brandjaguar + brandmazda + brandmercury + brandmitsubishi + 
                  brandnissan + brandplymouth + brandrenault + brandsaab + 
                  brandtoyota + brandvw + symboling1, data = cars_train)
summary(model_1.7)
vif(model_1.7)

# Removing symboling1
model_1.8 <- lm(formula = price ~ aspiration + enginelocation + 
                  carwidth + curbweight + stroke + 
                  horsepower + carbodyhatchback + 
                  carbodysedan + carbodywagon + drivewheelrwd + 
                  enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + brandbmw + brandbuick + branddodge + 
                  brandhonda + brandjaguar + brandmazda + brandmercury + brandmitsubishi + 
                  brandnissan + brandplymouth + brandrenault + brandsaab + 
                  brandtoyota + brandvw, data = cars_train)
summary(model_1.8)
vif(model_1.8)

# Removing aspiration
model_1.9 <- lm(formula = price ~ enginelocation + 
                  carwidth + curbweight + stroke + 
                  horsepower + carbodyhatchback + 
                  carbodysedan + carbodywagon + drivewheelrwd + 
                  enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + brandbmw + brandbuick + branddodge + 
                  brandhonda + brandjaguar + brandmazda + brandmercury + brandmitsubishi + 
                  brandnissan + brandplymouth + brandrenault + brandsaab + 
                  brandtoyota + brandvw, data = cars_train)
summary(model_1.9)
vif(model_1.9)

# Removing carbodysedan
model_1.10 <- lm(formula = price ~ enginelocation + 
                   carwidth + curbweight + stroke + 
                   horsepower + carbodyhatchback + 
                   carbodywagon + drivewheelrwd + 
                   enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmercury + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + brandsaab + 
                   brandtoyota + brandvw, data = cars_train)
summary(model_1.10)
vif(model_1.10)

# Removing carbodyhatchback
model_1.11 <- lm(formula = price ~ enginelocation + 
                   carwidth + curbweight + stroke + 
                   horsepower + carbodywagon + drivewheelrwd + 
                   enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmercury + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + brandsaab + 
                   brandtoyota + brandvw, data = cars_train)
summary(model_1.11)
vif(model_1.11)

# Removing brandmercury
model_1.12 <- lm(formula = price ~ enginelocation + 
                   carwidth + curbweight + stroke + 
                   horsepower + carbodywagon + drivewheelrwd + 
                   enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + brandsaab + 
                   brandtoyota + brandvw, data = cars_train)
summary(model_1.12)
vif(model_1.12)

# Removing stroke
model_1.13 <- lm(formula = price ~ enginelocation + 
                   carwidth + curbweight + 
                   horsepower + carbodywagon + drivewheelrwd + 
                   enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + brandsaab + 
                   brandtoyota + brandvw, data = cars_train)
summary(model_1.13)
vif(model_1.13)

# Removing carbodywagon
model_1.14 <- lm(formula = price ~ enginelocation + 
                   carwidth + curbweight + horsepower + drivewheelrwd + 
                   enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + brandsaab + 
                   brandtoyota + brandvw, data = cars_train)
summary(model_1.14)
vif(model_1.14)

# Removing brandsaab
model_1.15 <- lm(formula = price ~ enginelocation + 
                   carwidth + curbweight + horsepower + drivewheelrwd + 
                   enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + 
                   brandtoyota + brandvw, data = cars_train)
summary(model_1.15)
vif(model_1.15)


# Removing drivewheelrwd
model_1.16 <- lm(formula = price ~ enginelocation + 
                   carwidth + curbweight + horsepower + 
                   enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                   cylindernumbersix + brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + 
                   brandtoyota + brandvw, data = cars_train)
summary(model_1.16)
vif(model_1.16)

# Removing cylindernumbersix
model_1.17 <- lm(formula = price ~ enginelocation + 
                   carwidth + curbweight + horsepower + 
                   enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                   brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + 
                   brandtoyota + brandvw, data = cars_train)
summary(model_1.17)
vif(model_1.17)

# Removing curbweight
model_1.18 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + enginetypel + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                   brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + 
                   brandtoyota + brandvw, data = cars_train)
summary(model_1.18)
vif(model_1.18)

# Removing enginetypel
model_1.19 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                   brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + 
                   brandtoyota + brandvw, data = cars_train)
summary(model_1.19)
vif(model_1.19)

# Removing brandvw
model_1.20 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + enginetypeohcf + cylindernumberfive + cylindernumberfour + 
                   brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + 
                   brandtoyota, data = cars_train)
summary(model_1.20)
vif(model_1.20)

# Removing cylindernumberfour
model_1.21 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + enginetypeohcf + cylindernumberfive + 
                   brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + 
                   brandtoyota, data = cars_train)
summary(model_1.21)
vif(model_1.21)

# Removing cylindernumberfive
model_1.22 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + enginetypeohcf + 
                   brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + brandrenault + 
                   brandtoyota, data = cars_train)
summary(model_1.22)
vif(model_1.22)

# Removing brandrenault
model_1.23 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + enginetypeohcf + 
                   brandbmw + brandbuick + branddodge + 
                   brandhonda + brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + 
                   brandtoyota, data = cars_train)
summary(model_1.23)
vif(model_1.23)

# Removing brandhonda
model_1.24 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + enginetypeohcf + 
                   brandbmw + brandbuick + branddodge + 
                   brandjaguar + brandmazda + brandmitsubishi + 
                   brandnissan + brandplymouth + 
                   brandtoyota, data = cars_train)
summary(model_1.24)
vif(model_1.24)


# Removing brandmazda
model_1.25 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + enginetypeohcf + 
                   brandbmw + brandbuick + branddodge + 
                   brandjaguar + brandmitsubishi + 
                   brandnissan + brandplymouth + 
                   brandtoyota, data = cars_train)
summary(model_1.25)
vif(model_1.25)

# Removing brandnissan 
model_1.26 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + enginetypeohcf + 
                   brandbmw + brandbuick + branddodge + brandjaguar + brandmitsubishi + 
                   brandplymouth + brandtoyota, data = cars_train)
summary(model_1.26)
vif(model_1.26)

# Removing branddodge
model_1.27 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + enginetypeohcf + 
                   brandbmw + brandbuick + brandjaguar + brandmitsubishi + 
                   brandplymouth + brandtoyota, data = cars_train)
summary(model_1.27)
vif(model_1.27)

# Removing brandtoyota
model_1.28 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + enginetypeohcf + 
                   brandbmw + brandbuick + brandjaguar + brandmitsubishi + 
                   brandplymouth, data = cars_train)
summary(model_1.28)
vif(model_1.28)

# Removing enginetypeohcf
model_1.29 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + 
                   brandbmw + brandbuick + brandjaguar + brandmitsubishi + 
                   brandplymouth, data = cars_train)
summary(model_1.29)
vif(model_1.29)

# Removing brandplymouth
model_1.30 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + 
                   brandbmw + brandbuick + brandjaguar + 
                   brandmitsubishi, data = cars_train)
summary(model_1.30)
vif(model_1.30)

# Removing brandmitsubishi
model_1.31 <- lm(formula = price ~ enginelocation + 
                   carwidth + horsepower + 
                   brandbmw + brandbuick + brandjaguar, data = cars_train)
summary(model_1.31)
vif(model_1.31) # Best-fit model


# Model building stopped as following 3 conditions are met:
#   1. Difference between r-squared & adjusted r-squared is minimal 
#   2. All variables are highly significant and VIF scores are low.
#   3. Additional removal of predictor variable greatly impacts r-squared
# Model_1.31 describes car prices best with adjusted R-squared = 93.4%
# Most significant variables are:
# enginelocation, carwidth, horsepower, brandjaguar, brandbmw, brandbuick
# All coefficient are within residual range and have low p-values & VIF scores

# Prediction with Test-dataset
predict_1 <- predict(model_1.31, cars_test[,-1])
cars_test$predicted_price <- predict_1

# Calculate error = actual-price - predicted price
cars_test$error <- cars_test$price - cars_test$predicted_price

# Determine correlation between actual-price & predicted price
cars_corr <- cor(cars_test$price,cars_test$predicted_price)
rsquared <- cars_corr^2 
rsquared # Compares well with the R-aquared from best fit model

# Model validation: Graphical comparison between actual & predicted price of test dataset
predicted_prices_plot <- ggplot() + geom_line(data=cars_test, aes(x=as.numeric(row.names(cars_test)),y=price, colour="red"),size=1) +
  geom_line(data=cars_test, aes(x=as.numeric(row.names(cars_test)), y=predicted_price, colour="blue"),size=1) +
  scale_x_continuous(name = "ID", breaks = seq(0,65,5), limits = c(0,65)) +
  scale_y_continuous(name = "Price", breaks = seq(0,50000,5000), limits = c(0,50000)) +
  scale_color_discrete(name = "Car Price", labels = c("Actual", "Predicted"))
predicted_prices_plot # Close resemblence between actual & predicted price


# Model validation: Error check
error_plot <- ggplot(data = cars_test, aes(x=as.numeric(row.names(cars_test)),y=error)) + geom_point() + 
  scale_x_continuous(name = "Car-ID", breaks = seq(0,65,5), limits = c(0,65)) +
  scale_y_continuous(name = "Error", breaks = seq(-15000,15000,50000), limits = c(-15000,15000)) +
  geom_hline(yintercept = 0)
error_plot # Error distribution is white noise. No pattern observed

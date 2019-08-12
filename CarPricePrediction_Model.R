# installing/loading required packages

# install.packages("car")
# install.packages("MASS")

# load the package
library("car")
library(MASS)
library('tidyr')
library('dplyr') 
library('stringr')

CarPrice <-read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)

# Filtering company name
CarPrice$CarName <-gsub("([A-Za-z]+).*", "\\1", CarPrice$CarName)

# display the dataset to get a primary understanding of the various attributes
# present in the CarPrice dataset

View(CarPrice)

# Ensuring dataset imported is in correct format

str(CarPrice)

# Symboling Variable should be factor type but is in int by default.
CarPrice$symboling <- as.factor(CarPrice$symboling)

# CarName Variable should be factor type but is in Character by default.
CarPrice$CarName <- as.factor(CarPrice$CarName)
summary(CarPrice$CarName)
str(CarPrice$CarName)
levels(CarPrice$CarName)
CarPrice$CarName<-as.character(CarPrice$CarName)
CarPrice$CarName[which(CarPrice$CarName=='maxda')]<- 'mazda'
CarPrice$CarName[which(CarPrice$CarName=='Nissan')]<- 'nissan'
CarPrice$CarName[which(CarPrice$CarName=='toyouta')]<- 'toyota'
CarPrice$CarName[which(CarPrice$CarName=='porcshce')]<- 'porsche'
CarPrice$CarName[which(CarPrice$CarName=='vokswagen')]<- 'volkswagen'
CarPrice$CarName[which(CarPrice$CarName=='vw')]<- 'volkswagen'
CarPrice$CarName <- as.factor(CarPrice$CarName)
# Fueltype Variable should be factor type but is in Character by default.
CarPrice$fueltype <- as.factor(CarPrice$fueltype)

# aspiration Variable should be factor type but is in Character by default.
CarPrice$aspiration <- as.factor(CarPrice$aspiration)

# doornumber Variable should be factor type but is in Character by default.
CarPrice$doornumber <- as.factor(CarPrice$doornumber)

# carbody Variable should be factor type but is in Character by default.
CarPrice$carbody <- as.factor(CarPrice$carbody)

# drivewheel Variable should be factor type but is in Character by default.
CarPrice$drivewheel <- as.factor(CarPrice$drivewheel)

# enginelocation Variable should be factor type but is in Character by default.
CarPrice$enginelocation <- as.factor(CarPrice$enginelocation)

# enginetype Variable should be factor type but is in Character by default.
CarPrice$enginetype <- as.factor(CarPrice$enginetype)

# cylindernumber Variable should be factor type but is in Character by default.
CarPrice$cylindernumber <- as.factor(CarPrice$cylindernumber)

# fuelsystem Variable should be factor type but is in Character by default.
CarPrice$fuelsystem <- as.factor(CarPrice$fuelsystem)

# Check the structure of CarPrice dataset again to ensure that all the variables are
# in correct format now

str(CarPrice)

# remove duplicate values (if any) in the dataset
unique(CarPrice)

# check for missing values and treat if any
sum(is.na(CarPrice))

# check if there are outliers
quantile(CarPrice$boreratio,seq(0,1,0.01))
quantile(CarPrice$compressionratio,seq(0,1,0.01))
quantile(CarPrice$horsepower,seq(0,1,0.01))
quantile(CarPrice$peakrpm,seq(0,1,0.01))
quantile(CarPrice$citympg,seq(0,1,0.01))
quantile(CarPrice$highwaympg,seq(0,1,0.01))

# cap the values for variables
CarPrice$compressionratio[which(CarPrice$compressionratio > 10.9400)] <-10.9400
CarPrice$horsepower[which(CarPrice$horsepower > 184.00)] <- 184.00
CarPrice$peakrpm[which(CarPrice$peakrpm > 6000)] <- 6000
CarPrice$citympg[which(CarPrice$citympg > 38.00)] <- 38.00
CarPrice$highwaympg[which(CarPrice$highwaympg > 46.92)] <- 46.92

# create dummy variables to convert the categorical variable
dummy  <-model.matrix(~symboling - 1,data=CarPrice)
dummy1 <-model.matrix(~CarName - 1,data=CarPrice)
dummy2 <-model.matrix(~fueltype - 1,data=CarPrice)
dummy3 <-model.matrix(~aspiration - 1,data=CarPrice)
dummy4 <-model.matrix(~doornumber - 1,data=CarPrice)
dummy5 <-model.matrix(~carbody - 1,data=CarPrice)
dummy6 <-model.matrix(~drivewheel - 1,data=CarPrice)
dummy7 <-model.matrix(~enginelocation - 1,data=CarPrice)
dummy8 <-model.matrix(~enginetype - 1,data=CarPrice)
dummy9 <-model.matrix(~cylindernumber - 1,data=CarPrice)
dummy10 <-model.matrix(~fuelsystem - 1,data=CarPrice)
# in case of a factor with n levels, n-1 dummy variables are required. So, in this
# case, removing the first column from dummy
dummy  <- dummy[,-1]
dummy1 <- dummy1[,-1]
dummy2 <- dummy2[,-1]
dummy3 <- dummy3[,-1]
dummy4 <- dummy4[,-1]
dummy5 <- dummy5[,-1]
dummy6 <- dummy6[,-1]
dummy7 <- dummy7[,-1]
dummy8 <- dummy8[,-1]
dummy9 <- dummy9[,-1]
dummy10 <- dummy10[,-1]
#Now cbind the dummy variable with the CarPrice dataset
CarPrice<-cbind(CarPrice[,-2],dummy)
CarPrice<-cbind(CarPrice[,-2],dummy1)
CarPrice<-cbind(CarPrice[,-2],dummy2)
CarPrice<-cbind(CarPrice[,-2],dummy3)
CarPrice<-cbind(CarPrice[,-2],dummy4)
CarPrice<-cbind(CarPrice[,-2],dummy5)
CarPrice<-cbind(CarPrice[,-2],dummy6)
CarPrice<-cbind(CarPrice[,-2],dummy7)
CarPrice<-cbind(CarPrice[,-7],dummy8)
CarPrice<-cbind(CarPrice[,-7],dummy9)
CarPrice<-cbind(CarPrice[,-8],dummy10)
View(CarPrice)

# Removing Car-ID column 
CarPrice <- CarPrice[,-1]
# creating training and test data set
set.seed(100)
# randomly generate row indices for train dataset
trainindices= sample(1:nrow(CarPrice), 0.7*nrow(CarPrice))
# generate the train data set
train = CarPrice[trainindices,]
# store the rest of the observations into "test"
test = CarPrice[-trainindices,]

##############

#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)
# Check the summary of model.
summary(model_1)

# stepAIC makes multiple calls while checking which variables to keep
# Running it
step <- stepAIC(model_1, direction="both")
step

# store the last model equation of stepwise method into an object called model_2
model_2 <-lm(price ~ carwidth + curbweight + enginesize + boreratio + 
               stroke + compressionratio + horsepower + peakrpm + symboling3 + 
               CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
               CarNamesaab + CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
               dummy3 + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + dummy7 + enginetypeohc + enginetypeohcv + 
               enginetyperotor + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
               data = train)
summary(model_2)
vif(model_2)

# Removing CarNamesaab column due to high p-value=0.181238

model_3 <-lm(price ~ carwidth + curbweight + enginesize + boreratio + 
               stroke + compressionratio + horsepower + peakrpm + symboling3 + 
               CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
               CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
               dummy3 + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + dummy7 + enginetypeohc + enginetypeohcv + 
               enginetyperotor + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
               data = train)
summary(model_3)
vif(model_3)

# Removing fuelsystemmpfi column due to high p-value=0.180620 as well as VIF=9.341608

model_4 <-lm(price ~ carwidth + curbweight + enginesize + boreratio + 
               stroke + compressionratio + horsepower + peakrpm + symboling3 + 
               CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
               CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
               dummy3 + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + dummy7 + enginetypeohc + enginetypeohcv + 
               enginetyperotor + cylindernumberfour + fuelsystem2bbl , 
               data = train)
summary(model_4)
vif(model_4)

# Removing enginetypeohcv column due to high p-value=0.120859.

model_5 <-lm(price ~ carwidth + curbweight + enginesize + boreratio + 
               stroke + compressionratio + horsepower + peakrpm + symboling3 + 
               CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
               CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
               dummy3 + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + dummy7 + enginetypeohc + 
               enginetyperotor + cylindernumberfour + fuelsystem2bbl , 
             data = train)
summary(model_5)
vif(model_5)

# Removing stroke column due to high p-value=0.413508.

model_6 <-lm(price ~ carwidth + curbweight + enginesize + boreratio +
               compressionratio + horsepower + peakrpm + symboling3 + 
               CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
               CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
               dummy3 + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + drivewheelrwd + dummy7 + enginetypeohc + 
               enginetyperotor + cylindernumberfour + fuelsystem2bbl , 
             data = train)
summary(model_6)
vif(model_6)

# removing drivewheelrwd column due to high p-value=0.076431.

model_7 <-lm(price ~ carwidth + curbweight + enginesize + boreratio +
               compressionratio + horsepower + peakrpm + symboling3 + 
               CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
               CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
               dummy3 + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + dummy7 + enginetypeohc + 
               enginetyperotor + cylindernumberfour + fuelsystem2bbl , 
             data = train)
summary(model_7)
vif(model_7)

# removing symboling3 column due to high p-value=0.089758

model_8 <-lm(price ~ carwidth + curbweight + enginesize + boreratio +
               compressionratio + horsepower + peakrpm + 
               CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
               CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
               dummy3 + carbodyhardtop + carbodyhatchback + carbodysedan + 
               carbodywagon + dummy7 + enginetypeohc + 
               enginetyperotor + cylindernumberfour + fuelsystem2bbl , 
             data = train)
summary(model_8)
vif(model_8)

# removing carbodyhardtop column due to high p-value=0.072253

model_9 <-lm(price ~ carwidth + curbweight + enginesize + boreratio +
               compressionratio + horsepower + peakrpm + 
               CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
               CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
               CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
               CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
               dummy3 + carbodyhatchback + carbodysedan + 
               carbodywagon + dummy7 + enginetypeohc + 
               enginetyperotor + cylindernumberfour + fuelsystem2bbl , 
             data = train)
summary(model_9)
vif(model_9)

# removing carbodysedan column due to high p-value=0.246613 and vif=6.192520

model_10 <-lm(price ~ carwidth + curbweight + enginesize + boreratio +
                compressionratio + horsepower + peakrpm + 
                CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy3 + carbodyhatchback + 
                carbodywagon + dummy7 + enginetypeohc + 
                enginetyperotor + cylindernumberfour + fuelsystem2bbl , 
              data = train)
summary(model_10)
vif(model_10)

# removing carbodywagon column due to high p-value=0.185021

model_11 <-lm(price ~ carwidth + curbweight + enginesize + boreratio +
                compressionratio + horsepower + peakrpm + 
                CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy3 + carbodyhatchback + 
                 dummy7 + enginetypeohc + 
                enginetyperotor + cylindernumberfour + fuelsystem2bbl , 
              data = train)
summary(model_11)
vif(model_11)

# removing carbodyhatchback column due to high p-value=0.292814

model_12 <-lm(price ~ carwidth + curbweight + enginesize + boreratio +
                 compressionratio + horsepower + peakrpm + 
                 CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                 CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                 CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
                 CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                 dummy3 + 
                 dummy7 + enginetypeohc + 
                 enginetyperotor + cylindernumberfour + fuelsystem2bbl , 
               data = train)
summary(model_12)
vif(model_12)

#removing fuelsystem2bbl column due to high p-value=0.081526

model_13 <-lm(price ~ carwidth + curbweight + enginesize + boreratio +
                compressionratio + horsepower + peakrpm + 
                CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                CarNameisuzu + CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy3 + 
                dummy7 + enginetypeohc + 
                enginetyperotor + cylindernumberfour , 
              data = train)
summary(model_13)
vif(model_13) 

# removing CarNameisuzu it seems less significant and check adjusted r value

model_14 <-lm(price ~ carwidth + curbweight + enginesize + boreratio +
                compressionratio + horsepower + peakrpm + 
                CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy3 + 
                dummy7 + enginetypeohc + 
                enginetyperotor + cylindernumberfour , 
              data = train)
summary(model_14)

# adjusted r has decreased a little
# lets remove peakrpm it seems less significant and check adjusted r value

model_15 <-lm(price ~ carwidth + curbweight + enginesize + boreratio +
                compressionratio + horsepower +  
                CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy3 + 
                dummy7 + enginetypeohc + 
                enginetyperotor + cylindernumberfour , 
              data = train)
summary(model_15)

# lets remove cylindernumberfour and check adjusted r value

model_16 <-lm(price ~ carwidth + curbweight + enginesize + boreratio +
                compressionratio + horsepower +  
                CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy3 + 
                dummy7 + enginetypeohc + 
                enginetyperotor , 
              data = train)
summary(model_16)

# lets remove boreratio column due to high p-value=0.083567

model_17 <-lm(price ~ carwidth + curbweight + enginesize +
                compressionratio + horsepower +  
                CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy3 + 
                dummy7 + enginetypeohc + 
                enginetyperotor , 
              data = train)
summary(model_17)

# removing enginetyperotor due to less significant

model_18 <-lm(price ~ carwidth + curbweight + enginesize +
                compressionratio + horsepower +  
                CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNameporsche + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy3 + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_18) 
vif(model_18)
# removing CarNameporsche due to less significant

model_19 <-lm(price ~ carwidth + curbweight + enginesize +
                compressionratio + horsepower +  
                CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy3 + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_19) 
vif(model_19)

# removing dummy3 due to less significant
model_20 <-lm(price ~ carwidth + curbweight + enginesize +
                compressionratio + horsepower +  
                CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_20) 
vif(model_20)

# removing horsepower due to less significant

model_21 <-lm(price ~ carwidth + curbweight + enginesize +
                compressionratio +  
                CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_21) 
vif(model_21)

# removing compressionratio due to less significant
model_22 <-lm(price ~ carwidth + curbweight + enginesize +
                CarNamebmw + CarNamebuick + CarNamedodge + CarNamehonda + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_22) 
vif(model_22)

# removing CarNamehonda due to less significant
model_23 <-lm(price ~ carwidth + curbweight + enginesize +
                CarNamebmw + CarNamebuick + CarNamedodge + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_23) 
vif(model_23)

# removing CarNamedodge due to less significant
model_24 <-lm(price ~ carwidth + curbweight + enginesize +
                CarNamebmw + CarNamebuick + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + CarNamevolkswagen + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_24) 
vif(model_24)

# removing CarNamevolkswagen due to less significant
model_25 <-lm(price ~ carwidth + curbweight + enginesize +
                CarNamebmw + CarNamebuick + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNameplymouth + CarNamerenault + 
                CarNamesubaru + CarNametoyota + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_25) 
vif(model_25)

# rmoving CarNameplymouth due to less significant

model_26 <-lm(price ~ carwidth + curbweight + enginesize +
                CarNamebmw + CarNamebuick + 
                CarNamemazda + CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNamerenault + 
                CarNamesubaru + CarNametoyota + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_26) 
vif(model_26)

# removing CarNamemazda due to less significant
model_27 <-lm(price ~ carwidth + curbweight + enginesize +
                CarNamebmw + CarNamebuick + 
                CarNamemitsubishi + CarNamenissan + 
                CarNamepeugeot + CarNamerenault + 
                CarNamesubaru + CarNametoyota + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_27) 
vif(model_27)

# removing CarNamenissan due to less significant

model_28 <-lm(price ~ carwidth + curbweight + enginesize +
                CarNamebmw + CarNamebuick + 
                CarNamemitsubishi +
                CarNamepeugeot + CarNamerenault + 
                CarNamesubaru + CarNametoyota + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_28) 
vif(model_28)

# removing CarNamemitsubishi due to less significant
model_29 <-lm(price ~ carwidth + curbweight + enginesize +
                CarNamebmw + CarNamebuick + 
                CarNamepeugeot + CarNamerenault + 
                CarNamesubaru + CarNametoyota + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_29) 
vif(model_29)

# removing CarNamerenault due to less significant
model_30 <-lm(price ~ carwidth + curbweight + enginesize +
                CarNamebmw + CarNamebuick + 
                CarNamepeugeot +
                CarNamesubaru + CarNametoyota + 
                dummy7 + enginetypeohc , 
              data = train)
summary(model_30) 
vif(model_30)

# predicting the results in test dataset 
Predict_1 <- predict(model_30,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted price. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

# rsquared=0.848567
# R-squared is always between 0 and 100%:
  
#  0% indicates that the model explains none of the variability of the response data around its mean.
#100% indicates that the model explains all the variability of the response data around its mean.
#In general, the higher the R-squared, the better the model fits your data.


# normal probability plot of the residuals

plot(model_1) # R default plot
plot(density(resid(model_1))) #A density plot
qqnorm(resid(model_1)) # A quantile normal plot - good for checking normality
qqline(resid(model_1))


plot(model_30) # R default plot
plot(density(resid(model_30))) #A density plot
qqnorm(resid(model_30)) # A quantile normal plot - good for checking normality
qqline(resid(model_30))
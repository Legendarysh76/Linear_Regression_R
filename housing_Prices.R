# Case II
# Oluwaseyi Henry Orioye

##################################################################################
#.......................STEP ZERO - PRE-PROCESSING...............................
##################################################################################
# Pre-Process the original Data (house_price.csv) file in excel.
# The purpose of this step is to transform all variables into
# numerical variables by creating Dummy Variables.
# See the formulas in the excel file (house_price.xlsx) for how to do this
# This step is assumed completed prior to proceeding to Step one.
# The rest of the analysis is done on the pre-processed
# dataset (house_Price_Processed.csv).

##########################################################################################
#.......................STEP ONE - INSTALL-PACKAGES and LOAD DATA.....................
##########################################################################################
options("install.lock" = FALSE) # this is to prevent package installation error


install.packages("mice") # for multiple imputation (step two)
library(mice)

install.packages("caTools") # for sample splitting into train and test subsets (step three)
library(caTools)

install.packages("usdm") # to test for multicollinearity (step four)
library(usdm)

install.packages("car") # to test for independence of residuals (step four)
library(car)


# Read in the data
Housing <- read.csv(file.choose(), header = TRUE) # file is house_Price_Processed.csv

# Convert the data to a data frame (R converts .csv files to a data frame automatically
# but it is a good idea to do so formally anyway (in case you're dealing with other file types)
Housing <- data.frame(Housing)

# inspect the data
View(Housing)

# get to know the data types of your columns - at this point all should be num (numerical) or int(integer)
str(Housing) 

# summary tells you number of missing values, among other things
summary(Housing)

# Linear Regression assumes that the response (dependent) variable is a numerical continuous variable
# If this is not the case, linear regression is not the appropriate methodology for 
# creating a predictive model - Logistic Regression may be appropriate instead.

#########################################################################################################
#.......................STEP TWO - DEAL WITH OUTLIER AND MISSING DATA.....................
#########################################################################################################
# identify and display the outliers
for (i in c(1,12))  
{
  
  q1 <- quantile(Housing[,i], 0.25, na.rm = TRUE)
  q3 <- quantile(Housing[,i], 0.75, na.rm = TRUE)
  iqr <- IQR(Housing[,i],na.rm = TRUE)
  outlier_rows <- subset(Housing, (Housing[,i] < q1-1.5*iqr)|(Housing[,i] > q3+1.5*iqr))
  if (nrow(outlier_rows) > 0)
  {
    print(colnames(Housing)[i])
    print(outlier_rows)
  }
}


# make a copy of the dataframe in case you decide to delete some of the rows with outliers
Housing_copy <- Housing


# remove specific rows if you decide that the outliers were there due to data entry error etc.
Housing <- Housing[-c(36, 288), ]
nrow(Housing) # number of rows: 498

# replace the remaining outliers with NAs (not availables)
for (i in c(1,12))
{
  q1 <- quantile(Housing[,i], 0.25, na.rm = TRUE)
  q3 <- quantile(Housing[,i], 0.75, na.rm = TRUE)
  iqr <- IQR(Housing[,i],na.rm = TRUE)
  Housing[,i][(Housing[,i] < q1 - 1.5*iqr)|(Housing[,i] > q3+1.5*iqr)] <- NA
}

summary(Housing_copy) # original NAs from missing values

summary(Housing) # more NAs from the outliers being replaced by NAs 

Housing <- complete(mice(Housing)) # fill in all missing (NA) values (originally missing and those from the NAs for the outliers) with imputed values

summary(Housing) # note: no more missing values

##################################################################################################################################
#.......................STEP THREE - SPLIT THE SAMPLE INTO TRAINING AND TESTING SETS...............
##################################################################################################################################
# set a common seed (for replicability of results) - do this for all assignments
set.seed(123)

# do the random split of rows in a 70/30 ratio
split <- sample.split(Housing$Sell_Price, SplitRatio = 0.7) # note: you need to specify the response variable Sell_Price in the first parameter

# create the training set
train <- subset(Housing, split == TRUE)

# create the testing set
test <- subset(Housing, split == FALSE)

# check the split has occured
nrow(train) # 348 (i.e. ~70% of 498)
nrow(test)  # 150 (the remaining ~30%)

################################################################################################################################
#........................STEP FOUR - MODEL INTERNAL VALIDATION AND VARIABLE TRANSFORMATION............................ 
############################################ MODEL 1 ###########################################################################
# Model with All Variables
model1 <- lm(Sell_Price ~. , data=train)
summary(model1)

# need to extract just the predictor variables from the dataframe
indep_vars <- subset(Housing, select = -1) # exclude column 1 (the response variable)

str(indep_vars) # note: Sell_Price (the response variable) is no longer there

usdm::vif(indep_vars) # all values are less than 5 (10 or more would be worrisome - between 5 and 10 of concern) - so mc not an issue

# Assumption 1: linearity of relationship
# create scatterplots of response vs. predictor
pairs(Housing)
plot(Housing$Lot_Size, Housing$Sell_Price)

# Assumption 2: independence of residuals
durbinWatsonTest(model1) 

# assumption 3: equality of residual variances to check for equality of residual variances(homoscedasticity) we must plot the residuals against the predicted
model_pred <- fitted(model1)
plot(model_pred, model1$residuals)

# assumption #4: normality of residuals visual
hist(model1$residuals)
qqnorm(model1$residuals) 
qqline(model1$residuals) 

############################################## MODEL 2 ###########################################################
# The updated model - dropped all insignificant variables
model2 <- lm(Sell_Price ~ Lot_Size + BDRMS  + BTHRMS + RECRM + PREF_Loc, data=train)
summary(model2)

# Assumption 1: linearity of relationship
# create scatterplots of response vs. predictor
pairs(Housing)
plot(Housing$Lot_Size, Housing$Sell_Price)

# Assumption 2: independence of residuals
durbinWatsonTest(model2) 

# assumption 3: equality of residual variances to check for equality of residual variances(homoscedasticity) we must plot the residuals against the predicted
model_pred <- fitted(model2)
plot(model_pred, model2$residuals)

# assumption #4: normality of residuals visual
hist(model2$residuals)
qqnorm(model2$residuals) 
qqline(model2$residuals) 

################################################ MODEL 3 WITH VARIABLE TRANSFORMATION ################################
# New model with transformation
model3 <- lm(log(log(Sell_Price)) ~ log(log(Lot_Size)) + BDRMS  + PREF_Loc, data = train)
summary(model3) 

# Assumption 1: linearity of relationship
# create scatterplots of response vs. predictor
pairs(Housing)

# Assumption 2: independence of residuals
durbinWatsonTest(model3) 

# assumption 3: equality of residual variancesto check for equality of residual variances(homoscedasticity) we must plot the residuals against the predicted
model_pred <- fitted(model3)
plot(model_pred, model3$residuals)


# assumption #4: normality of residuals visual
hist(model3$residuals)
qqnorm(model3$residuals) 
qqline(model3$residuals) 

#################################################################################################################################################
#........................STEP FIVE - MODEL EXTERNAL VALIDATION............................ 
#################################################################################################################################################
transformed_predictions <- predict(model3, newdata = test) 

actual_predictions <- exp(exp(transformed_predictions)) # note: we restore to the actual predictions (i.e. Sell_Price instead of log(log(Sell_price))
actual_predictions

# first compute the sum of squares of errors (SSE) on the test 
SSE <- sum((test$Sell_Price-actual_predictions)^2) 
SSE

# next compute the total sum of squares (SST) 
SST <- sum((test$Sell_Price- mean(test$Sell_Price))^2)
SST

# now compute the model fit on the test set, R-squared = 1-SSE/SST
R_squared <- 1 - (SSE/SST)
R_squared # 79.49%
# externally validated since the test R-squared is close to the train R-squared

###########################################################################################################################################################
#........................STEP SIX - MAKE PREDICTIONS............................ 
###########################################################################################################################################################
# Predict the Selling Price of a two-floor, three-bedroom, two-bathroom house with a driveway, 
# full basement and recreation room, a two-car garage, gas heat and AC, sitting on a 6000 sq. ft. lot in a preferred location.
pred_data <- list(2,3,2,1,1,1,2,1,1,6000,1)

#Then converted to a data frame
pred_data <- data.frame(pred_data)

# we also need to assign names to the dataframe columns so they match with the 
colnames(pred_data) <- c("FLRS", "BDRMS", "BTHRMS", "DRVWY", "FL_BSMNT", "RECRM", "GARAGE_SZ", "GAS", "AC", "Lot_Size", "PREF_Loc")

# we first use the model to predict the response (which remember is log(log(sell_price)) for our final model)
prediction <- predict(model3, newdata = pred_data)

# restore to an actual prediction (i.e. Sell_Price)
actual_prediction <- exp(exp(prediction)) # actual Seell_Price predictions
actual_prediction # $275,304









library(readr)
library(tidyverse)
library(caTools)
library(mice)

setwd("/Users/liuxi/Desktop/Project/Analysis/input")

# ***** The imputation is based on the entire compliance data (n=4006) *****


# Read compliance data
comp = read_dta('https://www.dropbox.com/s/jri4jknenf2bhcu/compliance_data_2009.dta?dl=1')

comp = mutate(comp,
              NumOfVio = rowSums(dplyr::select(comp,childlab:mgmt),na.rm=FALSE))

#df_NAs = filter(comp,is.na(NumOfVio))
#comp_na = filter(comp,is.na(compl_pass))

# write.csv(comp,'pythontest.csv')

# Create a dataset removing rows containing NAs
df = filter(comp,!is.na(NumOfVio))

#df_NA_compl_pass = filter(df, is.na(compl_pass))

# set.seed(12)

# split the dataset into train (50%) and test(50%)
split <- sample.split(df,SplitRatio = 0.5)
train <- subset(df,split == 'TRUE')
test <- subset(df,split == 'FALSE')

# Convert compl_pass to factors, indicating the dependent variable is binomial
df$compl_pass <- as.factor(df$compl_pass)

# create the logistic model with the train dataset
# use total number of violations to predict compl_pass (0=pass)
model <- glm(compl_pass ~ NumOfVio, data = train, family = 'binomial')

# summarize the model
summary(model)

# Predict compl_pass using test dataset, the returned value is numeric
res <- predict(model,test,type='response')
# if the predicted value > 0.5, return 1
y_pred = ifelse(res > 0.5,1,0)

# Create a confusion matrix
confmatrix <- table(test$compl_pass, y_pred)
confmatrix

#Accuracy
(confmatrix[[1,1]]+confmatrix[[2,2]]) / sum(confmatrix)

# the accurary is greater than 90%.
# number of violations is a good predictor for compl_pass

######### Imputation of Missing Values in Compl_Pass
# select variables
df2 = dplyr::select(comp,fname,compl_pass,NumOfVio)

# check the missing values
md.pattern(df2)
# there are 1396 total missing values
# there are 936 data that only compl_pass is missing
# 148 data that only number of violation is missing
# 156 data that both numofvio and compl_pass are missing

#df2_na = filter(df2,is.na(compl_pass))
#df2_na_vio = filter(df2,is.na(NumOfVio))

# Set predictors for compl_pass: we only need num of vio
# the reason to have this step is because mice use multivariate imputation
imp = mice(df2,print=FALSE)
imp$predictorMatrix
pred = imp$predictorMatrix # e.g., num of violation is predicted by compl_pass | fanme is predicted by compl_pass and numofvio
pred[,'fname'] <- 0
pred[,'compl_pass'] <- 0
pred[1,'NumOfVio'] <- 0
pred

# Impute compl_pass
# make compl_pass a factor/categorical variable
df2$compl_pass <- as.factor(df2$compl_pass)
# use mice to impute missing values, the chosen method is logistic/logreg
temp_df <- mice(df2,predictorMatrix = pred,method = c("","logreg",""),seed = 500)
# Summary of the temp_df
summary(temp_df)


# Combine: fill the missing values with imputed values
completed_df <- complete(temp_df,1)
#xyplot(temp_df, compl_pass ~ NumOfVio, pch = 18, cex=1.2)

check = filter(completed_df,is.na(compl_pass))
# only 156 data is missing, 
# this number matches the # of observations which both 'numofvio' & 'compl_pass' are missing.

# Add the imputed column to the original dataset
comp = mutate(comp,
              imputed_compl_pass = completed_df$compl_pass)

#write.csv(comp,'imputed_compliance_data_2009.csv')


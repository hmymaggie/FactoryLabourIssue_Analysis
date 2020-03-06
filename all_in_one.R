library(readr)
library(tidyverse)
library(estimatr)
library(ggplot2)
library(haven)
library(psych)
library(anchors)
library(mice)
library(compare)
library(caTools)

# Table of Contents
# 0. Preparation
# 1. Managing compliance data: filling in NAs in compl_grade using two methods
# 2. Managing Economic performance data:
# 3. Merge compliance and economic data
# 4. Exploratory analysis


      ################## 0. Preparation ##################
#setwd('')
Sys.setlocale(locale="Chinese")

  ### 0.1 Read all Raw Data Files from input folder ###

# File 1: matched final deliverable. file obtained after matching process.
deliverable = read_csv("C:/Users/hmyma/Dropbox/factories_shared/build_was_analysis/input/Matched_Final Deliverable.csv") %>% 
  dplyr::rename(name = matched_names) %>% 
  mutate(deliv = 1)

# File 2: China dataset that includes all exconomic performance data
load('C:/Users/hmyma/Dropbox/factories_shared/build_was_analysis/input/china_full.RData')
china = d
rm(d)

china = china %>% 
  mutate(china = 1)

# File 3: Compliance/audit file
comp = read_dta('C:/Users/hmyma/Dropbox/factories_shared/build_was_analysis/input/compliance_data_2009.dta')


      ################## 1. Managing Compliance Data ##################

# Add total number of violations (sum of all individual labour violations) to compliance data
comp = mutate(comp,
              NumOfVio = rowSums(dplyr::select(comp,childlab:mgmt),na.rm=FALSE))

  ### 1.1 Build Logistic Model ###

# Create a dataset removing rows containing NAs
df = filter(comp,!is.na(NumOfVio))


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

  ### 1.2 Method 1: Imputation ###

# select variables used for imputation
df2 = dplyr::select(comp,fname,compl_pass,NumOfVio)

# check the missing values
md.pattern(df2)
# there are 1396 total missing values
# there are 936 data that only compl_pass is missing
# 148 data that only number of violation is missing
# 156 data that both numofvio and compl_pass are missing

# Set predictors for compl_pass: we only need num of vio
# the reason to have this step is because mice use multivariate imputation
imp = mice(df2,print=FALSE)
imp$predictorMatrix
pred = imp$predictorMatrix # e.g., num of violation is predicted by compl_pass | fanme is predicted by compl_pass and numofvio
pred[,'fname'] <- 0
pred[,'compl_pass'] <- 0
pred[1,'NumOfVio'] <- 0
pred

# make compl_pass a factor/categorical variable
df2$compl_pass <- as.factor(df2$compl_pass)

# Impute compl_pass: use mice to impute missing values, the chosen method is logistic/logreg
temp_df <- mice(df2,predictorMatrix = pred,method = c("","logreg",""),seed = 500)

# Summary of the temp_df
summary(temp_df)

# Combine: fill the missing values with imputed values
completed_df <- complete(temp_df,1)

# check = filter(completed_df,is.na(compl_pass))
# only 156 data is missing, 
# this number matches the # of observations which both 'numofvio' & 'compl_pass' are missing.

# Add the imputed column to the original dataset
comp = mutate(comp,
              imputed_compl_pass1 = completed_df$compl_pass)

  ### 1.3 Method 2: Imputation ###
# We used n = 527 to compute, will use this method after cleaning economic data
# Find in section 3

      ################## 2. Managing Economic Data ##################

# Merge the matched factories with the economic performance data from china_full.RData
merged_econ = full_join(china, deliverable, by = 'name')

# Add one more column to indicate whether it is a matched factory
merged_econ = mutate(merged_econ,
                     matched_or_not = ifelse(is.na(merged_econ$matched_countycode),"unmatched","matched"))

# save(merged_econ,file = 'economic_data_2009.RData')
# write.csv(merged,"merged_econ_data.csv",fileEncoding = 'UTF-8')

# Select matched factories and name it as 'econ'
econ = filter(merged_econ,matched_or_not=="matched")

#save(econ,file = 'economic_data_2009_527.RData')
#write.csv(df,"economic_data_2009_527_what.csv",fileEncoding = 'UTF-8')

# Drop useless columns from econ data (7 columns from econ dataset)
drop_econ <- c('ftyaddrlow','ftynamelow1','ftynamelow2','name1','Notes','deliv','china')
econ <- econ[,!(names(econ) %in% drop_econ)]

# Re-label column names: be ready for the codebook
names(econ)[names(econ) == 'name'] <- 'chn_names'
names(econ)[names(econ) == 'province.x'] <- 'chn_province'
names(econ)[names(econ) == 'city'] <- 'chn_prefecture'
names(econ)[names(econ) == 'county.x'] <- 'chn_county'
names(econ)[names(econ) == 'township.x'] <- 'chn_township'
names(econ)[names(econ) == 'village'] <- 'chn_village'
names(econ)[names(econ) == 'province.y'] <- 'py_province'
names(econ)[names(econ) == 'county.y'] <- 'py_county'
names(econ)[names(econ) == 'township.y'] <- 'py_township'
names(econ)[names(econ) == 'name2'] <- 'extracted_fname'
names(econ)[names(econ) == 'prefecture'] <- 'py_prefecture'

      ################## 3. Merge Economic & Compliance data ##################

# Convert factory names to lower case and then fix the typos
econ = mutate(econ,
              nameformerge = tolower(econ$ftyname1))

comp = mutate(comp,
              nameformerge = tolower(comp$fname))

comp[comp$fcode=="ZHWECA","nameformerge"] <- "zhejiang wenzhou cangnan plastic packaging industrial co., ltd"
comp[comp$fcode=="DONHAI","nameformerge"] <- "dongguan hai xing apparel co., ltd."


# Merge two datasets and save it as .RData
complete_data = full_join(comp,econ,by='nameformerge')
#save(complete_data,file = 'completed_data_2009.RData')

  ### 1.3 Method 2: Imputation ###
# In this method, we compute the mean of each individual labour violation 
# and use the column mean to fill in the missing values in each column

# Select matched factories
m = filter(complete_data,matched_or_not == 'matched')

# fill in NAs with column means
for (col in which(names(m) == "childlab") : which(names(m) == 'mgmt')) {
  m[is.na(m[,col]),col] <- mean(unlist(m[,col]), na.rm = TRUE)
}

#check = filter(m,is.na(NumOfVio))
# Calculate the total # of violations
m = mutate(m, NumOfVio_withoutNAs = rowSums(dplyr::select(m,childlab:mgmt),na.rm=FALSE))

# Select variables to be used for imputation
df = dplyr::select(m,nameformerge,compl_pass,NumOfVio_withoutNAs)
md.pattern(df)

# Set predictors
imp = mice(df,print=FALSE)
imp$predictorMatrix
pred = imp$predictorMatrix
pred[,'nameformerge'] <- 0
pred[,'compl_pass'] <- 0
pred[1,'NumOfVio_withoutNAs'] <- 0
pred


# Impute compl_pass
df$compl_pass <- as.factor(df$compl_pass)
temp_df <- mice(df,predictorMatrix = pred,method = c("","logreg",""),seed = 500)
summary(temp_df)

# Combine
completed_df <- complete(temp_df,1)
#view(filter(completed_df,is.na(compl_pass)))

# added the imputed column back to table m
m = mutate(m,
           imputed_compl_pass2 = completed_df$compl_pass)

#check = dplyr::select(m,'nameformerge','compl_grade','compl_pass','imputed_compl_pass1','imputed_compl_pass2')
#ggplot(check)+
#  geom_bar(aes(x=imputed_compl_pass2))

#filter(data.frame(m$nameformerge,m$s_auditdate),duplicated(data.frame(m$nameformerge,m$s_auditdate)) == 'TRUE')

# add the new column 'imputed_compl_pass2' to our completed data
imputed_completed_data = full_join(x = complete_data, y = m[,c('nameformerge','imputed_compl_pass2','s_auditdate','NumOfVio_withoutNAs','compl_grade')], by = c('nameformerge', 's_auditdate','compl_grade'))

# Delete the row where a factory was audited twice in a day
imputed_completed_data = imputed_completed_data[!(imputed_completed_data$s_auditdate=='20-Jan-09' & imputed_completed_data$fcode == 'NINHEY' & imputed_completed_data$compl_grade == 'A'),]

# Add profitability ratio
imputed_completed_data = mutate(imputed_completed_data,
                                profitability_ratio = profit/sales)

# save(imputed_completed_data,file = 'imputed_completed_data_2009.RData')

### Codebook ###
# convert column names to rows
codebook = data.frame(colnames(imputed_completed_data))
# write.csv(codebook,'c_codebook.csv')

      ################## 4. Exploratory Analysis ##################
  
  ### 4.1 Create Summary Table (for econ data) ###
# Select columns to be examined in the summary of descriptive statistics
econ_des_df = dplyr::select(econ, est_year, acc_receivable:workforce)

# check if there are missing values in the dataframe: No missing values...
finding_NAs <- econ_des_df[rowSums(is.na(econ_des_df)) > 0,]
subset(econ_des_df,is.na(econ_des_df))
econ_des_df[!complete.cases(econ_des_df),]
econ_des_df %>% filter_all(any_vars(is.na(.)))

# create the descriptive table for the selected variables
d = psych::describe(econ_des_df)
d

# Convert index to column
# This is because variable name is used as index; the index will disappear when save it into csv file.
d <- cbind(variables = rownames(d),d)
#rownames(d) <- -1:nrow(d)

# Add percentiles to the summary table
d = mutate(d, 
           "percentile.25"= NA,
           "percentile.75" = NA)

for (i in seq_along(d$variables)) {
  for (j in colnames(econ_des_df)) {
    if (d$variables[i] == j) {
      d$percentile.25[i] = quantile(econ_des_df[[j]],0.25);
      d$percentile.75[i] = quantile(econ_des_df[[j]],0.75)
    }
  }
}

# write.csv(d,"descriptive.csv")

  ### 4.2 Explore Compliance Data, started with completed dataset(n=4005) ###

# Find the duplicates of observations: # How many times a factory has been audited in 2009?
duplicates = data.frame(table(imputed_completed_data$nameformerge))
# 2907 factories was audited; the audit numbers range from 1-7 times

# Visualize: the frequency of audit
ggplot(duplicates,aes(x=Freq)) +
  geom_histogram(binwidth = 1) +
  geom_text(stat='count',aes(label=..count..),vjust=-0.2)+
  scale_x_continuous("Freq",labels=as.character(duplicates$Freq),breaks = duplicates$Freq)+
  theme_minimal()+
  xlab("Audit Frequency")+
  ylab("Count of Factories")
# 2063 factories were audited only once
# 304 factories were audited more than once

# Attach audit number to each factory
imputed_completed_data = mutate(imputed_completed_data,
                                audit_num = NA)

for (j in seq_along(duplicated(imputed_completed_data$nameformerge))){
  if (duplicated(imputed_completed_data$nameformerge)[j]=="FALSE") {
    imputed_completed_data$audit_num[j] = 1
  } else if (isTRUE(duplicated(imputed_completed_data$nameformerge)[j])) {
    a = c(1:7);
    for (i in seq_along(a)){
      if(isFALSE(duplicated(imputed_completed_data$nameformerge)[j-a[i]])){
        imputed_completed_data$audit_num[j] = 1+i
        break
      }
    }
  }
}

# For those that were visited more than once: why?
# Select factories that were audited more than once
dup_only = imputed_completed_data[duplicated(imputed_completed_data$nameformerge) | duplicated(imputed_completed_data$nameformerge, fromLast = TRUE), ]
# Then select their first audit
dup_only_first = filter(dup_only,dup_only$audit_num == 1)
# Visualize the the first audit grade for each factory
ggplot(dup_only_first,aes(x=compl_grade)) +
  geom_histogram(stat = 'count') +
  geom_text(stat='count',aes(label=..count..),vjust=-0.4)
# Most of the factories achieved C: 525

# what about factories that were audited only once?
not_dup = anti_join(imputed_completed_data,dup_only,by='nameformerge')
ggplot(not_dup,aes(x=compl_grade)) +
  geom_histogram(stat = 'count') +
  geom_text(stat='count',aes(label=..count..),vjust=-0.4)
# Most of the factories achieved C: 1268

  ### Explore Compliance Data: matched dataset(n = 527) ###
# Select matched data (n=741 / unique n=527)
cd_matched = filter(imputed_completed_data,matched_or_not=="matched")

# visualization: Exported destination
ggplot(filter(cd_matched,audit_num==1),aes(x=export_destination))+
  geom_histogram(stat='count')+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2)+
  theme(axis.text.x=element_text(angle=90,hjust=1),panel.background = element_rect(fill="white"))

# check = filter(cd_matched,is.na(NumOfVio)) # 65 missing

# Visualization: counts of factories based on the total # of violations for grade C
# (31 factories were removed because of NAs)
ggplot(filter(cd_matched,cd_matched$compl_grade=="C"),aes(x=NumOfVio))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous("NumOfVio",labels=as.character(cd_matched$NumOfVio),breaks = cd_matched$NumOfVio)

# Create tables for each compl_grade
grade_c = filter(cd_matched,cd_matched$compl_grade=="C")
grade_b = filter(cd_matched,cd_matched$compl_grade=="B")
grade_a = filter(cd_matched,cd_matched$compl_grade=="A")
grade_d = filter(cd_matched,cd_matched$compl_grade=="D")

# Average # of violations for each grade (remove NAs)
avgvio <- cd_matched %>% group_by(compl_grade) %>%
  summarize(avgvio = mean(NumOfVio,na.rm = TRUE))

# Histogram: counts of visit (n=527)
dup_merged = data.frame(table(cd_matched$nameformerge))
ggplot(dup_merged,aes(x=Freq))+
  geom_histogram(binwidth = 1) +
  geom_text(stat='count',aes(label=..count..),vjust=-0.2)+
  scale_x_continuous("Freq",labels=as.character(dup_merged$Freq),breaks = dup_merged$Freq)+
  theme_minimal()+
  labs(x="Number of Visit",y="Counts of Factories")+
  ggtitle("Number of Visits")
# when n = 527, 367 factories were audited only once.

# What categories that have been violated the most (n = 741 / removed NAs)?
sum(cd_matched$childlab,na.rm = TRUE) #5
sum(cd_matched$ilab,na.rm = TRUE) #87
sum(cd_matched$forcedlab,na.rm = TRUE) #6
sum(cd_matched$discipline,na.rm = TRUE) #46
sum(cd_matched$discrim,na.rm = TRUE) #33
sum(cd_matched$health,na.rm = TRUE) #579
sum(cd_matched$env,na.rm = TRUE) #250
sum(cd_matched$wages,na.rm = TRUE) #419
sum(cd_matched$hours,na.rm = TRUE) #370
sum(cd_matched$assoc,na.rm = TRUE) #30
sum(cd_matched$legal,na.rm = TRUE) #191
sum(cd_matched$subcon,na.rm = TRUE) #51
sum(cd_matched$mgmt,na.rm = TRUE) #265
# This contains duplicates, so we may wnat to examine the average... see below

# Average audit points for factories that have been audited multiple times
# Select variables to be examined & averaged
e = dplyr::select(cd_matched,fcode:nameformerge)

# Average each individual labour violation by factory names
childlab <- e %>% group_by(nameformerge) %>%
  summarise(avg_childlab = mean(childlab))
ilab <- e %>% group_by(nameformerge) %>%
  summarise(avg_ilab = mean(ilab))
forcedlab <- e %>% group_by(nameformerge) %>%
  summarise(avg_forcedlab = mean(forcedlab))
discipline <- e %>% group_by(nameformerge) %>%
  summarise(avg_discipline = mean(discipline))
discrim <- e %>% group_by(nameformerge) %>%
  summarise(avg_discrim = mean(discrim))
health <- e %>% group_by(nameformerge) %>%
  summarise(avg_health = mean(health))
env <- e %>% group_by(nameformerge) %>%
  summarise(avg_env = mean(env))
wages <- e %>% group_by(nameformerge) %>%
  summarise(avg_wages = mean(wages))
hours <- e %>% group_by(nameformerge) %>%
  summarise(avg_hours = mean(hours))
assoc <- e %>% group_by(nameformerge) %>%
  summarise(avg_assoc = mean(assoc))
legal <- e %>% group_by(nameformerge) %>%
  summarise(avg_legal = mean(legal))
subcon <- e %>% group_by(nameformerge) %>%
  summarise(avg_subcon = mean(subcon))
mgmt <- e %>% group_by(nameformerge) %>%
  summarise(avg_mgmt = mean(mgmt))

# Visualize total # of violation for each individual labour violation
tribble(
  ~violation,~frequency,
  "childlab", sum(childlab$avg_childlab,na.rm = TRUE), #3.17
  "ilab", sum(ilab$avg_ilab,na.rm = TRUE), #57.36
  "forcedlab", sum(forcedlab$avg_forcedlab,na.rm = TRUE), #4.5
  'discipline', sum(discipline$avg_discipline,na.rm = TRUE), #35.17
  'discrim', sum(discrim$avg_discrim,na.rm = TRUE), #27.25
  'health', sum(health$avg_health,na.rm = TRUE), #424.381
  'env', sum(env$avg_env,na.rm = TRUE), # 188.28
  'wages', sum(wages$avg_wages,na.rm = TRUE), # 298.67
  'hours', sum(hours$avg_hours,na.rm = TRUE), # 256.53
  'assoc', sum(assoc$avg_assoc,na.rm = TRUE), #25.5
  'legal',sum(legal$avg_legal,na.rm = TRUE), # 154.53
  'subcon', sum(subcon$avg_subcon,na.rm = TRUE), #35.73
  'mgmt', sum(mgmt$avg_mgmt,na.rm = TRUE) # 196.63
) %>%
  ggplot(aes(x=reorder(violation,-frequency),y=frequency))+
  geom_bar(stat='identity') +
  geom_text(aes(label=round(frequency)),vjust=-0.25)+
  theme(axis.text.x=element_text(angle=90,hjust=1),panel.background = element_rect(fill="white"))


# averaging variables by 'nameformerge'
# by default, NAs are not removed
cd_matched$imputed_compl_pass2 <- as.numeric(as.character(cd_matched$imputed_compl_pass2))

afteravg_target <- cd_matched %>% group_by(nameformerge) %>%
  summarise(avg_numofvio = mean(NumOfVio_withoutNAs),
            profit = mean(profit),
            imp_bin_pass = mean(imputed_compl_pass2),
            bin_pass = mean(compl_pass),
            worst_imputed_compl_pass = min(imputed_compl_pass2),
            best_imputed_compl_pass = max(imputed_compl_pass2),
            Workforce = mean(workforce),
            Sales = mean(sales),
            Profitability = mean(profitability_ratio),
            Export = mean(export),
            Profit = mean(profit))

# graph: profit vs. avg_numofvio
ggplot(afteravg_target,aes(x=avg_numofvio,y=profit))+
  geom_point() +
  geom_smooth(method="auto",se=TRUE)
#scale_y_continuous(limit=c(-10000,10000))

# mean profit vs. avg_numofvio
mean_afteravg_target <- afteravg_target %>% 
  group_by(avg_numofvio) %>%
  summarise(meanprofit = mean(profit))

ggplot(mean_afteravg_target,aes(x=avg_numofvio,y=meanprofit)) +
  geom_point() +
  geom_smooth(method="auto",se=TRUE) + 
  ggtitle("auto/meanprofit vs avg_numofwio")

# boxplot: profit vs. worst compl_pass (0=fail)
qplot(x=as.factor(worst_imputed_compl_pass),
      y=profit,
      data=afteravg_target,
      geom='boxplot')

# the same boxplot as the previous one, zoomed in
ggplot(afteravg_target,aes(x=as.factor(worst_imputed_compl_pass),y=profit)) +
  geom_boxplot()+
  scale_y_continuous(limit=c(-10000,10000))+
  xlab("compl_pass(0=fail)")

  ### Split dataset: whether correlations can be found when avg_numofvio > or <= 3 ###
low = filter(afteravg_target,avg_numofvio <= 3)
high = filter(afteravg_target,avg_numofvio > 3)

# Graph: when avg_numofvio <=3
ggplot(low,aes(x=avg_numofvio,y=profit))+
  geom_point()+
  geom_smooth(method='lm')+
  scale_y_continuous(limit=c(-10000,10000))

lm_robust(profit~avg_numofvio,low)

# Graph: when avg_numofvio > 3
ggplot(high,aes(x=avg_numofvio,y=profit))+
  geom_point()+
  geom_smooth(method='lm')+
  scale_y_continuous(limit=c(-10000,10000))

lm_robust(profit~avg_numofvio,high)

# scatterplot: avg_numofvio vs profitability ratio
ggplot(afteravg_target,aes(x=avg_numofvio,y=Profitability))+
  geom_point(alpha = 0.2,color = 'steelblue1',shape=16)+
  geom_smooth(method='auto')+
  theme_minimal()
#lm_robust(avg_numofvio~Profitability,afteravg_target)

# scatterplot: avg_numofvio vs workforce
ggplot(afteravg_target,aes(x=avg_numofvio,y=Workforce))+
  geom_point(alpha = 0.2,color = 'steelblue1',shape=16)+
  geom_smooth(method='auto')+
  theme_minimal()
lm_robust(avg_numofvio~Workforce,afteravg_target)
# seems like there's a linear correlation between the two variables

# scatterplot: avg_numofvio vs export
ggplot(afteravg_target,aes(avg_numofvio,y=Export))+
  geom_point(alpha = 0.2,color = 'steelblue1',shape=16)+
  geom_smooth(method='auto')+
  theme_minimal()
#lm_robust(avg_numofvio~Export,afteravg_target)

# scatterplot: avg_numofvio vs sales
ggplot(afteravg_target,aes(avg_numofvio,y=Sales))+
  geom_point(alpha = 0.2,color = 'steelblue1',shape=16)+
  geom_smooth(method='auto')+
  theme_minimal()
#lm_robust(avg_numofvio~Sales,afteravg_target)

# boxplot: worst_imputed_compl_pass2 vs profit
ggplot(afteravg_target,aes(x=as.factor(worst_imputed_compl_pass),y=Profit)) +
  geom_boxplot()+
  scale_y_continuous(limit=c(-10000,10000))+
  xlab("compl_pass(0=fail)")

#boxplot: best_imputed_compl_pass2 vs profit
ggplot(afteravg_target,aes(x=as.factor(best_imputed_compl_pass),y=Profit)) +
  geom_boxplot()+
  scale_y_continuous(limit=c(-10000,10000))+
  xlab("compl_pass(0=fail)")




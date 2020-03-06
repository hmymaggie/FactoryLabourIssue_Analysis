library(readr)
library(tidyverse)
library(estimatr)
library(ggplot2)
library(haven)
library(psych)
library(anchors)
library(mice)

# Read files and data preparation

#imputed_comp = read_csv(file = "/Users/liuxi/Desktop/Project/Analysis/input/imputed_compliance_data_2009.csv")
#load('/Users/liuxi/Desktop/Project/Analysis/input/completed_data_2009.RData')

imputed_comp=read.csv('https://www.dropbox.com/s/mf23quaaseirjts/imputed_compliance_data_2009.csv?dl=1')
load(url('https://www.dropbox.com/s/djqrrp2m6ilic98/completed_data_2009.RData?dl=1'))

complete_data = mutate(complete_data,
                       imputed_compl_pass = imputed_comp$imputed_compl_pass)

complete_data = mutate(complete_data,
                       NumOfVio = rowSums(dplyr::select(complete_data,childlab:mgmt),na.rm=FALSE))

# Check profit issues: profit_operational + profit_other <= profit??? no...
econ = read_csv('https://www.dropbox.com/s/5fspf4gx79e806j/economic_data_2009_527.csv?dl=1')
econ = mutate(econ,
              test_grossprofit = profit_other + profit_operational)

econ = mutate(econ,
              check = NA)

for (i in seq_along(econ$matched_or_not)) {
  if (abs(econ$test_grossprofit[i]) <= abs(econ$profit[i])) {
    econ$check[i] = TRUE
  }
}
f = filter(econ,is.na(econ$check))
# 232 factories have test_gross profit > profit


# Select matched factories
m = filter(complete_data,matched_or_not == 'matched')

# fill in NAs with means
for (col in which(names(m) == "childlab") : which(names(m) == 'mgmt')) {
  m[is.na(m[,col]),col] <- mean(unlist(m[,col]), na.rm = TRUE)
}

# Calculate the total # of violations
m = mutate(m, NumOfVio_withoutNAs = rowSums(dplyr::select(m,childlab:mgmt),na.rm=FALSE))


############### IMPUTATION of NAs in compl_pass using MICE

# Select variables to be used for imputation
df = dplyr::select(m,nameformerge,compl_pass,NumOfVio_withoutNAs)
md.pattern(df)

#df2_na = filter(df2,is.na(compl_pass))
#df2_na_vio = filter(df2,is.na(NumOfVio))

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

#check = dplyr::select(m,'nameformerge','compl_grade','compl_pass','imputed_compl_pass','imputed_compl_pass2')

################ Correlation examination
#add profitability ratio
m = mutate(m,
           profitability_ratio = profit / sales)
           #gross_profit = profit_other + profit_operational)

# Average by name
avg <- m %>% group_by(nameformerge) %>%
  summarise(avg_numofvio = mean(NumOfVio_withoutNAs),
            imp_bin_pass2 = mean(as.numeric(as.character(imputed_compl_pass2)),na.rm = FALSE),
            bin_pass = mean(compl_pass, na.rm = FALSE),
            worst_imputed_compl_pass2 = min(as.numeric(as.character(imputed_compl_pass2)), na.rm = FALSE),
            best_imputed_compl_pass2 = max(as.numeric(as.character(imputed_compl_pass2)), na.rm = FALSE),
            Workforce = mean(workforce),
            Sales = mean(sales),
            Profitability = mean(profitability_ratio),
            Export = mean(export),
            Profit = mean(profit))

# scatterplot: avg_numofvio vs profitability ratio
ggplot(avg,aes(x=avg_numofvio,y=Profitability))+
  geom_point(alpha = 0.2,color = 'steelblue1',shape=16)+
  geom_smooth(method='auto')+
  theme_minimal()
#lm_robust(avg_numofvio~Profitability,avg)

# scatterplot: avg_numofvio vs workforce
ggplot(avg,aes(x=avg_numofvio,y=Workforce))+
  geom_point(alpha = 0.2,color = 'steelblue1',shape=16)+
  geom_smooth(method='auto')+
  theme_minimal()
lm_robust(avg_numofvio~Workforce,avg)
# seems like there's a linear correlation between the two variables

# scatterplot: avg_numofvio vs export
ggplot(avg,aes(avg_numofvio,y=Export))+
  geom_point(alpha = 0.2,color = 'steelblue1',shape=16)+
  geom_smooth(method='auto')+
  theme_minimal()
#lm_robust(avg_numofvio~Export,avg)

# scatterplot: avg_numofvio vs sales
ggplot(avg,aes(avg_numofvio,y=Sales))+
  geom_point(alpha = 0.2,color = 'steelblue1',shape=16)+
  geom_smooth(method='auto')+
  theme_minimal()
#lm_robust(avg_numofvio~Sales,avg)

# boxplot: worst_imputed_compl_pass2 vs profit
ggplot(avg,aes(x=as.factor(worst_imputed_compl_pass2),y=Profit)) +
  geom_boxplot()+
  scale_y_continuous(limit=c(-10000,10000))+
  xlab("compl_pass(0=fail)")

#boxplot: best_imputed_compl_pass2 vs profit
ggplot(avg,aes(x=as.factor(best_imputed_compl_pass2),y=Profit)) +
  geom_boxplot()+
  scale_y_continuous(limit=c(-10000,10000))+
  xlab("compl_pass(0=fail)")

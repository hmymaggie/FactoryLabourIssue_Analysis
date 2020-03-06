library(readr)
library(tidyverse)
library(estimatr)
library(ggplot2)
library(haven)
library(psych)
library(anchors)

setwd("/Users/liuxi/Desktop/Project/Analysis")

# Read files
#load('/Users/liuxi/Desktop/Project/Analysis/input/completed_data_2009.RData')
load(url('https://www.dropbox.com/s/djqrrp2m6ilic98/completed_data_2009.RData?dl=1'))

#imputed_comp = read_csv(file = "/Users/liuxi/Desktop/Project/Analysis/input/imputed_compliance_data_2009.csv")
imputed_comp=read.csv('https://www.dropbox.com/s/mf23quaaseirjts/imputed_compliance_data_2009.csv?dl=1')

# Add the imputed column to complete_data
complete_data = mutate(complete_data,
                       imputed_compl_pass = imputed_comp$imputed_compl_pass)

# missed_compl_pass = filter(complete_data, is.na(imputed_compl_pass))

# Add total number of violation to the dataframe
complete_data = mutate(complete_data,
                       NumOfVio = rowSums(dplyr::select(complete_data,childlab:mgmt),na.rm=FALSE))


# Select matched factories (n=742 / unique n=527)
beforeavg_target = filter(complete_data,complete_data$matched_or_not == "matched")
# missingdata = filter(beforeavg_target, is.na(imputed_compl_pass)) # 34 data is still missing


# averaging variables by 'nameformerge'
# by default, NAs are not removed
afteravg_target <- beforeavg_target %>% group_by(nameformerge) %>%
  summarise(avg_numofvio = mean(NumOfVio),
            profit = mean(profit),
            imp_bin_pass = mean(imputed_compl_pass),
            bin_pass = mean(compl_pass),
            worst_imputed_compl_pass = min(imputed_compl_pass),
            best_imputed_compl_pass = max(imputed_compl_pass))

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

################### Split dataset: whether correlations can be found when avg_numofvio > or <= 3
low = filter(afteravg_target,avg_numofvio <= 3)
high = filter(afteravg_target,avg_numofvio > 3)

# Graph: when avg_numofvio <=3
ggplot(low,aes(x=avg_numofvio,y=profit))+
  geom_point()+
  geom_smooth(method='lm')+
  scale_y_continuous(limit=c(-10000,10000))

lm_robust(profit~avg_numofvio,low)
# (-484.1044,2740.677)

# Graph: when avg_numofvio > 3
ggplot(high,aes(x=avg_numofvio,y=profit))+
  geom_point()+
  geom_smooth(method='lm')+
  scale_y_continuous(limit=c(-10000,10000))

lm_robust(profit~avg_numofvio,high)
# (-1856.9503,437.8816)

library(readr)
library(tidyverse)
library(estimatr)
library(ggplot2)
library(haven)
library(psych)
library(anchors)

#setwd("/Users/liuxi/Desktop/Project/Analysis/input")
#Sys.setlocale(locale="UTF-8")
################## Merge Matched data and economic data

# Read files from dropbox
deliverable = read_csv('https://www.dropbox.com/s/h0jpfnasef72fcv/Matched_Final%20Deliverable.csv?dl=1') %>% 
  dplyr::rename(name = matched_names) %>% 
  mutate(deliv = 1)

load(url('https://www.dropbox.com/s/3iqomvfndarxexs/china_full.RData?dl=1'))
china = d
rm(d)

china = china %>% 
  mutate(china = 1)

merged = full_join(china, deliverable, by = 'name')

# Add one more column
merged = mutate(merged,
                matched_or_not = ifelse(is.na(merged$matched_countycode),"unmatched","matched"))

# write.csv(merged,"merged_econ_data.csv",fileEncoding = 'UTF-8')

# Select matched factories and save it as a csv file
df = filter(merged,matched_or_not=="matched")

#write.csv(df,"economic_data_2009_527_what.csv",fileEncoding = 'UTF-8')


################## Merge Economic and Compliance Data

# Read the files
econ = read_csv('https://www.dropbox.com/s/5fspf4gx79e806j/economic_data_2009_527.csv?dl=1')#%>% 
  #dplyr::rename(fname = ftyname1)

comp = read_dta('https://www.dropbox.com/s/jri4jknenf2bhcu/compliance_data_2009.dta?dl=1')

# Drop useless columns (7 columns from econ dataset)
drop_econ <- c('X1','ftyaddrlow','ftynamelow1','ftynamelow2','name1','Notes','deliv')
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

# missed_compl_pass = filter(complete_data, is.na(compl_pass))
# two companies in economic data set are not found in compliance data set? becasue of typo...
# missed = anti_join(econ,comp,by='nameformerge')


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

################## Codebook
# convert column names to rows
codebook = data.frame(colnames(complete_data))
#write.csv(codebook,'codebook.csv')

################## Create Summary Table (for econ data)
# Select columns to be examined in the summary of descriptive statistics
econ_des_df = dplyr::select(econ, est_year, acc_receivable:workforce)

# check if there are missing values in the dataframe: No missing values...
finding_NAs <- econ_des_df[rowSums(is.na(econ_des_df)) > 0,]
subset(econ_des_df,is.na(econ_des_df))
econ_des_df[!complete.cases(econ_des_df),]
econ_des_df %>% filter_all(any_vars(is.na(.)))

# create the descriptive table for the selected variables
d = psych::describe(econ_des_df)

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

################## Explore Compliance Data, started with completed dataset(n=4006)

# Find the duplicates of observations: # How many times a factory has been audited in 2009?
duplicates = data.frame(table(complete_data$nameformerge))
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
complete_data = mutate(complete_data,
                       audit_num = NA)

for (j in seq_along(duplicated(complete_data$nameformerge))){
  if (duplicated(complete_data$nameformerge)[j]=="FALSE") {
    complete_data$audit_num[j] = 1
  } else if (isTRUE(duplicated(complete_data$nameformerge)[j])) {
    a = c(1:7);
    for (i in seq_along(a)){
      if(isFALSE(duplicated(complete_data$nameformerge)[j-a[i]])){
        complete_data$audit_num[j] = 1+i
        break
      }
    }
  }
}

# For those that were visited more than once: why?
# Select factories that were audited more than once
dup_only = complete_data[duplicated(complete_data$nameformerge) | duplicated(complete_data$nameformerge, fromLast = TRUE), ]
# Then select their first audit
dup_only_first = filter(dup_only,dup_only$audit_num == 1)
# Visualize the the first audit grade for each factory
ggplot(dup_only_first,aes(x=compl_grade)) +
  geom_histogram(stat = 'count') +
  geom_text(stat='count',aes(label=..count..),vjust=-0.4)
# Most of the factories achieved C: 525

# what about factories that were audited only once?
not_dup = anti_join(complete_data,dup_only,by='nameformerge')
ggplot(not_dup,aes(x=compl_grade)) +
  geom_histogram(stat = 'count') +
  geom_text(stat='count',aes(label=..count..),vjust=-0.4)
# Most of the factories achieved C: 1268

#length(unique(complete_data$nameformerge)) #2905 unique factories

################## Explore Compliance Data: matched dataset(n = 527)
# Select matched data (n=742 / unique n=527)
cd_matched = filter(complete_data,matched_or_not=="matched")

# visualization: Exported destination
ggplot(filter(cd_matched,audit_num==1),aes(x=export_destination))+
  geom_histogram(stat='count')+
  geom_text(stat='count',aes(label=..count..),vjust=-0.2)+
  theme(axis.text.x=element_text(angle=90,hjust=1),panel.background = element_rect(fill="white"))

# what is the number of violation per audit? (NA is NOT removed)
cd_matched = mutate(cd_matched,
                    NumOfVio = rowSums(dplyr::select(cd_matched,childlab:mgmt),na.rm=FALSE))
# Visualization: counts of factories based on the total # of violations for grade C
# (31 factories were removed because of NAs)
ggplot(filter(cd_matched,cd_matched$compl_grade=="C"),aes(x=NumOfVio))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous("NumOfVio",labels=as.character(cd_matched$NumOfVio),breaks = cd_matched$NumOfVio)

# Create tables for each compl_grade
c = filter(cd_matched,cd_matched$compl_grade=="C")
b = filter(cd_matched,cd_matched$compl_grade=="B")
a = filter(cd_matched,cd_matched$compl_grade=="A")
d = filter(cd_matched,cd_matched$compl_grade=="D")

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

# What categories that have been violated the most (n = 742 / removed NAs)?
sum(cd_matched$childlab,na.rm = TRUE) #5
sum(cd_matched$ilab,na.rm = TRUE) #87
sum(cd_matched$forcedlab,na.rm = TRUE) #6
sum(cd_matched$discipline,na.rm = TRUE) #46
sum(cd_matched$discrim,na.rm = TRUE) #33
sum(cd_matched$health,na.rm = TRUE) #578
sum(cd_matched$env,na.rm = TRUE) #250
sum(cd_matched$wages,na.rm = TRUE) #418
sum(cd_matched$hours,na.rm = TRUE) #369
sum(cd_matched$assoc,na.rm = TRUE) #30
sum(cd_matched$legal,na.rm = TRUE) #190
sum(cd_matched$subcon,na.rm = TRUE) #52
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

library(readr)
library(pinyin)
library(magrittr)
library(tidyverse)
library(tibble)
library(stringr)

setwd('/Users/liuxi/Desktop/Project/Shandong')

# Read Files
# chn=read_csv('sd_fty_chn_in.csv')
eng = read_csv('sd_fty_eng_in.csv')
names = read_csv('shandong_pinyin_names.csv')

## engdata = English dataset
## pyadmindiv = pinyin administrative divisions

address_keywords_extraction <- function(engdata,pyadmindiv){
  engdata = mutate(engdata, ftyaddrlow=tolower(engdata$ftyaddr))
  engdata = mutate(engdata, number=str_extract_all(engdata$ftyaddr,"[0-9]+"))
  engdata = mutate(engdata, streetname=str_extract_all(engdata$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\s(road|avenue|street)"))
  engdata = mutate_at(engdata,vars(streetname),funs(replace(engdata$streetname,engdata$streetname=="character(0)",NA)))
  engdata = mutate_at(engdata,vars(number),funs(replace(engdata$number,engdata$number=="character(0)",NA)))
  engdata = mutate(engdata,
                   prefecture=NA,
                   county=NA,
                   province=NA)
  for (i in unique(pyadmindiv$Prefacture)) {for(j in 1:length(engdata$ftyaddrlow)) {if (isTRUE(grepl(i, engdata$ftyaddrlow[j])) ){engdata$prefecture[j]=i} }}
  for (i in unique(pyadmindiv$County)) {for(j in 1:length(engdata$ftyaddrlow)) {if (isTRUE(grepl(i, engdata$ftyaddrlow[j])) ){engdata$county[j]=i}}}
  for (i in unique(pyadmindiv$Province)) {for(j in 1:length(engdata$ftyaddrlow)) {if (isTRUE(grepl(i, engdata$ftyaddrlow[j])) ){engdata$province[j]=i}}}
  View(engdata)
}

address_keywords_extraction(eng,names)


# Convert the street names into lower case
#eng = mutate(eng, ftyaddrlow=tolower(eng$ftyaddr))

#extract street numbers
#eng = mutate(eng, number=str_extract_all(eng$ftyaddr,"[0-9]+"))

#extract street names
#eng = mutate(eng, streetname=str_extract_all(eng$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\s(road|avenue|street)"))

#replace character(0) to NA
#eng = mutate_at(eng,vars(streetname),funs(replace(eng$streetname,eng$streetname=="character(0)",NA)))
#eng = mutate_at(eng,vars(number),funs(replace(eng$number,eng$number=="character(0)",NA)))

# Create three lists with NA
#eng=mutate(eng,
#           prefecture=NA,
#           county=NA,
#           province=NA)

# Extract Prefecture
#for (i in unique(names$Prefacture)) {for(j in 1:length(eng$ftyaddrlow)) {if (isTRUE(grepl(i, eng$ftyaddrlow[j])) ){eng$prefecture[j]=i} }}

# Extract County
#for (i in unique(names$County)) {for(j in 1:length(eng$ftyaddrlow)) {if (isTRUE(grepl(i, eng$ftyaddrlow[j])) ){eng$county[j]=i}}}

# Extract Province
#for (i in unique(names$Province)) {for(j in 1:length(eng$ftyaddrlow)) {if (isTRUE(grepl(i, eng$ftyaddrlow[j])) ){eng$province[j]=i}}}

# Advantages: (1) avoid possible name issue, do not need to use index
# Disadvantage: (1) Some names that are not county or prefecture are extracted, 
#see 33 (but it is possible that it is actually the county), zaozhuang not extracted due to a whitespace
# 22: caixian county vs. cai 
# 23: feicheng vs. fei
# 13: jiaonan city is not extracted, not in the original dataset

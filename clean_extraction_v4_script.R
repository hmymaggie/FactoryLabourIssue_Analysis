# This script is used to clean the 4th version of data extraction after mannual adjustment
# Because re-adjust errors manually may cost too much time

library(readr)
library(tidyverse)
library(tibble)
library(stringr)
library(naniar)
library(pinyin)

setwd("/Users/liuxi/Desktop/Project/2009/Matching_basedon_v5")
Sys.setlocale(category="LC_ALL", locale="Chinese")

# Read files
eng = read_csv('name&addrextractionformatch_v4.csv')
pyadmin = read_csv("full_pyadmindiv.csv")

# Remove white spaces in the following columns: get ready for the match process!
for (i in seq_along(eng$s_fcode)){
  eng$name2[i] = gsub(" ","",eng$name2[i],fixed=TRUE);
  eng$streetnames[i] = gsub(" ","",eng$streetnames[i],fixed=TRUE);
  eng$prefecture[i] = gsub(" ","",eng$prefecture[i],fixed=TRUE);
  eng$county[i] = gsub(" ","",eng$county[i],fixed=TRUE);
  eng$indzone[i] = gsub(" ","",eng$indzone[i],fixed=TRUE);
  eng$basiclvl[i] = gsub(" ","",eng$basiclvl[i],fixed=TRUE);
  eng$province[i] = gsub(" ","",eng$province[i],fixed=TRUE)
}

a = filter(eng,is.na(eng$province))
b = filter(eng,is.na(eng$prefecture))

# fill the province based on prefecture


for (i in seq_along(eng$ftyname1)) {
  if (is.na(eng$province[i]) & !is.na(eng$prefecture[i])) {
    for (j in seq_along(pyadmin$prefecture_c)) {
      if (grepl(pyadmin$prefecture_c[j],eng$prefecture[i])){
        eng$province[i] = pyadmin$province_c[j]
      }
    }
  }
}


write_csv(eng,"name&addrextractionformatch_v5.csv")

library(readr)
library(tidyverse)
library(tibble)

library(stringr)
library(naniar)
Sys.setlocale(category="LC_ALL", locale="Chinese")

setwd("/Users/hmyma/Desktop")
all_factories_chinese <- read_csv("removed_all_factories_chinese.csv")
extract_pro <- function(a) {
  filter(all_factories_chinese,all_factories_chinese$var5== a)
  
}


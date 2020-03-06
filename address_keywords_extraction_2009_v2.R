library(readr)
library(tidyverse)
library(tibble)
library(stringr)
library(naniar)
library(magrittr)

setwd("/Users/hmyma/Desktop/Project/")

# Read files
all_factories_eng = read_csv('all_factories_2009.csv')

full_pyadmindiv = read_csv('full_pyadmindiv.csv')

# Convert upper case to lower case: company name & address
all_factories_eng = mutate(all_factories_eng, 
                           ftyaddrlow = tolower(all_factories_eng$ftyaddr),
                           ftynamelow = tolower(all_factories_eng$ftyname1))
# Create three empty columns for province, prefecture and county
all_factories_eng = mutate(all_factories_eng,
                           province = NA,
                           prefecture = NA,
                           county = NA)

# Extract province from company address or company name
for (i in unique(full_pyadmindiv$province_c)) { 
  for (j in 1:length(all_factories_eng$ftyaddrlow)) {
    if (isTRUE(grepl(i, all_factories_eng$ftyaddrlow[j]))) {
      all_factories_eng$province[j] = i
    } 
    else if (isTRUE(grepl(i, all_factories_eng$ftynamelow[j]))) {
      all_factories_eng$province[j] = i
    }
  }
}

# Test: the number of observations from which we fail to extract province.
# a = filter(all_factories_eng, is.na(province))


# Extract prefecture from address based on the province
for (add in 1:length(all_factories_eng$ftyaddrlow)){
  # Use filter function to create a new pinyin list of admin division for a specific province
  specific_prov_c=filter(full_pyadmindiv,province_c==all_factories_eng$province[add]);
  for (j in specific_prov_c$prefecture_c){
    if (isTRUE(grepl(j,all_factories_eng$ftyaddrlow[add]))){
      all_factories_eng$prefecture[add]=j
    }
  }
}

# Repeat previous for loop for province separated by whitespace
for (add in 1:length(all_factories_eng$ftyaddrlow)){
  specific_prov_s=filter(full_pyadmindiv,province_s==all_factories_eng$province[add]);
  for (j in specific_prov_s$prefecture_s){
    if (isTRUE(grepl(j,all_factories_eng$ftyaddrlow[add]))){
      all_factories_eng$prefecture[add]=j
    }
  }
}

# Since the prefecture will be extracted only in the presence of province
# if the prefecture is missing, use company name to extract prefecture
if (is.na(all_factories_eng$prefecture)) {
  for (i in unique(full_pyadmindiv$prefecture_c)) { 
    for (j in 1:length(all_factories_eng$ftynamelow)) {
      if (isTRUE(grepl(paste("\\b",i,"\\b",sep=""), all_factories_eng$ftynamelow[j]))) {
        all_factories_eng$prefecture[j] = i
      }
    }
  }
}

######## TEST:
# we encountered a problem: "changzhou" was extracted as "hangzhou"
# solution: add \\b
#c = "changzhou city"
#h = "hangzhou city"
#p = 'hangzhou'
#str_view(c,paste("\\b",p,"\\b", sep=""))

# Extract county based on prefecture: avoid overmatching within province
for (add in 1:length(all_factories_eng$ftyaddrlow)){
  specific_pref_c=filter(full_pyadmindiv,prefecture_c==all_factories_eng$prefecture[add]);
  for (j in specific_pref_c$county_c){
    if (isTRUE(grepl(paste("\\b",j,"\\b",sep=""),all_factories_eng$ftyaddrlow[add]))){
      all_factories_eng$county[add]=j
    }
  }
}

# The same as the previous for loop, but extract prefecture with whitespace
for (add in 1:length(all_factories_eng$ftyaddrlow)){
  specific_pref_s=filter(full_pyadmindiv,prefecture_c==all_factories_eng$prefecture[add]);
  for (j in specific_pref_s$county_s){
    if (isTRUE(grepl(paste("\\b",j,"\\b",sep=""),all_factories_eng$ftyaddrlow[add]))){
      all_factories_eng$county[add]=j
    }
  }
}

#Extract county based on prefecture by namelow: avoid overmatching within province
for (add in 1:length(all_factories_eng$ftyaddrlow)){
  specific_pref_c=filter(full_pyadmindiv,prefecture_c==all_factories_eng$prefecture[add]);
  for (j in specific_pref_c$county_c){
    if (isTRUE(grepl(paste("\\b",j,"\\b",sep=""),all_factories_eng$ftynamelow[add]))){
      all_factories_eng$county[add]=j
    }
  }
}

# The same as the previous for loop, but extract prefecture with whitespace
for (add in 1:length(all_factories_eng$ftyaddrlow)){
  specific_pref_s=filter(full_pyadmindiv,prefecture_c==all_factories_eng$prefecture[add]);
  for (j in specific_pref_s$county_s){
    if (isTRUE(grepl(paste("\\b",j,"\\b",sep=""),all_factories_eng$ftynamelow[add]))){
      all_factories_eng$county[add]=j
    }
  }
}


# Not a good solution to extract county from name: high rate of overmatching
#if (is.na(all_factories_eng$county)) {
#  for (i in unique(full_pyadmindiv$county_c)) { 
#    for (j in 1:length(all_factories_eng$ftynamelow)) {
#      if (isTRUE(grepl(paste("\\b",i,"\\b",sep=""), all_factories_eng$ftynamelow[j]))) {
#        all_factories_eng$county[j] = i
#      }
#    }
#  }
#}

# Create 5 temporary columns: prepare to convert 'list-columns' to 'characters' (str_extract_all returns vectorized objects)
all_factories_eng = mutate(all_factories_eng, 
                           temp_streetnumber = str_extract_all(all_factories_eng$ftyaddrlow,"[0-9]+"),
                           # we need to create 2 temporary columns because positive lookbehind does not work with repetitive quantifier
                           temp1_streetname = str_extract_all(all_factories_eng$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\s(?=(road|avenue|street|\\brd\\b|\\bst\\b|\\bave\\b|\\bav\\b))"),
                           temp_indzone = str_extract_all(all_factories_eng$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\s(?=industrial(\\s(park|zone|district|area|trading\\szone|borough))?)"),
                           temp_township = str_extract_all(all_factories_eng$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\s(?=town)"),
                           temp_basiclvl = str_extract_all(all_factories_eng$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\s(?=village)"))

#h = "daxihuan road"
#str_view(h,"(?<=\\bof\\b\\s)[a-z]+(?=\\sroad)")

all_factories_eng = mutate(all_factories_eng,
                           streetnumbers = NA,
                           temp2_streetname = NA,
                           indzone = NA,
                           township = NA,
                           basiclvl = NA)

# Convert list-columns to characters and store them in the empty columns
for (i in 15:19) {
  for(j in 1:length(all_factories_eng$s_fcode)) {
    all_factories_eng[[i+5]][j]=paste(all_factories_eng[[i]][j],collapse = ',')
  }
}

# Replace "character(0)" with "NA"
all_factories_eng[all_factories_eng=='character(0)']<-NA_character_


# Return a table without the temporary columns created earlier
all_factories_eng = all_factories_eng[,-c(15:19)]

# Delete "south areas of" from the temp2_streetname
all_factories_eng = mutate(all_factories_eng,
                           streetnames = NA)
for (i in 1:length(all_factories_eng$temp2_streetname)) {
  if (isTRUE(grepl("(\\bof\\b)",all_factories_eng$temp2_streetname[i]))) {
    all_factories_eng$streetnames[i] = str_remove_all(all_factories_eng$temp2_streetname[i],"([a-z]+\\s)*\\bof\\b")
  } else {
    all_factories_eng$streetnames[i] = all_factories_eng$temp2_streetname[i]
  }
}


write_csv(all_factories_eng,"address_keywords_extraction_v1.csv")

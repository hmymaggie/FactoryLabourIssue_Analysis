library(readr)
library(tidyverse)
library(tibble)
library(stringr)
library(naniar)

setwd("/Users/liuxi/Desktop/Project/2009")

# Read files
all_factories_eng = read_csv('all_factories_2009.csv')

full_pyadmindiv = read_csv('full_pyadmindiv.csv')

# Convert upper case to lower case: company name & address
all_factories_eng = mutate(all_factories_eng, 
                           ftyaddrlow = tolower(all_factories_eng$ftyaddr),
                           ftynamelow1 = tolower(all_factories_eng$ftyname1),
                           ftynamelow2 = tolower(all_factories_eng$ftyname2))
# Create three empty columns for province, prefecture and county
all_factories_eng = mutate(all_factories_eng,
                           province = NA,
                           prefecture = NA,
                           county = NA)

###################### 1. PROVINCE EXTRACTION ######################
###Extract province (no white space) from company address or company name 1 & 2
## Solved 2 & 8(b) by adding \\b
## Solved henan road/chongqing road by adding one more if condition

for (i in unique(full_pyadmindiv$province_s)) { 
  for (j in 1:length(all_factories_eng$ftyaddrlow)) {
    if (isTRUE(grepl(paste("\\b",i,"\\b",sep=""), all_factories_eng$ftynamelow1[j]))) {
      all_factories_eng$province[j] = i
    } 
    else if (isTRUE(grepl(paste("\\b",i,"\\b",sep=""), all_factories_eng$ftynamelow2[j]))) {
      all_factories_eng$province[j] = i
    }
    else if (isTRUE(grepl(paste("\\b",i,"\\b",sep=""), all_factories_eng$ftyaddrlow[j]))) {
      if (isTRUE(str_detect(all_factories_eng$ftyaddrlow[j],paste("\\b",i,"\\b","(?!\\s(road|avenue|street|\\brd\\b|\\bst\\b|\\bave\\b|\\bav\\b))",sep="")))) {
        all_factories_eng$province[j] = i
      } else {
        all_factories_eng$province[j] = NA_character_
      }
    }
  }
}

for (i in unique(full_pyadmindiv$province_c)) { 
  for (j in 1:length(all_factories_eng$ftyaddrlow)) {
    if (isTRUE(grepl(paste("\\b",i,"\\b",sep=""), all_factories_eng$ftynamelow1[j]))) {
      all_factories_eng$province[j] = i
    } 
    else if (isTRUE(grepl(paste("\\b",i,"\\b",sep=""), all_factories_eng$ftynamelow2[j]))) {
      all_factories_eng$province[j] = i
    }
    else if (isTRUE(grepl(paste("\\b",i,"\\b",sep=""), all_factories_eng$ftyaddrlow[j]))) {
      if (isTRUE(str_detect(all_factories_eng$ftyaddrlow[j],paste(i,"(?!\\s(road|avenue|street|\\brd\\b|\\bst\\b|\\bave\\b|\\bav\\b))",sep="")))) {
        all_factories_eng$province[j] = i
      } else {
        all_factories_eng$province[j] = NA_character_
      }
    }
  }
}






# Test: the number of observations from which we fail to extract province.
# a = filter(all_factories_eng, is.na(province)) #1920

###################### 2. PREFECTURE EXTRACTION ######################
# Extract prefecture from address based on the province (no whitespace)
for (add in 1:length(all_factories_eng$ftyaddrlow)){
  # Use filter function to create a new pinyin list of admin division for a specific province
  specific_prov_c=filter(full_pyadmindiv,province_c==all_factories_eng$province[add]);
  for (j in specific_prov_c$prefecture_c){
    if (isTRUE(grepl(j,all_factories_eng$ftyaddrlow[add]))){
      all_factories_eng$prefecture[add]=j
    }
  }
}


# Repeat previous for loop to extract prefecture from address based on province (with whitespace)
for (add in 1:length(all_factories_eng$ftyaddrlow)){
  specific_prov_s=filter(full_pyadmindiv,province_s==all_factories_eng$province[add]);
  for (j in specific_prov_s$prefecture_s){
    if (isTRUE(grepl(j,all_factories_eng$ftyaddrlow[add]))){
      all_factories_eng$prefecture[add]=j
    }
  }
}


# Since the prefecture will be extracted only in the presence of province
# if both province and prefecture are missing, use company name to extract prefecture 
# (prefecture without whitespace)
if (is.na(all_factories_eng$prefecture)) {
  for (i in unique(full_pyadmindiv$prefecture_c)) { 
    for (j in 1:length(all_factories_eng$ftynamelow1)) {
      if (isTRUE(grepl(paste("\\b",i,"\\b",sep=""), all_factories_eng$ftynamelow1[j]))) {
        all_factories_eng$prefecture[j] = i
      }
      else if (isTRUE(grepl(paste("\\b",i,"\\b",sep=""), all_factories_eng$ftynamelow2[j]))) {
        all_factories_eng$prefecture[j] = i
      }
    }
  }
}
# The same as the previous for loop but extract prefecture with whitespace
if (is.na(all_factories_eng$prefecture)) {
  for (i in unique(full_pyadmindiv$prefecture_s)) { 
    for (j in 1:length(all_factories_eng$ftynamelow1)) {
      if (isTRUE(grepl(paste("\\b",i,"\\b",sep=""), all_factories_eng$ftynamelow1[j]))) {
        all_factories_eng$prefecture[j] = i
      }
      else if (isTRUE(grepl(paste("\\b",i,"\\b",sep=""), all_factories_eng$ftynamelow2[j]))) {
        all_factories_eng$prefecture[j] = i
      }
    }
  }
}

# b = filter(all_factories_eng, is.na(prefecture)) #1256 #1198

###################### 3. COUNTY EXTRACTION ######################
# TEST:
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

#Extract county based on prefecture by namelow: avoid overmatching within prefecture
for (add in 1:length(all_factories_eng$ftyaddrlow)){
  specific_pref_c=filter(full_pyadmindiv,prefecture_c==all_factories_eng$prefecture[add]);
  for (j in specific_pref_c$county_c){
    if (isTRUE(grepl(paste("\\b",j,"\\b",sep=""),all_factories_eng$ftynamelow1[add]))){
      all_factories_eng$county[add]=j
    }
  }
}

# The same as the previous for loop, but extract prefecture with whitespace
for (add in 1:length(all_factories_eng$ftyaddrlow)){
  specific_pref_s=filter(full_pyadmindiv,prefecture_c==all_factories_eng$prefecture[add]);
  for (j in specific_pref_s$county_s){
    if (isTRUE(grepl(paste("\\b",j,"\\b",sep=""),all_factories_eng$ftynamelow1[add]))){
      all_factories_eng$county[add]=j
    }
  }
}
#c = filter(all_factories_eng, is.na(county)) #1890 #1546

###############################
selected = filter(all_factories_eng,is.na(prefecture))

# If province is not missing, extract county based on province from address
for (e in seq_along(selected$ftyaddrlow)) {
  if (!is.na(selected$province[e])) {
    filter_prov_c = filter(full_pyadmindiv, province_c == selected$province[e]);
    for (c in filter_prov_c$county_c) {
      if (isTRUE(grepl(paste("\\b",c,"\\b",sep = ""),selected$ftyaddrlow[e]))) {
        selected$county[e] = c
      }
    }
  }
}

for (e in seq_along(selected$ftyaddrlow)) {
  if (!is.na(selected$province[e])) {
    filter_prov_s = filter(full_pyadmindiv, province_s == selected$province[e]);
    for (s in filter_prov_s$county_s) {
      if (isTRUE(grepl(paste("\\b",s,"\\b",sep = ""),selected$ftyaddrlow[e]))) {
        selected$county[e] = s
      }
    }
  }
}
# If province is missing, extract prefecture from company address
# To avoid overmatching, we only match prefecture with length that equals to or more than 5 characters 
if (is.na(selected$province)) {
  for (e in seq_along(is.na(selected$province))) {
    for (q in unique(full_pyadmindiv$prefecture_c)) {
      if (nchar(q) > 4) {
        if (isTRUE(str_detect(selected$ftyaddrlow[e],paste("\\b",q,"\\b","(?!\\s(road|\\brd\\b|\\bst\\b|industrial|town|county|street))",sep = "")))) {
          selected$prefecture[e] = q
        }
      }
    }
  }
}
if (is.na(selected$province)) {
  for (e in seq_along(is.na(selected$province))) {
    for (q in unique(full_pyadmindiv$prefecture_s)) {
      if (nchar(q) > 4) {
        if (isTRUE(str_detect(selected$ftyaddrlow[e],paste("\\b",q,"\\b","(?!\\s(road|\\brd\\b|\\bst\\b|industrial|town|county|street))",sep = "")))) {
          selected$prefecture[e] = q
        }
      }
    }
  }
}
# Extract county based on prefecture 
for (e in seq_along(selected$ftyaddrlow)) {
  if (!is.na(selected$prefecture[e])) {
    filter_pref_c = filter(full_pyadmindiv, prefecture_c == selected$prefecture[e]);
    for (c in filter_pref_c$county_c) {
      if (isTRUE(grepl(paste("\\b",c,"\\b",sep = ""),selected$ftyaddrlow[e]))) {
        selected$county[e] = c
      }
    }
  }
}
for (e in seq_along(selected$ftyaddrlow)) {
  if (!is.na(selected$prefecture[e])) {
    filter_pref_s = filter(full_pyadmindiv, prefecture_s == selected$prefecture[e]);
    for (s in filter_pref_s$county_s) {
      if (isTRUE(grepl(paste("\\b",s,"\\b",sep = ""),selected$ftyaddrlow[e]))) {
        selected$county[e] = s
      }
    }
  }
}

# To avoid overmatching, use the following logic:
# If county is successfully extracted, there's a lower chance of overmatching
# If county is not extracted, there are 2 possibilities: 
# (1) county is not in the address but prefecture is CORRECTLY extracted
# (2) county is or is not in the address but prefecture is NOT correctly extracted
# Therefore, it's safer to delete the rows where prefecture is NOT NA but county is NA

for (i in seq_along(selected$prefecture)) {
  if (is.na(selected$county[i])) {
    if (!is.na(selected$prefecture[i])) {
      selected$prefecture[i] = NA_character_
    }
  }
}

#s = filter(selected,is.na(selected$county))

# If county is empty, create a loop for guangdong only since guangdong has many factories

for (i in seq_along(is.na(selected$county))) {
  guangdong = filter(full_pyadmindiv,full_pyadmindiv$province_c == "guangdong");
  for (j in guangdong$county_c) {
    if(grepl(paste("\\b",j,"\\b",sep=""),selected$ftyaddrlow[i])) {
      selected$county[i] = j
    }
  }
}

# Combine two dataframes:

for (i in seq_along(all_factories_eng$s_fcode)) {
  for (j in seq_along(selected$s_fcode)) {
    if (all_factories_eng$s_fcode[i] == selected$s_fcode[j]) {
      all_factories_eng$prefecture[i] = selected$prefecture[j]
      all_factories_eng$county[i] = selected$county[j]
    }
  }
}



#write_csv(selected,"selected.csv")

###################### 4. STREET #, NAME, INDZONE,TOWNSHIP & BASICLVL EXTRACTION ######################
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
for (i in 16:20) {
  for(j in 1:length(all_factories_eng$s_fcode)) {
    all_factories_eng[[i+5]][j]=paste(all_factories_eng[[i]][j],collapse = ',')
  }
}

# Replace "character(0)" with "NA"
all_factories_eng[all_factories_eng=='character(0)']<-NA_character_


# Return a table without the temporary columns created earlier
all_factories_eng = all_factories_eng[,-c(16:20)]

# Delete "south areas of" from the temp2_streetname
all_factories_eng = mutate(all_factories_eng,
                           streetnames = NA)
for (i in 1:length(all_factories_eng$temp2_streetname)) {
  if (isTRUE(grepl("(\\bof\\b)",all_factories_eng$temp2_streetname[i]))) {
    all_factories_eng$streetnames[i] = str_remove_all(all_factories_eng$temp2_streetname[i],"([a-z]+\\s)*\\bof\\b\\s")
  } else {
    all_factories_eng$streetnames[i] = all_factories_eng$temp2_streetname[i]
  }
}


#write_csv(all_factories_eng,"address_keywords_extraction_v1.csv")

###################### 5. COMPANY NAME EXTRACTION ######################
all_factories_eng = mutate(all_factories_eng,
                           name1 = NA,
                           name2 = NA)
# Delete some distracting words, such as "co", "ltd" and etc.
for (i in 1:length(all_factories_eng$ftynamelow1)) {
  all_factories_eng$name1[i] = str_remove_all(all_factories_eng$ftynamelow1[i],
                                              "\\bco\\b|\\bltd\\b|\\blimited\\b|,|\\.|;|\\bfactory\\b|\\bindustrial\\b|\\bindustry\\b|\\bmanufactory\\b
                                              |\\bcompany\\b|\\bcorp\\b|\\bfty\\b|workshop|\\binc\\b|\\bmanufacturing\\b|\\bind\\b|\\bmfg\\b
                                              |\\benterprise\\b|\\benterprises\\b|\\bproducts\\b|\\bproduct\\b|\\binternational\\b|\\bproduction\\b|\\bmanufacture\\b
                                              |\\bmanufacturers\\b|\\bmfy\\b|\\bmft\\b|\\bmfg\\b|\\baccessories\\b|\\baccessory\\b
                                              |\\bcity\\b|\\bchina\\b|'|-|\\(.*\\)|\\bprovince\\b|&|\\bcorporation\\b|\\borganization\\b|\\bintd\\b")
}

# Delete prefectures and province and county
# Problem: for the rows that we did not successfully extract county names, the county names were not deleted based on this for loop
for (j in seq_along(all_factories_eng$name1)) {
  if ((isTRUE(grepl(paste("\\b",all_factories_eng$province[j],"\\b",sep = ""),all_factories_eng$name1[j]))) & (isTRUE(grepl(paste("\\b",all_factories_eng$prefecture[j],"\\b",sep = ""),all_factories_eng$name1[j])))){
    all_factories_eng$name2[j] = str_remove_all(all_factories_eng$name1[j],paste("\\b",all_factories_eng$province[j],"\\b",sep=""))
    all_factories_eng$name2[j] = str_remove_all(all_factories_eng$name1[j],paste("\\b",all_factories_eng$prefecture[j],"\\b",sep=""))
  }
  else if (isTRUE(grepl(paste("\\b",all_factories_eng$province[j],"\\b",sep = ""),all_factories_eng$name1[j]))){
    all_factories_eng$name2[j] = str_remove_all(all_factories_eng$name1[j],paste("\\b",all_factories_eng$province[j],"\\b",sep=""))
  } 
  else if (isTRUE(grepl(paste("\\b",all_factories_eng$prefecture[j],"\\b",sep = ""),all_factories_eng$name1[j]))){
    all_factories_eng$name2[j] = str_remove_all(all_factories_eng$name1[j],paste("\\b",all_factories_eng$prefecture[j],"\\b",sep=""))
  }
  else if (isTRUE(grepl(paste("\\b",all_factories_eng$county[j],"\\b",sep = ""),all_factories_eng$name1[j]))){
    all_factories_eng$name2[j] = str_remove_all(all_factories_eng$name1[j],paste("\\b",all_factories_eng$county[j],"\\b",sep=""))
  }
  else {
    all_factories_eng$name2[j]=all_factories_eng$name1[j]
  }
}

# Delete product names
dic = read_csv('dictionary.csv')
diclist=c(dic$dictionaries)
for (j in seq_along(all_factories_eng$name2)) {
  for (var in diclist) {
    all_factories_eng$name2[j] = gsub(paste("\\b",var,"\\b",sep=""),"",all_factories_eng$name2[j])
  }
}


# export the file
write_csv(all_factories_eng,'name&address_extraction_v3.csv')

#### TEST ####
#h = 'yuandong arts and craft'
#for (var in diclist) {
#  h = gsub(paste("\\b",var,"\\b",sep=""),"",h)
#}
#h

# extremely slow for loop that can be used to remove province, prefecture and county from company name
#for (j in seq_along(all_factories_eng$name1)) {
#  for (col in select(all_factories_eng,province:county)) {
#    for (char in col) {
#      if (isTRUE(grepl(paste("\\b",char,"\\b",sep = ""),all_factories_eng$name1[j]))) {
#        all_factories_eng$name2[j] = str_remove_all(all_factories_eng$name1[j],paste("\\b",char,"\\b",sep=""))
#      }
#    }
#  }
#}
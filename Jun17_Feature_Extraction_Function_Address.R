library(readr)
library(pinyin)
library(magrittr)
library(tidyverse)
library(tibble)
library(stringr)
library(naniar)

setwd('/Users/liuxi/Desktop/Project/Shanghai')

# Read Files
# chn=read_csv('sh_fty_chn_in.csv')
sh_engdata = read_csv('sh_fty_eng_in.csv')
sh_pyadmindiv = read_csv('shanghai_pinyin_names.csv')

## _engdata = English dataset
## _pyadmindiv = pinyin administrative divisions

######################################## Function: address_keywords_extraction ########################################
address_keywords_extraction <- function(engdata,pyadmindiv) {
  engdata = mutate(engdata, ftyaddrlow=tolower(engdata$ftyaddr))
  # Step 2: Create 5 temporary columns: prepare to convert 'list-columns' to 'characters' (str_extract_all returns vectorized objects)
  engdata = mutate(engdata, 
                   temp_streetnumber=str_extract_all(engdata$ftyaddrlow,"[0-9]+"),
                   temp_streetname=str_extract_all(engdata$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\s(road|avenue|street|rd|st|ave|av)"),
                   temp_indzone=str_extract_all(engdata$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\sindustrial(\\s(park|zone|district|area|trading\\szone|borough))?"),
                   temp_township=str_extract_all(engdata$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\stown"),
                   temp_basiclvl=str_extract_all(engdata$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\svillage"))
  
  # Step 3: Create 3 new columns to fit extracted prefecutre, county, and province from _pyadmindiv table
  engdata = mutate(engdata,
                   prefecture=NA,
                   county=NA,
                   province=NA)
  
  # Step 4: Extract prefecture, county & province using 3 for loops: if the keywords 
  for (i in unique(pyadmindiv$Prefacture)) { 
    for (j in 1:length(engdata$ftyaddrlow)) {
      if (isTRUE(grepl(i, engdata$ftyaddrlow[j]))) {
        engdata$prefecture[j]=i
      } 
    }
  }
  
  for (i in unique(pyadmindiv$County)) {
    for(j in 1:length(engdata$ftyaddrlow)) {
      if (isTRUE(grepl(i, engdata$ftyaddrlow[j])) ){
        engdata$county[j]=i
      }
    }
  }
  
  for (i in unique(pyadmindiv$Province)) {
    for(j in 1:length(engdata$ftyaddrlow)) {
      if (isTRUE(grepl(i, engdata$ftyaddrlow[j])) ){
        engdata$province[j]=i
      }
    }
  }
  # Step 5: create 5 new columns for keywords
  engdata = mutate(engdata,
                   streetnumber=NA,
                   streetname=NA,
                   indzone=NA,
                   township=NA,
                   basiclvl=NA)
  # Step 6: Convert list-columns to characters and store them in the empty columns created in step 5
  for (i in 11:15) {
    for(j in 1:length(engdata$s_fcode)) {
      engdata[[i+8]][j]=paste(engdata[[i]][j],collapse = ',')
    }
  }
  # Step 7: Replace "character(0)" with "NA"
  engdata[engdata=='character(0)']<-NA_character_
  
  # Return a table without the temporary columns created in step 2
  return(add_keywords_table=engdata[,-c(11:15)])
}

# Write the csv file to your current working directory
write_csv(address_keywords_extraction(sh_engdata,sh_pyadmindiv),'sh_add_table.csv')

############## The End ##############










# Convert the street names into lower case
#engdata = mutate(engdata, ftyaddrlow=tolower(engdata$ftyaddr))

#eng = mutate(eng,
#            transition=str_extract(eng$ftyaddrlow,"(((no|#)(.)?(\\s)?[0-9]+)(\\s|,)(\\s)*([a-z]+\\s)*[a-z]+\\s(road|rd|street|st|avenue|ave|av|lu|jie|dao|highway))|([0-9]+(\\s)([a-z]+\\s)+(road|rd|street|st|avenue|ave|av|lu|jie|dao|highway))"))


#a="3866 husong highway, shanghai 201619,china, ,"
#str_view(a,"[0-9]+\\s([a-z]+\\s)+highway")

#engdata = mutate(engdata, ftyaddrlow=tolower(engdata$ftyaddr))
#engdata = mutate(engdata, temp=str_extract_all(engdata$ftyaddrlow,"(no|no|#)(.)?(\\s)?[0-9]+"))





#engdata = mutate(engdata, temp_streetnumber=str_extract_all(engdata$ftyaddrlow,"[0-9]+"))
#engdata = mutate(engdata, temp_streetname=str_extract_all(engdata$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\s(road|avenue|street|rd|st|ave|av)"))
#engdata = mutate(engdata, temp_indzone=str_extract_all(engdata$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\sindustrial(\\s(park|zone|district|area|trading\\szone|borough))?"))
#engdata = mutate(engdata, temp_township=str_extract_all(engdata$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\stown"))
#engdata = mutate(engdata, temp_basiclvl=str_extract_all(engdata$ftyaddrlow,"([a-z]+\\s)*[a-z]+\\svillage"))

#engdata = mutate_all(engdata,vars(number),funs(paste(engdata$number,collapse = ',')))
#engdata = mutate_at(engdata,vars(streetname),funs(replace(engdata$streetname,engdata$streetname=="character(0)",NA)))
#engdata = mutate_at(engdata,vars(number),funs(replace(engdata$number,engdata$number=="character(0)",NA)))

#engdata = mutate(engdata,prefecture=NA,county=NA,province=NA)
#for (i in unique(pyadmindiv$Prefacture)) {for(j in 1:length(engdata$ftyaddrlow)) {if (isTRUE(grepl(i, engdata$ftyaddrlow[j])) ){engdata$prefecture[j]=i} }}
#for (i in unique(pyadmindiv$County)) {for(j in 1:length(engdata$ftyaddrlow)) {if (isTRUE(grepl(i, engdata$ftyaddrlow[j])) ){engdata$county[j]=i}}}
#for (i in unique(pyadmindiv$Province)) {for(j in 1:length(engdata$ftyaddrlow)) {if (isTRUE(grepl(i, engdata$ftyaddrlow[j])) ){engdata$province[j]=i}}}



#engdata = mutate_at(engdata,vars(number),funs())
#engdata = mutate(engdata,streetnumber=NA,streetname=NA,indzone=NA,township=NA,basiclvl=NA)


#for (i in 11:15) {for(j in 1:length(engdata$s_fcode)) {engdata[[i+8]][j]=paste(engdata[[i]][j],collapse = ',')}}
#for (i in (ncol(engdata)-5):ncol(engdata)){mutate_at(engdata,vars((colnames(engdata)[i])),funs(replace(engdata[i]),engdata[i]=="character(0)",NA))}
#engdata[engdata=='character(0)']<-NA_character_


#writable=engdata[,-c(11:15)]
#write_csv(writable,'write.csv')



#length(engdata$streetnumber[37])

#for (var in colnames(engdata)) {if(var=='temp_streetnumber'){for (i in 1:length(engdata$temp_streetnumber)) { engdata$streetnumber[i]=paste(engdata$temp_streetnumber[i],collapse = ',')}}}


#typeof(engdata$streetnumber)

# for (i in 1:length(engdata$tempnumber)) { engdata$tempnumber[i]=paste(engdata$number[i],collapse = ',')}

library(tibble)
library(stringr)
library(tidyverse)
library(pinyin)

Sys.setlocale(category="LC_ALL", locale="Chinese")

dictionary = tribble (~English, ~Chinese,
                      "clothing",c("衣服","制衣"),
                      "clothes",c("衣服","制衣"),
                      "accessory",c("饰品","装饰"),
                      "arts",c("工艺品")) # used to match product names
#dictionary

factories = tribble(~name,
                    "yuandong clothing factory",
                    "anhui yichen arts and crafts")
#factories

factories = mutate(factories, productname = NA, match = NA)


for (n in seq_along(factories$name)){
  for (p in seq_along(dictionary$English)) {
    if (str_detect(factories$name[n],dictionary$English[p])){
      factories$productname[n] = dictionary$Chinese[p]
    }
  }
}

#factories

chinesedata = tribble(~chinesename,
                      "远东制衣厂",
                      "安徽怡辰工艺品有限公司") 

chinesedata = mutate(chinesedata,
                     pinyinnames = NA) # used to match company names

# "dic = pinyin2", "dic = pinyin" gives wrong pinyin
for (chchar in seq_along(chinesedata$chinesename)) {
  chinesedata$pinyinnames[chchar] = py(chinesedata$chinesename[chchar],sep = "",dic = pydic(method = c("toneless"),multi = FALSE, dic = c("pinyin2")))
}

#chinesedata


for (chchar in seq_along(chinesedata$chinesename)){
  for (num in seq_along(factories$productname)){
    for (i in lapply(factories$productname[num], function(x) lapply(x, identity))){
      if (grepl(paste(i,collapse = "|"),chinesedata$chinesename[chchar])) {
        factories$match[num] = chinesedata$chinesename[chchar]
      }
    }
  }
}


factories

#a = list(b=list("你好","你是谁"),c=list("很好","我是怡辰"),d=list("不好","我不是怡辰"))
#print(a[[2]][2])

#lapply(a[1],function(x) lapply(x, identity))


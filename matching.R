library(readr)
library(tidyverse)
library(tibble)
library(stringr)
library(naniar)
library(pinyin)

setwd("/Users/liuxi/Desktop/Project/2009/Matching_basedon_v5")
Sys.setlocale(category="LC_ALL", locale="Chinese")

# Read files
all_eng = read_csv('name&addrextractionformatch_v5.csv')

# Duplicates have been removed in excel
all_chn = read_csv("all_factories_chinese.csv")


# Divide chinese data set into several provinces
guangdong_chn = filter(all_chn,grepl("广东",all_chn$var5))
fujian_chn = filter(all_chn,grepl("福建",all_chn$var5))
zhejiang_chn = filter(all_chn,grepl("浙江",all_chn$var5))
jiangsu_chn = filter(all_chn,grepl("江苏",all_chn$var5))
shanghai_chn = filter(all_chn,grepl("上海",all_chn$var5))
shandong_chn = filter(all_chn,grepl("山东",all_chn$var5))
other_chn = filter(all_chn,!grepl("广东|福建|浙江|江苏|上海|山东",all_chn$var5))

# Upload the dictionary
dictionary = tribble (~English, ~Chinese,
                      "accessories",c("饰品","装饰","零件","部件","辅料"),
                      "accessory",c("饰品","装饰","零件","部件","辅料"),
                      "arts",c("工艺","艺术","美术","美工"),
                      "art",c("工艺","艺术","美术","美工"),
                      "apparel",c("服装","衣服","制衣","服饰"),
                      "apparels",c("服装","衣服","制衣","服饰"),
                      "appliance",c("器具","器械","装置"),
                      "crafts",c("工艺"),
                      "artcraft",c("工艺","艺术"),
                      "artcrafts",c("工艺","艺术"),
                      "artificial flower",c("假花"),
                      "brushes",c("刷"),
                      "brush",c("刷"),
                      "artware",c("工艺品","美术品"),
                      "bag",c("包"),
                      "bags",c("包"),
                      "bamboo",c("竹子"),
                      "beauty",c("化妆","美容","护肤"),
                      "belt",c("皮带"),
                      "bicycle",c("自行车"),
                      "bicycles",c("自行车"),
                      "blanket",c("毯"),
                      "blankets",c("毯"),
                      "briefcase",c("旅行箱","箱包","箱"),
                      "candle",c("蜡烛","烛"),
                      "carpet",c("地毯"),
                      "cashmere",c("羊绒"),
                      "cast iron",c("铸铁","铸造"),
                      "ceramic",c("陶瓷","陶艺","制陶"),
                      "ceramics",c("陶瓷","陶艺","制陶"),
                      "cleaning",c("清洗","精洗","洗涤"),
                      "clothes",c("服装","衣服","制衣","服饰"),
                      "clothing",c("服装","衣服","制衣","服饰"),
                      "computer hosiery",c(""),
                      "cookware",c("炊具","厨具"),
                      "cosmetic",c("美容","化妆","护肤","美妆"),
                      "cosmetics",c("美容","化妆","护肤","美妆"),
                      "craftwork",c("工艺"),
                      "craftworks",c("工艺"),
                      "cuff link",c("袖扣","链扣"),
                      "cutlery",c("餐具"),
                      "clock",c("钟表","秒表","时钟"),
                      "decorated",c("装饰","布置"),
                      "decoration",c("装饰","布置"),
                      "design",c("设计"),
                      "designs",c("设计"),
                      "device",c("设备","装置","器械","设备","器件"),
                      "deying",c("染色","染料"),
                      "dress",c("连衣裙","裙"),
                      "drygoods",c("纺织品","纺织"),
                      "drygood",c("纺织品","纺织"),
                      "dye",c("染色","染料"),
                      "dyeing",c("染色","染料"),
                      "electric",c("电子"),
                      "electrical",c("电子","电动","导电"),
                      "electromachinery",c("电加工"),
                      "electronic",c("电子","电动","导电"),
                      "electronics",c("电子","电动","导电"),
                      "embroidery",c("绣品","刺绣","绣"),
                      "equipment",c("设备"),
                      "fabric",c("织物","纺织"),
                      "fabrics",c("织物","纺织"),
                      "fashion",c("时尚","时装"),
                      "fashions",c("时尚","时装"),
                      "feather",c("羽毛"),
                      "flower",c("花"),
                      "footwear",c("鞋","制鞋"),
                      "footwears",c("鞋","制鞋"),
                      "frame",c("相框","框架","框艺","画框","镜框"),
                      "frames",c("相框","框架","框艺","画框","镜框"),
                      "framing",c("相框","框架","框艺","画框","镜框"),
                      "furnace",c("火炉","熔炉"),
                      "furnishing",c("装备","家具","家俱"),
                      "furniture",c("家具","家俱"),
                      "garment",c("服装","衣服","制衣","服饰"),
                      "garments",c("服装","衣服","制衣","服饰"),
                      "gift",c("礼品","礼物"),
                      "gifts",c("礼品","礼物"),
                      "giftware",c("礼品","礼物"),
                      "glass",c("玻璃"),
                      "glassware",c("玻璃"),
                      "glove",c("手套"),
                      "gloves",c("手套"),
                      "guitar",c("吉他"),
                      "hairdressing",c("头饰","美发"),
                      "handbag",c("包"),
                      "hand bag",c("手提包","包"),
                      "handbags",c("手提包","包"),
                      "handcraft",c("手工艺"),
                      "handcrafts",c("手工艺"),
                      "handicraft",c("手工艺"),
                      "hardware",c("五金","硬件"),
                      "hardwares",c("五金","硬件"),
                      "hat",c("帽"),
                      "hats",c("帽"),
                      "headgear",c("帽"),
                      "home",c("家居","家用"),
                      "hometexitile",c("床品"),
                      "hometextiles",c("床品"),
                      "household",c("家居","家用"),
                      "houseware",c("家居用品","生活用品","日常用品"),
                      "housewares",c("家居用品","生活用品","日常用品"),
                      "inflatable",c("膨胀"),
                      "inflatables",c("膨胀"),
                      "instrument",c("仪器","工具","器械"),
                      "investment",c("仪器","工具","器械"),
                      "jewelry",c("珠宝"),
                      "knitting",c("编织","针织"),
                      "kitchen",c("厨房"),
                      "knit",c("编织","针织"),
                      "knitgood",c("编织","针织"),
                      "knitgoods",c("编织","针织"),
                      "knitted",c("编织","针织"),
                      "knittingly",c("编织","针织"),
                      "knitwear",c("编织","针织"),
                      "laundry",c("洗涤","洗衣","清洗"),
                      "leather",c("皮具","皮革","皮"),
                      "leathergoods",c("皮具","皮革","皮"),
                      "leathergood",c("皮具","皮革","皮"),
                      "leatherware",c("皮具","皮革","皮"),
                      "life saving",c("救生j"),
                      "lifesaving",c("救生"),
                      "light",c("灯","照明","光","灯光"),
                      "lights",c("灯","照明","光","灯光"),
                      "lighting",c("灯","照明","光","灯光"),
                      "luggage",c("行李箱","皮箱","旅行箱","箱包","行李","旅行"),
                      "machinery",c("机械"),
                      "material",c("材料","用具","布料"),
                      "mechanical",c("机械"),
                      "medical",c("医用","药用"),
                      "melamine ware",c("密胺餐具","美耐皿餐具"),
                      "metal",c("金属","合金","五金"),
                      "metalic",c("金属","合金","五金"),
                      "metalware",c("金属","合金","五金"),
                      "mill",c("磨","机床","切割"),
                      "mirror",c("镜"),
                      "mould",c("模具","浇铸","模制"),
                      "moulding",c("模具","浇铸","模制"),
                      "sweater",c("毛衣","毛衫"),
                      "x'mas",c("圣诞"),
                      "woollen",c("毛线","毛织","羊毛",'毛纺'),
                      "woolen",c("毛线","毛织","羊毛",'毛纺'),
                      "wool",c("羊毛","毛线"),
                      "woodware",c("木制品","木器"),
                      "woodenarts",c("木艺"),
                      "window",c("窗"),
                      "willowcraft",c("柳制品","柳编"),
                      "willow",c("柳制品","柳编"),
                      "wig",c("假发","发套"),
                      "weaving",c("编制","纺织"),
                      "weave",c("编制","纺织"),
                      "utility",c("设施","纺织"),
                      "utensils",c("餐具","用具","器皿"),
                      "utensil",c("餐具","用具","器皿"),
                      "uniform",c("制服","工装","工服"),
                      "umbrella",c("伞"),
                      "traveling",c('旅游'),
                      "travel",c('旅游'),
                      "trading",c("贸易","商贸"),
                      "trade",c("贸易","商贸"),
                      "toy",c("玩具","玩偶"),
                      "toys",c("玩具","玩偶"),
                      "tool",c("工具","用具","器械","机床"),
                      "tools",c("工具","用具","器械","机床"),
                      "tissue",c("纸巾","薄纱"),
                      "tissues",c("纸巾","薄纱"),
                      "timer",c("计时","定时"),
                      "timber",c("木料","木材","木业"),
                      "ties",c("领带"),
                      "tie & gmt",c("领带"),
                      "textiles",c("纺织品","织物"),
                      "textilegarments",c("纺织服装","纺织品","织物"),
                      "textile",c("纺织品","织物"),
                      "technology",c("科技"),
                      "technics",c("技术","手艺"),
                      "technique",c("技术"),
                      "tech",c("科技"),
                      "tape",c("胶带"),
                      "tailoring",c("裁缝","剪切"),
                      "tailored",c("裁缝","剪切"),
                      "swimwear",c('泳'),
                      "supplies",c("用品","物资"),
                      "suitcase",c("装箱","箱包"),
                      "suit",c("西装","套装"),
                      "strawhat",c("草帽"),
                      "strawhats",c("草帽"),
                      "straw",c("草编","草制"),
                      "stone carving",c("石雕"),
                      "steel",c("钢铁","铁器"),
                      "stationary",c("文具"),
                      "stationery",c("文具"),
                      "stainless",c("不锈钢"),
                      "spray",c("喷"),
                      "sportwear",c("运动装"),
                      "sport",c("运动"),
                      "sponge",c("海绵"),
                      "spinning dud",c("纺纱","纺织"),
                      "spinclothing",c("纺纱","纺织"),
                      "spin",c("纺纱","纺织"),
                      "silk",c("绸"),
                      "shoes",c("鞋"),
                      "shirts",c("衫"),
                      "screwdriver",c("螺丝刀"),
                      "rubber",c("橡胶","橡皮"),
                      "printing",c("印刷","打印"),
                      "print",c("印刷","打印"),
                      "power",c("能源"),
                      "pottery",c('陶器','陶瓷','瓷'),
                      "porcelain",c('瓷'),
                      "polymer",c("聚合物"),
                      "plush",c("毛绒"),
                      "plastic",c("塑料","塑胶"),
                      "plastics",c("塑料","塑胶"),
                      "plant",c("植物","种植"),
                      "picture",c("相片","影像","照相","图片"),
                      "pet",c("宠物"),
                      "patio",c("露台"),
                      "paper",c("造纸","纸"),
                      "painting",c("涂料","涂层"),
                      "painted",c("涂料","涂层"),
                      "packing",c("包装","打包"),
                      "packaging",c("包装","打包"),
                      "ornament",c("装饰"),
                      "ornaments",c("装饰"),
                      "office supply",c("办公用品"),
                      "nonwoven",c("无纺",'非纺'),
                      "muffler",c("围巾",'消音器','手套'))

# Attach product names to English dataset
all_eng = mutate(all_eng,
                 productname = NA)

for (n in seq_along(all_eng$s_fcode)){
  for (p in seq_along(dictionary$English)) {
    if (str_detect(all_eng$ftynamelow1[n],dictionary$English[p])){
      all_eng$productname[n] = dictionary$Chinese[p]
    }
  }
}

# Divide english data set into several provinces
guangdong_eng = filter(all_eng,all_eng$province=="guangdong")
fujian_eng = filter(all_eng,all_eng$province=="fujian")
zhejiang_eng = filter(all_eng,all_eng$province=="zhejiang")
jiangsu_eng = filter(all_eng,all_eng$province=="jiangsu")
shanghai_eng = filter(all_eng,all_eng$province=="shanghai")
shandong_eng = filter(all_eng,all_eng$province=="shandong")
#other_eng = filter(all_eng,all_eng$province!="shandong&guangdong&fujian&zhejiang&jiangsu&shanghai")

matching <- function (eng,chn) {
  eng = mutate(eng,
               matched_names=NA,
               matched_addr=NA)
  chn = mutate(chn,
               pynames = py(chn$var2,sep = "",dic = pydic(method = c("toneless"),multi = FALSE, dic = c("pinyin2"))),
               pyprovince = py(chn$var5,sep = "",dic = pydic(method = c("toneless"),multi = FALSE, dic = c("pinyin2"))),
               pyprefecture = py(chn$var6,sep = "",dic = pydic(method = c("toneless"),multi = FALSE, dic = c("pinyin2"))),
               pycounty = py(chn$var7,sep = "",dic = pydic(method = c("toneless"),multi = FALSE, dic = c("pinyin2"))),
               pyaddr = py(chn$var9,sep = "",dic = pydic(method = c("toneless"),multi = FALSE, dic = c("pinyin2"))),
               pytownship = py(chn$var8,sep = "",dic = pydic(method = c("toneless"),multi = FALSE, dic = c("pinyin2"))))
  
  
  for (i in seq_along(eng$matched_names)) {
    for (indexnum in grep(eng$prefecture[i],chn$pyprefecture)) {
      for (addr in chn$pyaddr[indexnum]) {
        if (isTRUE(grepl(eng$streetnumbers[i],addr)) & isTRUE(grepl(eng$streetnames[i],addr))) {
          eng$matched_names[i] = paste(eng$matched_names[i],chn$var2[indexnum],sep=",");
          eng$matched_addr[i] = paste(eng$matched_addr[i],chn$var9[indexnum],sep=",")
        }
      }  
    }
  }
  
  
  for (i in seq_along(eng$matched_names)) {
    if (grepl(",",eng$matched_names[i])) {
      for (indexnum in grep(eng$name2[i],chn$pynames)){ 
        for (chchar in chn$var2[indexnum]){ 
          for (j in lapply(eng$productname[i], function(x) lapply(x, identity))){
            if (grepl(paste(j,collapse = "|"),chchar)) {
              for (n in chchar) {
                if (grepl(n,eng$matched_names[i])) {
                  eng$matched_names[i] = n;
                  eng$matched_addr[i] = chn$var9[chn$var2==n]
                }
              }
            }
          }
        }
      }
    } else if (is.na(eng$matched_names[i])) {
      for (indexnum in grep(eng$name2[i],chn$pynames)){ 
        for (chchar in chn$var2[indexnum]){ 
          for (j in lapply(eng$productname[i], function(x) lapply(x, identity))){
            if (grepl(paste(j,collapse = "|"),chchar)) {
              eng$matched_names[i] = paste(eng$matched_names[i],chn$var2[indexnum],sep=",");
              eng$matched_addr[i] = paste(eng$matched_addr[i],chn$var9[indexnum],sep=",")
            }
          }
        }
      }
    } 
  }
  
  
  for (i in seq_along(eng$matched_names)){
    eng$matched_names[i] = str_replace(string=eng$matched_names[i],pattern="NA,",replacement="");
    eng$matched_addr[i] = str_replace(string=eng$matched_addr[i],pattern="NA,",replacement="")
  }
  return(eng)
}

shanghai = matching(shanghai_eng,shanghai_chn)
#sh_matched=filter(shanghai,!is.na(shanghai$matched_names)) #20
jiangsu = matching(jiangsu_eng,jiangsu_chn)
zhejiang = matching(zhejiang_eng,zhejiang_chn)
shandong = matching(shandong_eng,shandong_chn)
fujian = matching(fujian_eng,fujian_chn)
guangdong = matching(guangdong_eng,guangdong_chn)

combine6prov = rbind(shanghai,jiangsu,zhejiang,shandong,fujian,guangdong)
combine6prov = select(combine6prov,-"productname")
matched_all = filter(combine6prov,!is.na(combine6prov$matched_names))
matched_single_pre = filter(combine6prov,!grepl(",",combine6prov$matched_names))
matched_single = filter(matched_single_pre,!is.na(matched_single_pre$matched_names))

#write_csv(combine6prov,"matched6prov.csv")
#write_csv(matched_single,"matched6prov_single_match_only.csv")
#proc.time() #14min

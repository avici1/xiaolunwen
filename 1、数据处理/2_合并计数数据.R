library(readxl)
library(writexl)
library(dplyr)

#导入数据
hosp_filter<-read_xlsx("F:/文章/小论文/Outpatient/hosp_filter.xlsx") #门诊数据总
hosp_hemo<-read_xlsx("F:/文章/小论文/Outpatient/hosp_hemo.xlsx") #门诊数据脑出血
hosp_infar<-read_xlsx("F:/文章/小论文/Outpatient/hosp_infar.xlsx") #门诊数据脑梗死


data_wr<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/data_all.xlsx")#空气污染数据
data_wr <- data_wr %>%
  mutate(across(where(is.numeric), round, 1))#保留1小数


data_qx<- read_xlsx("F:/文章/小论文/气象-空气污染物数据/data_qx.xlsx")





###########################门诊数据####################################

###########################计数每个日期的入院数########################

library(dplyr)
library(lubridate)

# 创建一个完整的日期范围，假设 '入院日期' 是日期类型
date_range <- seq(min(hosp_filter$`入院日期`), max(hosp_filter$`入院日期`), by = "day")

# 将完整的日期范围转换为数据框
full_dates <- data.frame(`入院日期` = date_range)

# 按 '入院日期' 分组，计算每个日期的计数
hosp_merged <- hosp_filter %>%
  group_by(`入院日期`) %>%
  summarize(counts = n()) %>%
  ungroup()

# 合并完整日期范围与计算出的计数结果，使用 left_join 保证所有日期都保留
hosp_merged <- full_dates %>%
  left_join(hosp_merged, by = "入院日期") %>%
  mutate(counts = ifelse(is.na(counts), 0, counts))  # 将 NA 转换为 0




#脑卒中
hosp_merged <- hosp_filter %>%
  group_by(`入院日期`) %>%    # 按 '入院日期' 分组
  summarize(counts = n()) %>% # 计算每个 '入院日期' 的观测数并保存在 counts 变量中
  ungroup()  

hosp_yearly <- hosp_merged %>%
  mutate(年份 = year(`入院日期`)) %>%  # 提取年份
  group_by(年份) %>%                   # 按年份分组
  summarize(年总入院人数 = sum(counts)) %>%  # 汇总每年人数
  ungroup()

#脑出血
hosp_hemo_merged <- hosp_hemo %>%
  group_by(`入院日期`) %>%    # 按 '入院日期' 分组
  summarize(counts = n()) 

#脑梗
hosp_infar_merged <- hosp_infar %>%
  group_by(`入院日期`) %>%    # 按 '入院日期' 分组
  summarize(counts = n()) 

######################################################################

#################统计年龄分布############################
hosp_filter_count <- hosp_filter %>%
  mutate(age_group = case_when(
    age <= 40 ~ "≤40",
    age >= 41 & age <= 50 ~ "41-50",
    age >= 51 & age <= 60 ~ "51-60",
    age >= 61 & age <= 70 ~ "61-70",
    age >= 71 & age <= 80 ~ "71-80",
    age > 80 ~ ">80"
  ))

# 统计人数和百分比
age_summary <- hosp_filter_count %>%
  group_by(age_group) %>%
  summarise(
    count = n(),
    percent = round(100 * n() / nrow(hosp_filter_count), 2)
  ) %>%
  arrange(match(age_group, c("≤40", "41-50", "51-60", "61-70", "71-80", ">80")))

# 输出结果
print(age_summary)
#############################
################统计诊断分布######################
icd_summary <- hosp_filter %>%
  mutate(ICD3 = substr(主诊断ICD10, 1, 3)) %>%
  group_by(ICD3) %>%
  summarise(
    count = n(),
    percent = round(100 * n() / nrow(hosp_filter), 2)  # 注意分母是全体（包括NA）
  ) %>%
  arrange(desc(count))

#############



##########################空气污染-气象数据############################
#合并

data_all <- inner_join(data_qx, data_wr, by = "date")
write_xlsx(data_all, "F:/文章/小论文/气象-空气污染物数据/data_all.xlsx")

library(summarytools)

options(scipen = 999)  #禁用科学计数法
descr_df_all <- as.data.frame(descr(data_all[, c("rainfall", 
                                                 "eva_cap_b",
                                                "temp_avg","temp_l","temp_h",
                                                "air_p_avg","air_p_water","air_p_l","air_p_h",
                                                "humidity","humidity_l",
                                                "w_speed","max_w_speed","peek_w_speed",
                                                "sunshine","aqi","pm2_5" ,"pm10" ,"co" ,"so2" ,"no2" ,"o3"
                                                )]))
data_all_sum <- round(descr_df_all,2)
write_xlsx(data_all_sum, "F:/文章/小论文/出图/描述性分析-气象-污染.xlsx")


################################数据添补#############################
#由于选择使用前后变量均值填补 而
#变量 evp_cap,max_w_dir,peek_w_dir缺失值过多，因此放弃这三个变量

data_all <- data_all %>% select(-c(eva_cap,max_w_dir,peek_w_dir))


#对剩下数据填补

variables_to_fill <- c("rainfall", "eva_cap_b", "temp_avg", "temp_l", "temp_h",
                       "air_p_avg", "air_p_water", "air_p_l", "air_p_h", "humidity", "humidity_l",
                       "w_speed", "max_w_speed", "peek_w_speed", "sunshine", "aqi", "pm2_5", "pm10", 
                       "co", "so2", "no2", "o3")

# 函数：前后均值填补
fill_na_with_avg <- function(x) {
  for (i in 2:(length(x) - 1)) {
    if (is.na(x[i])) {
      # 计算前后两个值的均值
      x[i] <- mean(c(x[i - 1], x[i + 1]), na.rm = TRUE)
    }
  }
  return(x)
}

# 对指定列进行前后均值填补
for (var in variables_to_fill) {
  # 判断列是否存在且是数值型列
  if (var %in% colnames(data_all) && is.numeric(data_all[[var]])) {
    data_all[[var]] <- fill_na_with_avg(data_all[[var]])
  }
}

write_xlsx(data_all, "F:/文章/小论文/气象-空气污染物数据/data_all_imputted.xlsx")

#####################################################












##############################合并气象-污染-计数###################################
data_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/data_all_imputted.xlsx")


data_all$date<-as.Date(data_all$date)
hosp_hemo_merged$date<-as.Date(hosp_hemo_merged$入院日期)
hosp_infar_merged$date<-as.Date(hosp_infar_merged$入院日期)

hosp_hemo_merged <- hosp_hemo_merged %>% select(-入院日期)
hosp_infar_merged <- hosp_infar_merged %>% select(-入院日期)

all_data_hemo <- left_join(data_all,hosp_hemo_merged,  by = "date")
all_data_infar <- left_join(data_all,hosp_infar_merged,  by = "date")





###########################节假日#############################################
library(lubridate)

# 假设hemo_all是你的数据框，date是日期列
# 创建中国法定节假日的向量（修改为所有年份的01-01，其他节假日也按需要更新）
holidays <- as.Date(c(
  # 2014年
  "2014-01-01",                     # 元旦节
  "2014-01-31", "2014-02-01", "2014-02-02", "2014-02-03", "2014-02-04", "2014-02-05", "2014-02-06",  # 春节假期
  "2014-04-05", "2014-04-06", "2014-04-07",   # 清明节假期
  "2014-05-01",                     # 劳动节
  "2014-06-02",                     # 端午节
  "2014-09-08",                     # 中秋节
  "2014-10-01", "2014-10-02", "2014-10-03", "2014-10-04", "2014-10-05", "2014-10-06", "2014-10-07",   # 国庆节
  
  # 2015年
  "2015-01-01",                     # 元旦节
  "2015-02-18", "2015-02-19", "2015-02-20", "2015-02-21", "2015-02-22", "2015-02-23", "2015-02-24",  # 春节假期
  "2015-04-04", "2015-04-05", "2015-04-06",   # 清明节假期
  "2015-05-01",                     # 劳动节
  "2015-06-20",                     # 端午节
  "2015-09-27",                     # 中秋节
  "2015-10-01", "2015-10-02", "2015-10-03", "2015-10-04", "2015-10-05", "2015-10-06", "2015-10-07",   # 国庆节
  
  # 2016年
  "2016-01-01",                     # 元旦节
  "2016-02-07", "2016-02-08", "2016-02-09", "2016-02-10", "2016-02-11", "2016-02-12", "2016-02-13",   # 春节假期
  "2016-04-02", "2016-04-03", "2016-04-04",   # 清明节假期
  "2016-05-01",                     # 劳动节
  "2016-06-09",                     # 端午节
  "2016-09-15",                     # 中秋节
  "2016-10-01", "2016-10-02", "2016-10-03", "2016-10-04", "2016-10-05", "2016-10-06", "2016-10-07",   # 国庆节
  
  # 2017年
  "2017-01-01",                     # 元旦节
  "2017-01-27", "2017-01-28", "2017-01-29", "2017-01-30", "2017-01-31", "2017-02-01", "2017-02-02",   # 春节假期
  "2017-04-04", "2017-04-05", "2017-04-06",   # 清明节假期
  "2017-05-01",                     # 劳动节
  "2017-05-28",                     # 端午节
  "2017-10-04",                     # 中秋节
  "2017-10-01", "2017-10-02", "2017-10-03", "2017-10-04", "2017-10-05", "2017-10-06", "2017-10-07",   # 国庆节
  
  # 2018年
  "2018-01-01",                     # 元旦节
  "2018-02-15", "2018-02-16", "2018-02-17", "2018-02-18", "2018-02-19", "2018-02-20", "2018-02-21",   # 春节假期
  "2018-04-04", "2018-04-05", "2018-04-06",   # 清明节假期
  "2018-05-01",                     # 劳动节
  "2018-06-18",                     # 端午节
  "2018-09-24",                     # 中秋节
  "2018-10-01", "2018-10-02", "2018-10-03", "2018-10-04", "2018-10-05", "2018-10-06", "2018-10-07",   # 国庆节
  
  # 2019年
  "2019-01-01",                     # 元旦节
  "2019-02-04", "2019-02-05", "2019-02-06", "2019-02-07", "2019-02-08", "2019-02-09", "2019-02-10",   # 春节假期
  "2019-04-05", "2019-04-06", "2019-04-07",   # 清明节假期
  "2019-05-01",                     # 劳动节
  "2019-06-07",                     # 端午节
  "2019-09-13",                     # 中秋节
  "2019-10-01", "2019-10-02", "2019-10-03", "2019-10-04", "2019-10-05", "2019-10-06", "2019-10-07"    # 国庆节
))
all_data_hemo$date <- as.Date(all_data_hemo$date)
all_data_infar$date <- as.Date(all_data_infar$date)

all_data_hemo$holiday <- ifelse(all_data_hemo$date %in% holidays, 1, 0)
all_data_infar$holiday <- ifelse(all_data_infar$date %in% holidays, 1, 0)


all_data_hemo <- subset(all_data_hemo, date <= "2020-01-01")
all_data_infar <- subset(all_data_infar, date <= "2020-01-01")

###############################################




#######把NA替换为0#######
all_data_hemo<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_hemo.xlsx")
all_data_infar<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_infar.xlsx")


all_data_hemo$counts[is.na(all_data_hemo$counts)] <- 0
all_data_infar$counts[is.na(all_data_infar$counts)] <- 0



write_xlsx(all_data_hemo,  "F:/文章/小论文/气象-空气污染物数据/all_data_hemo.xlsx")
write_xlsx(all_data_infar,  "F:/文章/小论文/气象-空气污染物数据/all_data_infar.xlsx")
####################################################################################







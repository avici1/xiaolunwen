
library(writexl)
library(readxl)
library(dplyr)
library(haven)
library(summarytools)
library(Hmisc)


data_14_21 <- read_sas("F:/文章/小论文/气象-空气污染物数据/air.sas7bdat")
data_qixiang <- read_sas("F:/文章/小论文/气象-空气污染物数据/meteorolog.sas7bdat")

data_qixiang$date <- as.Date(paste(data_qixiang$year, data_qixiang$month, data_qixiang$day, sep = "-"))

##################排序 降水-蒸发-气温-气压-湿度-风速-风向######################

data_qx <- data.frame(
  #日期
  date = data_qixiang$date,
  #降水
  rainfall = data_qixiang$jiangshui,
  #蒸发
  eva_cap = data_qixiang$zhengfa,
  eva_cap_b = data_qixiang$zhengfada,
  #气温
  temp_avg = data_qixiang$pingjunqiwen,
  temp_l = data_qixiang$diqiwen,
  temp_h = data_qixiang$gaoqiwen,
  #气压
  air_p_avg = data_qixiang$pingjunqiya,
  air_p_water = data_qixiang$pingjunshuiqiya,
  air_p_l = data_qixiang$diqiya,
  air_p_h = data_qixiang$gaoqiya,
  #湿度
  humidity = data_qixiang$pingjunshidu,
  humidity_l = data_qixiang$zuixiaoshidu,
  #风速
  w_speed = data_qixiang$twominfengsu,
  max_w_speed = data_qixiang$zuidafengsu,
  peek_w_speed = data_qixiang$jidafengsu,
  #风向
  max_w_dir = data_qixiang$zuidafengxiang,
  peek_w_dir = data_qixiang$jidafengxiang,
  #日照时长
  sunshine = data_qixiang$rizhao
)
###################################

###########处理离群值/缺失值##############
data_qx$rainfall[data_qx$rainfall > 10000] <- NA
data_qx$eva_cap[data_qx$eva_cap > 10000] <- NA
data_qx$eva_cap_b[data_qx$eva_cap_b > 10000] <- NA
data_qx$temp_avg[data_qx$temp_avg > 10000]  <- NA
data_qx$temp_l[data_qx$temp_l > 10000]  <- NA
data_qx$temp_h[data_qx$temp_h > 10000]  <- NA
data_qx$air_p_avg[data_qx$air_p_avg > 10000]  <- NA
data_qx$air_p_water[data_qx$air_p_water > 10000]  <- NA
data_qx$air_p_l[data_qx$air_p_l > 10000]  <- NA
data_qx$air_p_h[data_qx$air_p_h > 10000]  <- NA
data_qx$humidity[data_qx$humidity > 10000]  <- NA
data_qx$humidity_l[data_qx$humidity_l > 10000]  <- NA
data_qx$w_speed[data_qx$w_speed > 10000]  <- NA
data_qx$max_w_speed[data_qx$max_w_speed > 10000]  <- NA
data_qx$peek_w_speed[data_qx$peek_w_speed > 10000]  <- NA
data_qx$max_w_dir[data_qx$max_w_dir > 10000]  <- NA
data_qx$peek_w_dir[data_qx$peek_w_dir > 10000]  <- NA
data_qx$sunshine[data_qx$sunshine > 10000]  <- NA

###################################






#################查看描述性统计内容####################
library(summarytools)

options(scipen = 999)  #禁用科学计数法
descr_df_all <- as.data.frame(descr(data_qx[, c("rainfall", 
                                                 "eva_cap", "eva_cap_b",
                                                 "temp_avg","temp_l","temp_h",
                                                 "air_p_avg","air_p_water","air_p_l","air_p_h",
                                                 "humidity","humidity_l",
                                                 "w_speed","max_w_speed","peek_w_speed",
                                                 "max_w_dir","peek_w_dir",
                                                 "sunshine")]))
data_qx_sum <- round(descr_df_all,2)

write_xlsx(data_qx,"F:/文章/小论文/气象-空气污染物数据/data_qx.xlsx")
write_xlsx(data_qx_sum,path="F:/文章/小论文/气象-空气污染物数据/data_qx_sum.xlsx" )

##################周末################
data_qx<- read_xlsx("F:/文章/小论文/气象-空气污染物数据/data_qx.xlsx")

data_qx$weekday<-weekdays(data_qx$date)
data_qx$weekend<-ifelse(data_qx$weekday == "星期六"|data_qx$weekday == "星期日",1 ,0)

write_xlsx(data_qx,"F:/文章/小论文/气象-空气污染物数据/data_qx.xlsx")




###############查找缺失#######################
data_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/data_all.xlsx")#空气污染数据
data_qx<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/data_qx.xlsx")#气象数据


describe(data_all)
describe(data_qx)



library(Amelia)
imputed_data_qx <- amelia(data_qx, m = 5, idvars = c("date", "weekday"))

library(writexl)

# 将每个插补数据集保存为一个工作表
imputed_data_list <- lapply(1:length(imputed_data_qx$imputations), function(i) {
  # 返回每个插补数据集
  imputed_data_qx$imputations[[i]]
})

# 保存为 Excel 文件，sheet names 自动生成
write_xlsx(imputed_data_list, path = "F:/文章/小论文/气象-空气污染物数据/imputed_data_qx.xlsx")




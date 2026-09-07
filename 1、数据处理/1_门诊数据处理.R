library(readxl)
library(dplyr)
library(writexl)
library(ggplot2)
library(summarytools)
library(Hmisc)

data <- read.csv("F:/文章/小论文/Outpatient/门诊数据.csv")
hosp<- read_xlsx("F:/文章/小论文/Outpatient/住院数据.xlsx")





#筛选ICD10包含I61 I60 I63 G45的对象
hosp_filter <- hosp %>%
  filter(grepl("^I61|^I63|^I60|^G45", 主诊断ICD10))










#计算年龄
hosp_filter$入院日期 <- as.Date(hosp_filter$入院日期)
hosp_filter$生日 <- as.Date(hosp_filter$生日)
hosp_filter$age<- round((as.numeric(hosp_filter$入院日期-hosp_filter$生日))/365.25,digits=1)
hosp_filter$hosp_times<-as.numeric(hosp_filter$住院次数)


#脑出血
hosp_hemo <- hosp_filter %>%
  filter(grepl("^I61|^I60", 主诊断ICD10))

#脑梗死
hosp_infar <- hosp_filter %>%
  filter(grepl("^I63|^G45", 主诊断ICD10))

#查看数据

describe(hosp_hemo)
describe(hosp_infar)
describe(hosp_filter)

#输出数据
write_xlsx(hosp_hemo,"F:/文章/小论文/Outpatient/hosp_hemo.xlsx")               #确定数据框
write_xlsx(hosp_infar,"F:/文章/小论文/Outpatient/hosp_infar.xlsx")               #确定数据框
write_xlsx(hosp_filter,"F:/文章/小论文/Outpatient/hosp_filter.xlsx")               #确定数据框


table(hosp_hemo$性别)
table(hosp_infar$性别)


mean(hosp_hemo$age, na.rm = TRUE)
mean(hosp_infar$age, na.rm = TRUE)














###########################基线表格########################################

#年龄分布
workdir<-"F:/文章/小论文/出图/描述性分析"
setwd(workdir)

png(file = "年龄分布hemo直方图.png", width = 1500, height = 1000, res = 300)
ggplot(hosp_hemo, aes(x = age)) + 
  geom_histogram(binwidth = 1,        # 每个柱子的宽度
                 fill = "lightblue",  # 柱子的填充颜色
                 color = "black",     # 边框颜色
                 alpha = 0.7) +       # 柱子的透明度
  labs(title = "出血性脑卒中年龄分布直方图",        # 设置标题
       x = "年龄",                   # 设置x轴标签
       y = "频数") +                 # 设置y轴标签
  theme_minimal()  
dev.off()

png(file = "年龄分布infar直方图.png", width = 1500, height = 1000, res = 300)
ggplot(hosp_infar, aes(x = age)) + 
  geom_histogram(binwidth = 1,        # 每个柱子的宽度
                 fill = "lightblue",  # 柱子的填充颜色
                 color = "black",     # 边框颜色
                 alpha = 0.7) +       # 柱子的透明度
  labs(title = "缺血性脑卒中年龄分布直方图",        # 设置标题
       x = "年龄",                   # 设置x轴标签
       y = "频数") +                 # 设置y轴标签
  theme_minimal()  
dev.off()





png(file = "脑卒中年龄分布直方图.png", width = 1500, height = 1000, res = 300)
ggplot(hosp_filter, aes(x = age)) + 
  geom_histogram(binwidth = 1,        # 每个柱子的宽度
                 fill = "lightblue",  # 柱子的填充颜色
                 color = "black",     # 边框颜色
                 alpha = 0.7) +       # 柱子的透明度
  labs(title = "脑卒中年龄分布直方图",        # 设置标题
       x = "年龄",                   # 设置x轴标签
       y = "频数") +                 # 设置y轴标签
  theme_minimal()  
dev.off()











#住院次数分布
png(file = "住院次数直方图.png", width = 1500, height = 1000, res = 300)
ggplot(hosp_filter, aes(x = hosp_times)) + 
  geom_histogram(binwidth = 1,        # 每个柱子的宽度
                 fill = "lightblue",  # 柱子的填充颜色
                 color = "black",     # 边框颜色
                 alpha = 0.7) +       # 柱子的透明度
  labs(title = "住院次数直方图",        # 设置标题
       x = "住院次数",                   # 设置x轴标签
       y = "频数") +                 # 设置y轴标签
  theme_minimal()  
dev.off()

#描述性统计
hosp_filter$gender<-ifelse(hosp_filter$性别 == "男",1,0)
gender<-as.data.frame(table(hosp$gender))
write_xlsx(gender,"F:/文章/小论文/出图/gender.xlsx")


ggplot(hosp_filter, aes(x = gender)) + 
  geom_histogram(binwidth = 1,        # 每个柱子的宽度
                 fill = "lightblue",  # 柱子的填充颜色
                 color = "black",     # 边框颜色
                 alpha = 0.7) +       # 柱子的透明度
  labs(title = "住院次数直方图",        # 设置标题
       x = "住院次数",                   # 设置x轴标签
       y = "频数") +                 # 设置y轴标签
  theme_minimal()  


age_distr <- as.data.frame(descr(hosp_filter[, c("hosp_times","age")]))
age_distr<- round(age_distr,2)
write_xlsx(age_distr,"F:/文章/小论文/出图/住院次数_年龄分布.xlsx")





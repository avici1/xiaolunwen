library(readxl)
library(writexl)
library(dplyr)
library(haven)
library(ggplot2)
library(tibble)
library(summarytools)






all_data_hemo<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_hemo.xlsx") #门诊数据总
all_data_infar<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_infar.xlsx")
stroke_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_stroke.xlsx")

#计数
data_h<-all_data_hemo
data_i<-all_data_infar
data_h$cumulative_var_name <- cumsum(data_h$counts)
data_i$cumulative_var_name <- cumsum(data_i$counts)


#hemo5013例 infar16519例   合计21532例



#######################################################描述性分析图##########################################

workdir<-"F:/文章/小论文/出图/描述性分析"
setwd(workdir)

#入院时间分布图
png(file = "脑出血入院数时间.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = counts)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "脑出血入院数统计") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()


png(file = "脑缺血入院数时间.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_infar, aes(x = date, y = counts)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "脑缺血入院数统计") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()


png(file = "脑卒中入院数时间.png", width = 3000, height = 1000, res = 300)
ggplot(stroke_all, aes(x = date, y = counts)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "脑卒中入院数统计") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()







####################气象数据###################

#降雨
png(file = "日期-降雨.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = rainfall)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "降水", title = "日期-降雨") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

#蒸发
png(file = "日期-蒸发.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = eva_cap_b)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "蒸发", title = "日期-蒸发") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

#温度

long_data <- all_data_hemo %>%
  pivot_longer(cols = c(temp_avg, temp_l, temp_h), 
               names_to = "temperature_type", 
               values_to = "temperature")

# 使用 ggplot 绘制图形
png(file = "日期-温度.png", width = 3000, height = 1000, res = 300)
ggplot(long_data, aes(x = date, y = temperature, color = temperature_type)) +
  geom_point(size = 0.5) +
  geom_line(stat = "identity", size = 0.1) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "温度", title = "日期-温度") +   # 添加标题和轴标签
  scale_color_manual(values = c("red", "blue", "green")) +  # 自定义颜色
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 旋转x轴标签
dev.off()



png(file = "日期-温度avg.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = temp_avg)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "平均温度", title = "日期-温度avg") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

png(file = "日期-温度l.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = temp_l)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "日期-温度l") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

png(file = "日期-温度h.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = temp_h)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "日期-温度h") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

#气压


png(file = "日期-气压avg.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = air_p_avg)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "平均气压", title = "日期-气压avg") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

png(file = "日期-气压l.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = air_p_l)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "日期-气压l") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

png(file = "日期-气压h.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = air_p_h)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "日期-气压h") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

png(file = "日期-水蒸气压.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = air_p_water)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "日期-水蒸气压") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()


#湿度
png(file = "日期-湿度.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = humidity)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "相对湿度", title = "日期-湿度") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

png(file = "日期-湿度_l.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = humidity_l)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "日期-最小湿度") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

#风速
png(file = "日期-平均风速.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = w_speed)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "日期-平均风速") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

png(file = "日期-最大风速.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = max_w_speed)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "日期-最大风速") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

png(file = "日期-极大风速.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = peek_w_speed)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "日期-极大风速") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()

#日照
png(file = "日期-日照.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = sunshine)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "日照", title = "日期-日照") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()












#################空气污染数据#####################
#aqi
png(file = "日期-aqi.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = aqi)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "入院数", title = "日期-aqi") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()


#pm2.5
png(file = "日期-pm2_5.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = pm2_5)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "PM2.5", title = "日期-pm2_5") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()


#pm10
png(file = "日期-pm10.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = pm10)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "PM10", title = "日期-pm10") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()


#so2
png(file = "日期-so2.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = so2)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "SO2", title = "日期-so2") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()


#no2
png(file = "日期-no2.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = no2)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "NO2", title = "日期-no2") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()


#co
png(file = "日期-co.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = co)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "CO", title = "日期-co") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()


#o3
png(file = "日期-o3.png", width = 3000, height = 1000, res = 300)
ggplot(all_data_hemo, aes(x = date, y = o3)) +
  geom_point(color="red",size=1)+
  geom_line(stat = "identity", color = "black",size=0.5) +   # 用 'stat="identity"' 显示实际的计数
  labs(x = "日期", y = "O3", title = "日期-o3") +   # 添加标题和轴标签
  theme_minimal() +   # 使用简洁主题
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
dev.off()


####################################




##############空气污染数据三线表########################


options(scipen = 999)
hemo_desc<-as.data.frame(descr(all_data_hemo))
write_xlsx(hemo_desc,"F:/文章/小论文/出图/描述性分析/空气-气象数据描述.xlsx")



# 设定标准值
standards <- c(pm2_5 = 35, pm10 = 70, so2 = 60, no2 = 40, o3 = 200, co = 2)

# 计算超标的观测次数
count_over_limit <- sapply(names(standards), function(pollutant) {
  sum(all_data_hemo[[pollutant]] > standards[pollutant], na.rm = TRUE)
})




#############相关系数矩阵#################
cor_matrix <- as.data.frame(cor(all_data_hemo[, c("pm2_5", "pm10", "so2", "co", "no2", "o3")], use = "complete.obs"))
write_xlsx(cor_matrix,"F:/文章/小论文/出图/描述性分析/相关系数矩阵.xlsx")

cor_matrix_QX <- as.data.frame(cor(all_data_hemo[, c("temp_avg", "air_p_avg", "eva_cap_b", "humidity", "rainfall")], use = "complete.obs"))
write_xlsx(cor_matrix_QX,"F:/文章/小论文/出图/描述性分析/相关系数矩阵_气象.xlsx")

cor_matrix_all<-as.data.frame(cor(all_data_hemo[, c("temp_avg", "air_p_avg", "eva_cap_b", "humidity", "rainfall","pm2_5", "pm10", "so2", "co", "no2", "o3")], use = "complete.obs"))
write_xlsx(cor_matrix_all,"F:/文章/小论文/出图/描述性分析/相关系数矩阵_全部.xlsx")










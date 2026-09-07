library(haven)
library(readxl)
library(dlnm)
library(ggplot2)
library(splines) 
library(writexl)
library(dplyr)



#汇总


hemo_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_hemo.xlsx") #门诊数据总
infar_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_infar.xlsx")

####################合成脑卒中##############################
merged_data <- left_join(hemo_all, infar_all, by = "date", suffix = c("_hemo", "_infar"))

# 计算 counts 的和，并保存到 counts 变量中
merged_data <- merged_data %>%
  mutate(counts = coalesce(counts_hemo, 0) + coalesce(counts_infar, 0))

# 如果需要，可以删除 `counts_hemo` 和 `counts_infar` 列
merged_data <- merged_data %>%
  select(date, counts)  # 保留 date 和 counts 列

hemo_all<-hemo_all %>% select(-counts)

stroke_all<-left_join(merged_data,hemo_all , by = "date")
write_xlsx(stroke_all,"F:/文章/小论文/气象-空气污染物数据/all_data_stroke.xlsx")
###################################################



stroke_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_stroke.xlsx")










####################2024-1-5改################################
##########空气污染数据Lag改为30天


stroke_cb.so2 = crossbasis(stroke_all$so2, lag=14, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=2))
stroke_cb.pm10 = crossbasis(stroke_all$pm10, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="poly", degree=4))
stroke_cb.pm2_5 = crossbasis(stroke_all$pm2_5, lag=14, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=5))
stroke_cb.o3 = crossbasis(stroke_all$o3, lag=14, argvar=list(fun="poly", degree=1), arglag=list(fun="ns", df=2))
stroke_cb.no2 = crossbasis(stroke_all$no2, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="ns", df=2))
stroke_cb.co = crossbasis(stroke_all$co, lag=14, argvar=list(fun="ns", df=2), arglag=list(fun="ns", df=5))

stroke_cb.temp_avg = crossbasis(stroke_all$temp_avg, lag=14, argvar=list(fun="poly", degree=1), arglag=list(fun="ns", df=4))
stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=14, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=2))
stroke_cb.humidity = crossbasis(stroke_all$humidity, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="ns", df=3))
stroke_cb.eva_cap_b = crossbasis(stroke_all$eva_cap_b, lag=14, argvar=list(fun="ns", df=2), arglag=list(fun="ns", df=2))
stroke_cb.air_p_avg = crossbasis(stroke_all$air_p_avg, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="poly", degree=3))


stroke_model_pm2_5 = glm(counts ~ stroke_cb.pm2_5 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm10 = glm(counts ~ stroke_cb.pm10 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_so2 = glm(counts ~ stroke_cb.so2 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_co = glm(counts ~ stroke_cb.co + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_no2 = glm(counts ~ stroke_cb.no2 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_o3 = glm(counts ~ stroke_cb.o3 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_temp_avg = glm(counts ~ stroke_cb.temp_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_air_p_avg = glm(counts ~ stroke_cb.air_p_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_eva_cap_b = glm(counts ~ stroke_cb.eva_cap_b + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_humidity = glm(counts ~ stroke_cb.humidity + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_rainfall = glm(counts ~ stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 



#stroke建模作图程序
stroke_pred1.co = crosspred(stroke_cb.co, stroke_model_co, cen=round(median(stroke_all$co)), bylag=0.2) # 拟合模型计算 RR 以及 RR 的置信区间， cen 选取对照点作为 RR 分母，bylag 表示 lag 的切片单位。
stroke_pred1.pm2_5 = crosspred(stroke_cb.pm2_5, stroke_model_pm2_5, cen=round(median(stroke_all$pm2_5)), bylag=0.2)
stroke_pred1.pm10 = crosspred(stroke_cb.pm10, stroke_model_pm10, cen=round(median(stroke_all$pm10)), bylag=0.2) 
stroke_pred1.so2 = crosspred(stroke_cb.so2, stroke_model_so2, cen=round(median(stroke_all$so2)), bylag=0.2) 
stroke_pred1.no2 = crosspred(stroke_cb.no2, stroke_model_no2, cen=round(median(stroke_all$no2)), bylag=0.2) 
stroke_pred1.o3 = crosspred(stroke_cb.o3, stroke_model_o3, cen=round(median(stroke_all$o3)), bylag=0.2) 
stroke_pred1.temp_avg = crosspred(stroke_cb.temp_avg, stroke_model_temp_avg, cen=round(median(stroke_all$temp_avg)), bylag=0.2) # 拟合模型计算 RR 以及 RR 的置信区间， cen 选取对照点作为 RR 分母，bylag 表示 lag 的切片单位。
stroke_pred1.air_p_avg = crosspred(stroke_cb.air_p_avg, stroke_model_air_p_avg, cen=round(median(stroke_all$air_p_avg)), bylag=0.2)
stroke_pred1.eva_cap_b = crosspred(stroke_cb.eva_cap_b, stroke_model_eva_cap_b, cen=round(median(stroke_all$eva_cap_b)), bylag=0.2) 
stroke_pred1.humidity = crosspred(stroke_cb.humidity, stroke_model_humidity, cen=round(median(stroke_all$humidity)), bylag=0.2) 
stroke_pred1.rainfall = crosspred(stroke_cb.rainfall, stroke_model_rainfall, cen=round(median(stroke_all$rainfall)), bylag=0.2) 


#输出RR值
#空气污染数据RR
RR_wr <- data.frame()

# 需要循环的变量集合
char_wr <- c("co", "pm2_5", "pm10", "so2", "no2", "o3")

# 一层循环
for(ii in char_wr){
  dynamic_var1 <- paste0("stroke_pred1.", ii, "$matRRfit")
  dynamic_var2 <- paste0("stroke_pred1.", ii, "$matRRlow")
  dynamic_var3 <- paste0("stroke_pred1.", ii, "$matRRhigh")
  
  RRfit<-as.data.frame(t(apply(eval(parse(text = dynamic_var1)), 2, max)))
  RRl <- as.data.frame(t(apply(eval(parse(text = dynamic_var2)), 2, max)))
  RRh <- as.data.frame(t(apply(eval(parse(text = dynamic_var3)), 2, max)))
  RR_c<-rbind(RRfit,RRl,RRh)[, c("lag0","lag1","lag10","lag20", "lag30")]
  RR_ct<-as.data.frame(t(RR_c))
  RR_ct1<-cbind(ii,RR_ct)
  RR_wr<-rbind(RR_wr,RR_ct1)
}
RR_wr$V1 <- round(RR_wr$V1, 3)
RR_wr$V2 <- round(RR_wr$V2, 3)
RR_wr$V3 <- round(RR_wr$V3, 3)

RR_wr$V4 <-  paste(RR_wr$V1, "(", RR_wr$V2, ",", RR_wr$V3, ")", sep = "")


#气象数据RR
char_qx <- c( "temp_avg", "air_p_avg", "eva_cap_b", "humidity", "rainfall")
RR_qx <- data.frame()
# 一层循环
for(ii in char_qx){
  dynamic_var1 <- paste0("stroke_pred1.", ii, "$matRRfit")
  dynamic_var2 <- paste0("stroke_pred1.", ii, "$matRRlow")
  dynamic_var3 <- paste0("stroke_pred1.", ii, "$matRRhigh")
  
  RRfit<-as.data.frame(t(apply(eval(parse(text = dynamic_var1)), 2, max)))
  RRl <- as.data.frame(t(apply(eval(parse(text = dynamic_var2)), 2, max)))
  RRh <- as.data.frame(t(apply(eval(parse(text = dynamic_var3)), 2, max)))
  RR_c<-rbind(RRfit,RRl,RRh)[, c("lag0","lag1","lag3")]
  RR_ct<-as.data.frame(t(RR_c))
  RR_ct1<-cbind(ii,RR_ct)
  RR_qx<-rbind(RR_qx,RR_ct1)
}
RR_qx$V1 <- round(RR_qx$V1, 3)
RR_qx$V2 <- round(RR_qx$V2, 3)
RR_qx$V3 <- round(RR_qx$V3, 3)

RR_qx$V4 <-  paste(RR_qx$V1, "(", RR_qx$V2, ",", RR_qx$V3, ")", sep = "")






####################2024-1-5改################################



























#########################空气污染物#########################
#aqi  pm2.5  pm10  so2  co  no2  o3
#共7个
hemo_cb.pm2_5 = crossbasis(hemo_all$pm2_5, lag=14, argvar=list(fun="ns", df=3),arglag=list(fun="ns",df=2))
infar_cb.pm2_5 = crossbasis(infar_all$pm2_5, lag=14, argvar=list(fun="bs", degree=5), arglag=list(fun="poly", degree=4))

hemo_cb.pm10 = crossbasis(hemo_all$pm10, lag=14, argvar=list(fun="ns", df=2),arglag=list(fun="ns",df=2))
infar_cb.pm10 = crossbasis(infar_all$pm10, lag=14, argvar=list(fun="ns", df=5), arglag=list(fun="ns", df=5))

hemo_cb.so2 = crossbasis(hemo_all$so2, lag=14, argvar=list(fun="ns", df=2),arglag=list(fun="poly",degree=5))
infar_cb.so2 = crossbasis(infar_all$so2, lag=14, argvar=list(fun="ns", df=4), arglag=list(fun="poly", degree=5))


hemo_cb.co = crossbasis(hemo_all$co, lag=14, argvar=list(fun="ns", df=2),arglag=list(fun="ns",df=2))
infar_cb.co = crossbasis(infar_all$co, lag=14, argvar=list(fun="ns", df=2), arglag=list(fun="poly", degree=2))

hemo_cb.no2 = crossbasis(hemo_all$no2, lag=14, argvar=list(fun="ns", df=2),arglag=list(fun="ns",df=2))
infar_cb.no2 = crossbasis(infar_all$no2, lag=14, argvar=list(fun="poly", degree=4), arglag=list(fun="poly", degree=5))

hemo_cb.o3 = crossbasis(hemo_all$o3, lag=14, argvar=list(fun="ns", df=3),arglag=list(fun="poly",degree=5))
infar_cb.o3 = crossbasis(infar_all$o3, lag=14, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=2))


#单因素模型构建

hemo_model_pm2_5 = glm(counts ~ hemo_cb.pm2_5 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_model_pm10 = glm(counts ~ hemo_cb.pm10 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_model_so2 = glm(counts ~ hemo_cb.so2 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_model_co = glm(counts ~ hemo_cb.co + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_model_no2 = glm(counts ~ hemo_cb.no2 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_model_o3 = glm(counts ~ hemo_cb.o3 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all) 

infar_model_pm2_5 = glm(counts ~ infar_cb.pm2_5 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_model_pm10 = glm(counts ~ infar_cb.pm10 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_model_so2 = glm(counts ~ infar_cb.so2 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_model_co = glm(counts ~ infar_cb.co + ns(date,8*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_model_no2 = glm(counts ~ infar_cb.no2 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_model_o3 = glm(counts ~ infar_cb.o3 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), infar_all) 

####################################################

#########################气象指标###########################
#temp air_p eva humidity rainfall

hemo_cb.temp_avg = crossbasis(hemo_all$temp_avg, lag=14, argvar=list(fun="poly", degree=1),arglag=list(fun="bs",df=5))
infar_cb.temp_avg = crossbasis(infar_all$temp_avg, lag=14, argvar=list(fun="poly", degree=3), arglag=list(fun="poly", degree=1))

hemo_cb.air_p_avg = crossbasis(hemo_all$air_p_avg, lag=14, argvar=list(fun="ns", df=1),arglag=list(fun="poly",degree=2))
infar_cb.air_p_avg = crossbasis(infar_all$air_p_avg, lag=14, argvar=list(fun="ns", df=3), arglag=list(fun="poly", degree=1))

hemo_cb.eva_cap_b = crossbasis(hemo_all$eva_cap_b, lag=14, argvar=list(fun="ns", df=2),arglag=list(fun="ns",df=2))
infar_cb.eva_cap_b = crossbasis(infar_all$eva_cap_b, lag=14, argvar=list(fun="ns", df=3), arglag=list(fun="ns", df=3))

hemo_cb.humidity = crossbasis(hemo_all$humidity, lag=14, argvar=list(fun="ns", df=1),arglag=list(fun="poly",degree=2))
infar_cb.humidity = crossbasis(infar_all$humidity, lag=14, argvar=list(fun="ns", df=3), arglag=list(fun="poly", degree=1))

hemo_cb.rainfall = crossbasis(hemo_all$rainfall, lag=14, argvar=list(fun="ns", df=1),arglag=list(fun="poly",degree=2))
infar_cb.rainfall = crossbasis(infar_all$rainfall, lag=14, argvar=list(fun="ns", df=3), arglag=list(fun="poly", degree=1))

#单因素模型构建

hemo_model_temp_avg = glm(counts ~ hemo_cb.temp_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_model_air_p_avg = glm(counts ~ hemo_cb.air_p_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_model_eva_cap_b = glm(counts ~ hemo_cb.eva_cap_b + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_model_humidity = glm(counts ~ hemo_cb.humidity + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_model_rainfall = glm(counts ~ hemo_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all) 

infar_model_temp_avg = glm(counts ~ infar_cb.temp_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_model_air_p_avg = glm(counts ~ infar_cb.air_p_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_model_eva_cap_b = glm(counts ~ infar_cb.eva_cap_b + ns(date,8*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_model_humidity = glm(counts ~ infar_cb.humidity + ns(date,8*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_model_rainfall = glm(counts ~ infar_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), infar_all) 

############################################################

##################脑卒中-空气污染-气象#######################


stroke_cb.so2 = crossbasis(stroke_all$so2, lag=14, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=2))
stroke_cb.pm10 = crossbasis(stroke_all$pm10, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="poly", degree=4))
stroke_cb.pm2_5 = crossbasis(stroke_all$pm2_5, lag=14, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=5))
stroke_cb.o3 = crossbasis(stroke_all$o3, lag=14, argvar=list(fun="poly", degree=1), arglag=list(fun="ns", df=2))
stroke_cb.no2 = crossbasis(stroke_all$no2, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="ns", df=2))
stroke_cb.co = crossbasis(stroke_all$co, lag=14, argvar=list(fun="ns", df=2), arglag=list(fun="ns", df=5))

stroke_cb.temp_avg = crossbasis(stroke_all$temp_avg, lag=14, argvar=list(fun="poly", degree=1), arglag=list(fun="ns", df=4))
stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=14, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=2))
stroke_cb.humidity = crossbasis(stroke_all$humidity, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="ns", df=3))
stroke_cb.eva_cap_b = crossbasis(stroke_all$eva_cap_b, lag=14, argvar=list(fun="ns", df=2), arglag=list(fun="ns", df=2))
stroke_cb.air_p_avg = crossbasis(stroke_all$air_p_avg, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="poly", degree=3))


stroke_model_pm2_5 = glm(counts ~ stroke_cb.pm2_5 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm10 = glm(counts ~ stroke_cb.pm10 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_so2 = glm(counts ~ stroke_cb.so2 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_co = glm(counts ~ stroke_cb.co + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_no2 = glm(counts ~ stroke_cb.no2 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_o3 = glm(counts ~ stroke_cb.o3 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_temp_avg = glm(counts ~ stroke_cb.temp_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_air_p_avg = glm(counts ~ stroke_cb.air_p_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_eva_cap_b = glm(counts ~ stroke_cb.eva_cap_b + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_humidity = glm(counts ~ stroke_cb.humidity + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_rainfall = glm(counts ~ stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 



#stroke建模作图程序
stroke_pred1.co = crosspred(stroke_cb.co, stroke_model_co, cen=round(median(stroke_all$co)), bylag=0.2) # 拟合模型计算 RR 以及 RR 的置信区间， cen 选取对照点作为 RR 分母，bylag 表示 lag 的切片单位。
stroke_pred1.pm2_5 = crosspred(stroke_cb.pm2_5, stroke_model_pm2_5, cen=round(median(stroke_all$pm2_5)), bylag=0.2)
stroke_pred1.pm10 = crosspred(stroke_cb.pm10, stroke_model_pm10, cen=round(median(stroke_all$pm10)), bylag=0.2) 
stroke_pred1.so2 = crosspred(stroke_cb.so2, stroke_model_so2, cen=round(median(stroke_all$so2)), bylag=0.2) 
stroke_pred1.no2 = crosspred(stroke_cb.no2, stroke_model_no2, cen=round(median(stroke_all$no2)), bylag=0.2) 
stroke_pred1.o3 = crosspred(stroke_cb.o3, stroke_model_o3, cen=round(median(stroke_all$o3)), bylag=0.2) 
stroke_pred1.temp_avg = crosspred(stroke_cb.temp_avg, stroke_model_temp_avg, cen=round(median(stroke_all$temp_avg)), bylag=0.2) # 拟合模型计算 RR 以及 RR 的置信区间， cen 选取对照点作为 RR 分母，bylag 表示 lag 的切片单位。
stroke_pred1.air_p_avg = crosspred(stroke_cb.air_p_avg, stroke_model_air_p_avg, cen=round(median(stroke_all$air_p_avg)), bylag=0.2)
stroke_pred1.eva_cap_b = crosspred(stroke_cb.eva_cap_b, stroke_model_eva_cap_b, cen=round(median(stroke_all$eva_cap_b)), bylag=0.2) 
stroke_pred1.humidity = crosspred(stroke_cb.humidity, stroke_model_humidity, cen=round(median(stroke_all$humidity)), bylag=0.2) 
stroke_pred1.rainfall = crosspred(stroke_cb.rainfall, stroke_model_rainfall, cen=round(median(stroke_all$rainfall)), bylag=0.2) 



#输出RR
RR_all <- data.frame()

# 需要循环的变量集合
char_wr <- c("co", "pm2_5", "pm10", "so2", "no2", "o3", "temp_avg", "air_p_avg", "eva_cap_b", "humidity", "rainfall")

# 一层循环
for(ii in char_wr){
    dynamic_var1 <- paste0("stroke_pred1.", ii, "$matRRfit")
    dynamic_var2 <- paste0("stroke_pred1.", ii, "$matRRlow")
    dynamic_var3 <- paste0("stroke_pred1.", ii, "$matRRhigh")

    RRfit<-as.data.frame(t(apply(eval(parse(text = dynamic_var1)), 2, max)))
    RRl <- as.data.frame(t(apply(eval(parse(text = dynamic_var2)), 2, max)))
    RRh <- as.data.frame(t(apply(eval(parse(text = dynamic_var3)), 2, max)))
    RR_c<-rbind(RRfit,RRl,RRh)[, c("lag1","lag3","lag7", "lag14")]
    RR_ct<-as.data.frame(t(RR_c))
    RR_ct1<-cbind(ii,RR_ct)
    RR_all<-rbind(RR_all,RR_ct1)
}
RR_all$V1 <- round(RR_all$V1, 3)
RR_all$V2 <- round(RR_all$V2, 3)
RR_all$V3 <- round(RR_all$V3, 3)

RR_all$V4 <-  paste(RR_all$V1, "(", RR_all$V2, ",", RR_all$V3, ")", sep = "")

write_xlsx(RR_all,"F:/文章/小论文/出图/结果出图/RR矩阵_单变量.xlsx")





























####################################

#####################构建作图程序###########################

hemo_pred1.co = crosspred(hemo_cb.co, hemo_model_co, cen=round(median(hemo_all$co)), bylag=0.2) # 拟合模型计算 RR 以及 RR 的置信区间， cen 选取对照点作为 RR 分母，bylag 表示 lag 的切片单位。
hemo_pred1.pm2_5 = crosspred(hemo_cb.pm2_5, hemo_model_pm2_5, cen=round(median(hemo_all$pm2_5)), bylag=0.2)
hemo_pred1.pm10 = crosspred(hemo_cb.pm10, hemo_model_pm10, cen=round(median(hemo_all$pm10)), bylag=0.2) 
hemo_pred1.so2 = crosspred(hemo_cb.so2, hemo_model_so2, cen=round(median(hemo_all$so2)), bylag=0.2) 
hemo_pred1.no2 = crosspred(hemo_cb.no2, hemo_model_no2, cen=round(median(hemo_all$no2)), bylag=0.2) 
hemo_pred1.o3 = crosspred(hemo_cb.o3, hemo_model_o3, cen=round(median(hemo_all$o3)), bylag=0.2) 

infar_pred1.co = crosspred(infar_cb.co, infar_model_co, cen=round(median(infar_all$co)), bylag=0.2) # 拟合模型计算 RR 以及 RR 的置信区间， cen 选取对照点作为 RR 分母，bylag 表示 lag 的切片单位。
infar_pred1.pm2_5 = crosspred(infar_cb.pm2_5, infar_model_pm2_5, cen=round(median(infar_all$pm2_5)), bylag=0.2)
infar_pred1.pm10 = crosspred(infar_cb.pm10, infar_model_pm10, cen=round(median(infar_all$pm10)), bylag=0.2) 
infar_pred1.so2 = crosspred(infar_cb.so2, infar_model_so2, cen=round(median(infar_all$so2)), bylag=0.2) 
infar_pred1.no2 = crosspred(infar_cb.no2, infar_model_no2, cen=round(median(infar_all$no2)), bylag=0.2) 
infar_pred1.o3 = crosspred(infar_cb.o3, infar_model_o3, cen=round(median(infar_all$o3)), bylag=0.2)


hemo_pred1.temp_avg = crosspred(hemo_cb.temp_avg, hemo_model_temp_avg, cen=round(median(hemo_all$temp_avg)), bylag=0.2) # 拟合模型计算 RR 以及 RR 的置信区间， cen 选取对照点作为 RR 分母，bylag 表示 lag 的切片单位。
hemo_pred1.air_p_avg = crosspred(hemo_cb.air_p_avg, hemo_model_air_p_avg, cen=round(median(hemo_all$air_p_avg)), bylag=0.2)
hemo_pred1.eva_cap_b = crosspred(hemo_cb.eva_cap_b, hemo_model_eva_cap_b, cen=round(median(hemo_all$eva_cap_b)), bylag=0.2) 
hemo_pred1.humidity = crosspred(hemo_cb.humidity, hemo_model_humidity, cen=round(median(hemo_all$humidity)), bylag=0.2) 
hemo_pred1.rainfall = crosspred(hemo_cb.rainfall, hemo_model_rainfall, cen=round(median(hemo_all$rainfall)), bylag=0.2) 

infar_pred1.temp_avg = crosspred(infar_cb.temp_avg, infar_model_temp_avg, cen=round(median(infar_all$temp_avg)), bylag=0.2) # 拟合模型计算 RR 以及 RR 的置信区间， cen 选取对照点作为 RR 分母，bylag 表示 lag 的切片单位。
infar_pred1.air_p_avg = crosspred(infar_cb.air_p_avg, infar_model_air_p_avg, cen=round(median(infar_all$air_p_avg)), bylag=0.2)
infar_pred1.eva_cap_b = crosspred(infar_cb.eva_cap_b, infar_model_eva_cap_b, cen=round(median(infar_all$eva_cap_b)), bylag=0.2) 
infar_pred1.humidity = crosspred(infar_cb.humidity, infar_model_humidity, cen=round(median(infar_all$humidity)), bylag=0.2) 
infar_pred1.rainfall = crosspred(infar_cb.rainfall, infar_model_rainfall, cen=round(median(infar_all$rainfall)), bylag=0.2) 

##################################################









work_dir <- "F:/文章/小论文/出图/2025-1-5"
setwd(work_dir)

#####################三维图########################

##################################################

#脑卒中
png(file = "单变量模型_co_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.co,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_co",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_pm2_5_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm2_5,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm2.5",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_pm10_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm10,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm10",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_so2_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.so2,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_so2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_no2_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.no2,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_no2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_o3_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.o3,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_o3",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_temp_avg_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.temp_avg,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_temp_avg",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_air_p_avg_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.air_p_avg,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_air_p_avg",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_eva_cap_b_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.eva_cap_b,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_eva_cap_b",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_humidity_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.humidity,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_humidity",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_rainfall_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.rainfall,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_rainfall",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()









#出血性脑卒中
png(file = "单变量模型_co_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.co,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_co",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_pm2_5_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.pm2_5,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_pm2.5",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_pm10_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.pm10,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_pm10",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_so2_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.so2,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_so2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_no2_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.no2,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_no2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_o3_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.o3,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_o3",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_temp_avg_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.temp_avg,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_temp_avg",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_air_p_avg_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.air_p_avg,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_air_p_avg",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_eva_cap_b_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.eva_cap_b,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_eva_cap_b",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_humidity_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.humidity,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_humidity",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_rainfall_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.rainfall,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_rainfall",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()







#缺血性脑卒中
png(file = "单变量模型_co_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.co,ticktype='detailed',border='#3366FF',xlab="缺血性脑卒中Mean_co",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_pm2_5_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.pm2_5,ticktype='detailed',border='#3366FF',xlab="缺血性脑卒中Mean_pm2.5",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_pm10_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.pm10,ticktype='detailed',border='#3366FF',xlab="缺血性脑卒中Mean_pm10",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_so2_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.so2,ticktype='detailed',border='#3366FF',xlab="缺血性脑卒中Mean_so2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_no2_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.no2,ticktype='detailed',border='#3366FF',xlab="缺血性脑卒中Mean_no2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_o3_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.o3,ticktype='detailed',border='#3366FF',xlab="缺血性脑卒中Mean_o3",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_temp_avg_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.temp_avg,ticktype='detailed',border='#3366FF',xlab="缺血性脑卒中Mean_temp_avg",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_air_p_avg_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.air_p_avg,ticktype='detailed',border='#3366FF',xlab="缺血性脑卒中Mean_air_p_avg",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_eva_cap_b_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.eva_cap_b,ticktype='detailed',border='#3366FF',xlab="缺血性脑卒中Mean_eva_cap_b",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_humidity_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.humidity,ticktype='detailed',border='#3366FF',xlab="缺血性脑卒中Mean_humidity",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_rainfall_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.rainfall,ticktype='detailed',border='#3366FF',xlab="缺血性脑卒中Mean_rainfall",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()

################################

#####################二位切片图######################

####################################作图——————二维切片图#############################

#脑卒中
png(file = "二维切片图_co_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.co,"slices",col="purple",lag=1,
     xlab = "co", ylab = "RR", main = "脑卒中-co")
lines(stroke_pred1.co,col="green" ,lag=3)
lines(stroke_pred1.co,col="red" ,lag=7)
lines(stroke_pred1.co,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_pm2_5_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.pm2_5,"slices",col="purple",lag=1,
     xlab = "pm2_5", ylab = "RR", main = "脑卒中-pm2_5")
lines(stroke_pred1.pm2_5,col="green" ,lag=3)
lines(stroke_pred1.pm2_5,col="red" ,lag=7)
lines(stroke_pred1.pm2_5,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_pm10_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.pm10,"slices",col="purple",lag=1,
     xlab = "pm10", ylab = "RR", main = "脑卒中-pm10")
lines(stroke_pred1.pm10,col="green" ,lag=3)
lines(stroke_pred1.pm10,col="red" ,lag=7)
lines(stroke_pred1.pm10,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_so2_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.so2,"slices",col="purple",lag=1,
     xlab = "so2", ylab = "RR", main = "脑卒中-so2")
lines(stroke_pred1.so2,col="green" ,lag=3)
lines(stroke_pred1.so2,col="red" ,lag=7)
lines(stroke_pred1.so2,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_no2_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.no2,"slices",col="purple",lag=1,
     xlab = "no2", ylab = "RR", main = "脑卒中-no2")
lines(stroke_pred1.no2,col="green" ,lag=3)
lines(stroke_pred1.no2,col="red" ,lag=7)
lines(stroke_pred1.no2,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_o3_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.o3,"slices",col="purple",lag=1,
     xlab = "o3", ylab = "RR", main = "脑卒中-o3")
lines(stroke_pred1.o3,col="green" ,lag=3)
lines(stroke_pred1.o3,col="red" ,lag=7)
lines(stroke_pred1.o3,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_temp_avg_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.temp_avg,"slices",col="purple",lag=1,
     xlab = "temp_avg", ylab = "RR", main = "脑卒中-temp_avg")
lines(stroke_pred1.temp_avg,col="green" ,lag=3)
lines(stroke_pred1.temp_avg,col="red" ,lag=7)
lines(stroke_pred1.temp_avg,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_air_p_avg_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.air_p_avg,"slices",col="purple",lag=1,
     xlab = "air_p_avg", ylab = "RR", main = "脑卒中-air_p_avg")
lines(stroke_pred1.air_p_avg,col="green" ,lag=3)
lines(stroke_pred1.air_p_avg,col="red" ,lag=7)
lines(stroke_pred1.air_p_avg,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_eva_cap_b_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.eva_cap_b,"slices",col="purple",lag=1,
     xlab = "eva_cap_b", ylab = "RR", main = "脑卒中-eva_cap_b")
lines(stroke_pred1.eva_cap_b,col="green" ,lag=3)
lines(stroke_pred1.eva_cap_b,col="red" ,lag=7)
lines(stroke_pred1.eva_cap_b,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_humidity_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.humidity,"slices",col="purple",lag=1,
     xlab = "humidity", ylab = "RR", main = "脑卒中-humidity")
lines(stroke_pred1.humidity,col="green" ,lag=3)
lines(stroke_pred1.humidity,col="red" ,lag=7)
lines(stroke_pred1.humidity,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_rainfall_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.rainfall,"slices",col="purple",lag=1,
     xlab = "rainfall", ylab = "RR", main = "脑卒中-rainfall")
lines(stroke_pred1.rainfall,col="green" ,lag=3)
lines(stroke_pred1.rainfall,col="red" ,lag=7)
lines(stroke_pred1.rainfall,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()



















###############出血性脑卒中
png(file = "二维切片图_co_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.co,"slices",col="purple",lag=1,
     xlab = "co", ylab = "RR", main = "出血性脑卒中-co")
lines(hemo_pred1.co,col="green" ,lag=3)
lines(hemo_pred1.co,col="red" ,lag=7)
lines(hemo_pred1.co,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_pm2_5_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.pm2_5,"slices",col="purple",lag=1,
     xlab = "pm2_5", ylab = "RR", main = "出血性脑卒中-pm2_5")
lines(hemo_pred1.pm2_5,col="green" ,lag=3)
lines(hemo_pred1.pm2_5,col="red" ,lag=7)
lines(hemo_pred1.pm2_5,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_pm10_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.pm10,"slices",col="purple",lag=1,
     xlab = "pm10", ylab = "RR", main = "出血性脑卒中-pm10")
lines(hemo_pred1.pm10,col="green" ,lag=3)
lines(hemo_pred1.pm10,col="red" ,lag=7)
lines(hemo_pred1.pm10,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_so2_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.so2,"slices",col="purple",lag=1,
     xlab = "so2", ylab = "RR", main = "出血性脑卒中-so2")
lines(hemo_pred1.so2,col="green" ,lag=3)
lines(hemo_pred1.so2,col="red" ,lag=7)
lines(hemo_pred1.so2,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_no2_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.no2,"slices",col="purple",lag=1,
     xlab = "no2", ylab = "RR", main = "出血性脑卒中-no2")
lines(hemo_pred1.no2,col="green" ,lag=3)
lines(hemo_pred1.no2,col="red" ,lag=7)
lines(hemo_pred1.no2,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_o3_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.o3,"slices",col="purple",lag=1,
     xlab = "o3", ylab = "RR", main = "出血性脑卒中-o3")
lines(hemo_pred1.o3,col="green" ,lag=3)
lines(hemo_pred1.o3,col="red" ,lag=7)
lines(hemo_pred1.o3,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_temp_avg_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.temp_avg,"slices",col="purple",lag=1,
     xlab = "temp_avg", ylab = "RR", main = "出血性脑卒中-temp_avg")
lines(hemo_pred1.temp_avg,col="green" ,lag=3)
lines(hemo_pred1.temp_avg,col="red" ,lag=7)
lines(hemo_pred1.temp_avg,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_air_p_avg_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.air_p_avg,"slices",col="purple",lag=1,
     xlab = "air_p_avg", ylab = "RR", main = "出血性脑卒中-air_p_avg")
lines(hemo_pred1.air_p_avg,col="green" ,lag=3)
lines(hemo_pred1.air_p_avg,col="red" ,lag=7)
lines(hemo_pred1.air_p_avg,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_eva_cap_b_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.eva_cap_b,"slices",col="purple",lag=1,
     xlab = "eva_cap_b", ylab = "RR", main = "出血性脑卒中-eva_cap_b")
lines(hemo_pred1.eva_cap_b,col="green" ,lag=3)
lines(hemo_pred1.eva_cap_b,col="red" ,lag=7)
lines(hemo_pred1.eva_cap_b,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_humidity_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.humidity,"slices",col="purple",lag=1,
     xlab = "humidity", ylab = "RR", main = "出血性脑卒中-humidity")
lines(hemo_pred1.humidity,col="green" ,lag=3)
lines(hemo_pred1.humidity,col="red" ,lag=7)
lines(hemo_pred1.humidity,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_rainfall_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.rainfall,"slices",col="purple",lag=1,
     xlab = "rainfall", ylab = "RR", main = "出血性脑卒中-rainfall")
lines(hemo_pred1.rainfall,col="green" ,lag=3)
lines(hemo_pred1.rainfall,col="red" ,lag=7)
lines(hemo_pred1.rainfall,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()













###############缺血性脑卒中
png(file = "二维切片图_co_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.co,"slices",col="purple",lag=1,
     xlab = "co", ylab = "RR", main = "缺血性脑卒中-co")
lines(infar_pred1.co,col="green" ,lag=3)
lines(infar_pred1.co,col="red" ,lag=7)
lines(infar_pred1.co,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_pm2_5_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.pm2_5,"slices",col="purple",lag=1,
     xlab = "pm2_5", ylab = "RR", main = "缺血性脑卒中-pm2_5")
lines(infar_pred1.pm2_5,col="green" ,lag=3)
lines(infar_pred1.pm2_5,col="red" ,lag=7)
lines(infar_pred1.pm2_5,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_pm10_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.pm10,"slices",col="purple",lag=1,
     xlab = "pm10", ylab = "RR", main = "缺血性脑卒中-pm10")
lines(infar_pred1.pm10,col="green" ,lag=3)
lines(infar_pred1.pm10,col="red" ,lag=7)
lines(infar_pred1.pm10,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_so2_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.so2,"slices",col="purple",lag=1,
     xlab = "so2", ylab = "RR", main = "缺血性脑卒中-so2")
lines(infar_pred1.so2,col="green" ,lag=3)
lines(infar_pred1.so2,col="red" ,lag=7)
lines(infar_pred1.so2,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_no2_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.no2,"slices",col="purple",lag=1,
     xlab = "no2", ylab = "RR", main = "缺血性脑卒中-no2")
lines(infar_pred1.no2,col="green" ,lag=3)
lines(infar_pred1.no2,col="red" ,lag=7)
lines(infar_pred1.no2,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_o3_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.o3,"slices",col="purple",lag=1,
     xlab = "o3", ylab = "RR", main = "缺血性脑卒中-o3")
lines(infar_pred1.o3,col="green" ,lag=3)
lines(infar_pred1.o3,col="red" ,lag=7)
lines(infar_pred1.o3,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_temp_avg_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.temp_avg,"slices",col="purple",lag=1,
     xlab = "temp_avg", ylab = "RR", main = "缺血性脑卒中-temp_avg")
lines(infar_pred1.temp_avg,col="green" ,lag=3)
lines(infar_pred1.temp_avg,col="red" ,lag=7)
lines(infar_pred1.temp_avg,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_air_p_avg_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.air_p_avg,"slices",col="purple",lag=1,
     xlab = "air_p_avg", ylab = "RR", main = "缺血性脑卒中-air_p_avg")
lines(infar_pred1.air_p_avg,col="green" ,lag=3)
lines(infar_pred1.air_p_avg,col="red" ,lag=7)
lines(infar_pred1.air_p_avg,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_eva_cap_b_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.eva_cap_b,"slices",col="purple",lag=1,
     xlab = "eva_cap_b", ylab = "RR", main = "缺血性脑卒中-eva_cap_b")
lines(infar_pred1.eva_cap_b,col="green" ,lag=3)
lines(infar_pred1.eva_cap_b,col="red" ,lag=7)
lines(infar_pred1.eva_cap_b,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_humidity_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.humidity,"slices",col="purple",lag=1,
     xlab = "humidity", ylab = "RR", main = "缺血性脑卒中-humidity")
lines(infar_pred1.humidity,col="green" ,lag=3)
lines(infar_pred1.humidity,col="red" ,lag=7)
lines(infar_pred1.humidity,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_rainfall_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.rainfall,"slices",col="purple",lag=1,
     xlab = "rainfall", ylab = "RR", main = "缺血性脑卒中-rainfall")
lines(infar_pred1.rainfall,col="green" ,lag=3)
lines(infar_pred1.rainfall,col="red" ,lag=7)
lines(infar_pred1.rainfall,col="blue" ,lag=14)
legend("topright", 
       legend = c("Lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()


###########################累积效应图###############################
work_dir <- "F:/文章/小论文/出图/结果出图"
setwd(work_dir)
####################################################################

#脑卒中
png(file = "单变量-累积效应-co-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.co,stroke_model_co,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_co",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-co- 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.co,stroke_model_co,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_co",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-co- 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.co,stroke_model_co,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_co",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-co- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-no2-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.no2,stroke_model_no2,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-no2- 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2,stroke_model_no2,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-no2- 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2,stroke_model_no2,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-no2- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-o3-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.o3,stroke_model_o3,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-o3- 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.o3,stroke_model_o3,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-o3- 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.o3,stroke_model_o3,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-o3- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-pm2_5-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm2_5,stroke_model_pm2_5,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5,stroke_model_pm2_5,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5,stroke_model_pm2_5,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-pm10-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm10,stroke_model_pm10,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10,stroke_model_pm10,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10,stroke_model_pm10,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-so2-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.so2,stroke_model_so2,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-so2- 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.so2,stroke_model_so2,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-so2- 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.so2,stroke_model_so2,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-so2- 1-14 days",cex=0.89)
dev.off()


png(file = "单变量-累积效应-temp_avg-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,2))
crall <- crossreduce(stroke_cb.temp_avg,stroke_model_temp_avg,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_temp_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-temp_avg- 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.temp_avg,stroke_model_temp_avg,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_temp_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-temp_avg- 1-7 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-air_p_avg-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,2))
crall <- crossreduce(stroke_cb.air_p_avg,stroke_model_air_p_avg,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_air_p_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-air_p_avg- 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.air_p_avg,stroke_model_air_p_avg,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_air_p_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-air_p_avg- 1-7 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-eva_cap_b-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,2))
crall <- crossreduce(stroke_cb.eva_cap_b,stroke_model_eva_cap_b,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_eva_cap_b",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-eva_cap_b- 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.eva_cap_b,stroke_model_eva_cap_b,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_eva_cap_b",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-eva_cap_b- 1-7 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-humidity-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,2))
crall <- crossreduce(stroke_cb.humidity,stroke_model_humidity,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_humidity",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-humidity- 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.humidity,stroke_model_humidity,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_humidity",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-humidity- 1-7 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-rainfall-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,2))
crall <- crossreduce(stroke_cb.rainfall,stroke_model_rainfall,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_rainfall",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-rainfall- 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.rainfall,stroke_model_rainfall,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_rainfall",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-rainfall- 1-7 days",cex=0.89)
dev.off()











#出血性
png(file = "单变量-累积效应-co-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.co,hemo_model_co,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_co",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-co- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.co,hemo_model_co,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_co",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-co- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.co,hemo_model_co,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_co",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-co- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-no2-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.no2,hemo_model_no2,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-no2- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.no2,hemo_model_no2,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-no2- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.no2,hemo_model_no2,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-no2- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-o3-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.o3,hemo_model_o3,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-o3- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.o3,hemo_model_o3,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-o3- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.o3,hemo_model_o3,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-o3- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-pm2_5-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.pm2_5,hemo_model_pm2_5,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.pm2_5,hemo_model_pm2_5,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.pm2_5,hemo_model_pm2_5,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-pm10-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.pm10,hemo_model_pm10,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.pm10,hemo_model_pm10,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.pm10,hemo_model_pm10,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-so2-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.so2,hemo_model_so2,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-so2- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.so2,hemo_model_so2,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-so2- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.so2,hemo_model_so2,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-so2- 1-14 days",cex=0.89)
dev.off()


png(file = "单变量-累积效应-temp_avg-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.temp_avg,hemo_model_temp_avg,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_temp_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-temp_avg- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.temp_avg,hemo_model_temp_avg,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_temp_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-temp_avg- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.temp_avg,hemo_model_temp_avg,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_temp_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-temp_avg- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-air_p_avg-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.air_p_avg,hemo_model_air_p_avg,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_air_p_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-air_p_avg- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.air_p_avg,hemo_model_air_p_avg,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_air_p_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-air_p_avg- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.air_p_avg,hemo_model_air_p_avg,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_air_p_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-air_p_avg- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-eva_cap_b-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.eva_cap_b,hemo_model_eva_cap_b,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_eva_cap_b",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-eva_cap_b- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.eva_cap_b,hemo_model_eva_cap_b,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_eva_cap_b",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-eva_cap_b- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.eva_cap_b,hemo_model_eva_cap_b,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_eva_cap_b",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-eva_cap_b- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-humidity-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.humidity,hemo_model_humidity,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_humidity",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-humidity- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.humidity,hemo_model_humidity,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_humidity",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-humidity- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.humidity,hemo_model_humidity,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_humidity",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-humidity- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-rainfall-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.rainfall,hemo_model_rainfall,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_rainfall",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-rainfall- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.rainfall,hemo_model_rainfall,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_rainfall",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-rainfall- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.rainfall,hemo_model_rainfall,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_rainfall",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-rainfall- 1-14 days",cex=0.89)
dev.off()











###############################################################缺血性脑卒中

png(file = "单变量-累积效应-co-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.co,infar_model_co,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_co",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-co- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.co,infar_model_co,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_co",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-co- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.co,infar_model_co,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_co",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-co- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-no2-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.no2,infar_model_no2,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-no2- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.no2,infar_model_no2,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-no2- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.no2,infar_model_no2,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-no2- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-o3-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.o3,infar_model_o3,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-o3- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.o3,infar_model_o3,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-o3- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.o3,infar_model_o3,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-o3- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-pm2_5-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.pm2_5,infar_model_pm2_5,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.pm2_5,infar_model_pm2_5,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.pm2_5,infar_model_pm2_5,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-pm10-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.pm10,infar_model_pm10,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.pm10,infar_model_pm10,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.pm10,infar_model_pm10,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-so2-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.so2,infar_model_so2,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-so2- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.so2,infar_model_so2,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-so2- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.so2,infar_model_so2,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-so2- 1-14 days",cex=0.89)
dev.off()


png(file = "单变量-累积效应-temp_avg-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.temp_avg,infar_model_temp_avg,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_temp_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-temp_avg- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.temp_avg,infar_model_temp_avg,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_temp_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-temp_avg- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.temp_avg,infar_model_temp_avg,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_temp_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-temp_avg- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-air_p_avg-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.air_p_avg,infar_model_air_p_avg,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_air_p_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-air_p_avg- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.air_p_avg,infar_model_air_p_avg,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_air_p_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-air_p_avg- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.air_p_avg,infar_model_air_p_avg,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_air_p_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-air_p_avg- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-eva_cap_b-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.eva_cap_b,infar_model_eva_cap_b,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_eva_cap_b",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-eva_cap_b- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.eva_cap_b,infar_model_eva_cap_b,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_eva_cap_b",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-eva_cap_b- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.eva_cap_b,infar_model_eva_cap_b,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_eva_cap_b",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-eva_cap_b- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-humidity-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.humidity,infar_model_humidity,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_humidity",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-humidity- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.humidity,infar_model_humidity,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_humidity",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-humidity- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.humidity,infar_model_humidity,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_humidity",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-humidity- 1-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-rainfall-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.rainfall,infar_model_rainfall,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_rainfall",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-rainfall- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.rainfall,infar_model_rainfall,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_rainfall",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-rainfall- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.rainfall,infar_model_rainfall,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_rainfall",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-rainfall- 1-14 days",cex=0.89)
dev.off()


##########################################################




max(hemo_pred1.co$predvar)

max_rr_lag <- hemo_pred1.co$lag[which.max(hemo_pred1.co$predvar)]












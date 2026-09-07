library(haven)
library(readxl)
library(dlnm)
library(ggplot2)
library(splines) 
library(writexl)
library(dplyr)

options(scipen = 999)

stroke_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_stroke.xlsx")



#过拟合
stroke_cb.so2 = crossbasis(stroke_all$so2, lag=14, argvar=list(fun="ns", df=6), arglag=list(fun="ns", df=6))
stroke_cb.pm10 = crossbasis(stroke_all$pm10, lag=14, argvar=list(fun="ns", df=6), arglag=list(fun="ns", df=6))
stroke_cb.pm2_5 = crossbasis(stroke_all$pm2_5, lag=14, argvar=list(fun="ns", df=6), arglag=list(fun="ns", df=6))
stroke_cb.o3 = crossbasis(stroke_all$o3, lag=14, argvar=list(fun="ns", df=6), arglag=list(fun="ns", df=6))
stroke_cb.no2 = crossbasis(stroke_all$no2, lag=14, argvar=list(fun="ns", df=6), arglag=list(fun="ns", df=6))
stroke_cb.co = crossbasis(stroke_all$co, lag=14, argvar=list(fun="ns", df=6), arglag=list(fun="ns", df=6))

stroke_cb.temp_avg = crossbasis(stroke_all$temp_avg, lag=4, argvar=list(fun="ns", df=4), arglag=list(fun="ns", df=4))
stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=4, argvar=list(fun="ns", df=4), arglag=list(fun="ns", df=4))
stroke_cb.humidity = crossbasis(stroke_all$humidity, lag=4, argvar=list(fun="ns", df=3), arglag=list(fun="ns", df=3))
stroke_cb.eva_cap_b = crossbasis(stroke_all$eva_cap_b, lag=4, argvar=list(fun="ns", df=4), arglag=list(fun="ns", df=4))
stroke_cb.air_p_avg = crossbasis(stroke_all$air_p_avg, lag=4, argvar=list(fun="ns", df=4), arglag=list(fun="ns", df=4))





#Lag=14
stroke_cb.so2 = crossbasis(stroke_all$so2, lag=14, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=2))
stroke_cb.pm10 = crossbasis(stroke_all$pm10, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="poly", degree=4))
stroke_cb.pm2_5 = crossbasis(stroke_all$pm2_5, lag=14, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=5))
stroke_cb.o3 = crossbasis(stroke_all$o3, lag=14, argvar=list(fun="poly", degree=1), arglag=list(fun="ns", df=2))
stroke_cb.no2 = crossbasis(stroke_all$no2, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="ns", df=2))
stroke_cb.co = crossbasis(stroke_all$co, lag=14, argvar=list(fun="ns", df=2), arglag=list(fun="ns", df=5))

stroke_cb.temp_avg = crossbasis(stroke_all$temp_avg, lag=3, argvar=list(fun="poly", degree=1), arglag=list(fun="ns", df=4))
stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=3, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=2))
stroke_cb.humidity = crossbasis(stroke_all$humidity, lag=3, argvar=list(fun="ns", df=1), arglag=list(fun="ns", df=3))
stroke_cb.eva_cap_b = crossbasis(stroke_all$eva_cap_b, lag=3, argvar=list(fun="ns", df=2), arglag=list(fun="ns", df=2))
stroke_cb.air_p_avg = crossbasis(stroke_all$air_p_avg, lag=3, argvar=list(fun="ns", df=1), arglag=list(fun="poly", degree=3))






#模型构建
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







#######################输出单变量模型ANOVA值######################
# 初始化 anova2 空数据框
anova_all <- data.frame()
# 定义char_wr向量
char_wr <- c("co", "pm2_5", "pm10", "so2", "no2", "o3","temp_avg", "air_p_avg", "eva_cap_b", "humidity", "rainfall")
# 遍历char_wr中的每个元素
for(ii in char_wr){
  # 动态生成模型的名字
  dynamic_model_name <- paste0("stroke_model_", ii)
  # 使用get()函数获取实际的模型对象
  dynamic_model <- get(dynamic_model_name)
  # 计算anova
  anova_result <- anova(dynamic_model, test = "Chisq")
  p_value <- anova_result[2, 5]
  anova1 <- data.frame(Variable = ii, P_Value = p_value)
  anova_all <- rbind(anova_all, anova1)
}
write_xlsx(anova_all,"F:/文章/小论文/出图/2025-1-5/anova_all.xlsx")

############################################










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
RR_wr <- data.frame()
# 需要循环的变量集合
char_wr <- c("co", "pm2_5", "pm10", "so2", "no2", "o3")
for(ii in char_wr){
  dynamic_var1 <- paste0("stroke_pred1.", ii, "$matRRfit")
  dynamic_var2 <- paste0("stroke_pred1.", ii, "$matRRlow")
  dynamic_var3 <- paste0("stroke_pred1.", ii, "$matRRhigh")
  
  RRfit<-as.data.frame(t(apply(eval(parse(text = dynamic_var1)), 2, max)))
  RRl <- as.data.frame(t(apply(eval(parse(text = dynamic_var2)), 2, max)))
  RRh <- as.data.frame(t(apply(eval(parse(text = dynamic_var3)), 2, max)))
  RR_c<-rbind(RRfit,RRl,RRh)[, c("lag0","lag1","lag4", "lag7","lag8","lag10","lag14")]
  RR_ct<-as.data.frame(t(RR_c))
  RR_ct <- cbind(Variable = rownames(RR_ct), RR_ct)
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
for(ii in char_qx){
  dynamic_var1 <- paste0("stroke_pred1.", ii, "$matRRfit")
  dynamic_var2 <- paste0("stroke_pred1.", ii, "$matRRlow")
  dynamic_var3 <- paste0("stroke_pred1.", ii, "$matRRhigh")
  
  RRfit<-as.data.frame(t(apply(eval(parse(text = dynamic_var1)), 2, max)))
  RRl <- as.data.frame(t(apply(eval(parse(text = dynamic_var2)), 2, max)))
  RRh <- as.data.frame(t(apply(eval(parse(text = dynamic_var3)), 2, max)))
  RR_c<-rbind(RRfit,RRl,RRh)[, c("lag0","lag1","lag3")]
  RR_ct<-as.data.frame(t(RR_c))
  RR_ct <- cbind(Variable = rownames(RR_ct), RR_ct)
  RR_ct1<-cbind(ii,RR_ct)
  RR_qx<-rbind(RR_qx,RR_ct1)
}
RR_qx$V1 <- round(RR_qx$V1, 3)
RR_qx$V2 <- round(RR_qx$V2, 3)
RR_qx$V3 <- round(RR_qx$V3, 3)

RR_qx$V4 <-  paste(RR_qx$V1, "(", RR_qx$V2, ",", RR_qx$V3, ")", sep = "")


RR_all<-rbind(RR_qx,RR_wr)


write_xlsx(RR_all,"F:/文章/小论文/出图/2025-1-5/RR_all.xlsx")











########################出图################################
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












###########################累积效应图###############################
work_dir <- "F:/文章/小论文/出图/2025-1-5"
setwd(work_dir)
####################################################################
png(file = "单变量-累积效应-co-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.co,stroke_model_co,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_co",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-co- 0-1 days",cex=0.89)
crall <- crossreduce(stroke_cb.co,stroke_model_co,cen=20,type="overall",lag=c(0,7))
plot(crall,xlab="Mean_co",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-co- 0-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.co,stroke_model_co,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Mean_co",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-co- 0-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-no2-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.no2,stroke_model_no2,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-no2- 0-1 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2,stroke_model_no2,cen=20,type="overall",lag=c(0,7))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-no2- 0-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2,stroke_model_no2,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-no2- 0-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-o3-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.o3,stroke_model_o3,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-o3- 0-1 days",cex=0.89)
crall <- crossreduce(stroke_cb.o3,stroke_model_o3,cen=20,type="overall",lag=c(0,7))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-o3- 0-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.o3,stroke_model_o3,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-o3- 0-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-pm2_5-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm2_5,stroke_model_pm2_5,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 0-1 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5,stroke_model_pm2_5,cen=20,type="overall",lag=c(0,7))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 0-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5,stroke_model_pm2_5,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 0-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-pm10-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm10,stroke_model_pm10,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 0-1 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10,stroke_model_pm10,cen=20,type="overall",lag=c(0,7))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 0-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10,stroke_model_pm10,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 0-14 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-so2-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.so2,stroke_model_so2,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-so2- 0-1 days",cex=0.89)
crall <- crossreduce(stroke_cb.so2,stroke_model_so2,cen=20,type="overall",lag=c(0,7))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-so2- 0-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.so2,stroke_model_so2,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-so2- 0-14 days",cex=0.89)
dev.off()



#气象
png(file = "单变量-累积效应-temp_avg-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.temp_avg,stroke_model_temp_avg,cen=20,type="overall",lag=c(0,0))
plot(crall,xlab="Mean_temp_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-temp_avg- 0-1 days",cex=0.89)
crall <- crossreduce(stroke_cb.temp_avg,stroke_model_temp_avg,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_temp_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-temp_avg- 0-1 days",cex=0.89)
crall <- crossreduce(stroke_cb.temp_avg,stroke_model_temp_avg,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_temp_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-temp_avg- 0-3 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-air_p_avg-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,2))
crall <- crossreduce(stroke_cb.air_p_avg,stroke_model_air_p_avg,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_air_p_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-air_p_avg- 0-1 days",cex=0.89)
crall <- crossreduce(stroke_cb.air_p_avg,stroke_model_air_p_avg,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_air_p_avg",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-air_p_avg- 0-3 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-eva_cap_b-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,2))
crall <- crossreduce(stroke_cb.eva_cap_b,stroke_model_eva_cap_b,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_eva_cap_b",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-eva_cap_b- 0-1 days",cex=0.89)
crall <- crossreduce(stroke_cb.eva_cap_b,stroke_model_eva_cap_b,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_eva_cap_b",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-eva_cap_b- 0-3 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-humidity-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,2))
crall <- crossreduce(stroke_cb.humidity,stroke_model_humidity,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_humidity",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-humidity- 0-1 days",cex=0.89)
crall <- crossreduce(stroke_cb.humidity,stroke_model_humidity,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_humidity",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-humidity- 0-3 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-rainfall-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,2))
crall <- crossreduce(stroke_cb.rainfall,stroke_model_rainfall,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_rainfall",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-rainfall- 0-1 days",cex=0.89)
crall <- crossreduce(stroke_cb.rainfall,stroke_model_rainfall,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_rainfall",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-rainfall- 0-3 days",cex=0.89)
dev.off()
################################









########################二维切片图###################################
#脑卒中
png(file = "二维切片图_co_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.co,"slices",col="purple",lag=1,
     xlab = "co", ylab = "RR", main = "脑卒中-co")
lines(stroke_pred1.co,col="black" ,lag=0)
lines(stroke_pred1.co,col="purple" ,lag=1)
lines(stroke_pred1.co,col="green" ,lag=3)
lines(stroke_pred1.co,col="red" ,lag=7)
lines(stroke_pred1.co,col="blue" ,lag=14)
legend("topright", 
       legend = c("lag = 0","lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("black", "purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_pm2_5_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.pm2_5,"slices",col="purple",lag=1,
     xlab = "pm2_5", ylab = "RR", main = "脑卒中-pm2_5")
lines(stroke_pred1.pm2_5,col="black" ,lag=0)
lines(stroke_pred1.pm2_5,col="purple" ,lag=1)
lines(stroke_pred1.pm2_5,col="green" ,lag=3)
lines(stroke_pred1.pm2_5,col="red" ,lag=7)
lines(stroke_pred1.pm2_5,col="blue" ,lag=14)
legend("topright", 
       legend = c("lag = 0","lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("black", "purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_pm10_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.pm10,"slices",col="purple",lag=1,
     xlab = "pm10", ylab = "RR", main = "脑卒中-pm10")
lines(stroke_pred1.pm10,col="black" ,lag=0)
lines(stroke_pred1.pm10,col="purple" ,lag=1)
lines(stroke_pred1.pm10,col="green" ,lag=3)
lines(stroke_pred1.pm10,col="red" ,lag=7)
lines(stroke_pred1.pm10,col="blue" ,lag=14)
legend("topright", 
       legend = c("lag = 0","lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("black", "purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_so2_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.so2,"slices",col="purple",lag=1,
     xlab = "so2", ylab = "RR", main = "脑卒中-so2")
lines(stroke_pred1.so2,col="black" ,lag=0)
lines(stroke_pred1.so2,col="purple" ,lag=1)
lines(stroke_pred1.so2,col="green" ,lag=3)
lines(stroke_pred1.so2,col="red" ,lag=7)
lines(stroke_pred1.so2,col="blue" ,lag=14)
legend("topright", 
       legend = c("lag = 0","lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("black", "purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_no2_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.no2,"slices",col="purple",lag=1,
     xlab = "no2", ylab = "RR", main = "脑卒中-no2")
lines(stroke_pred1.no2,col="black" ,lag=0)
lines(stroke_pred1.no2,col="purple" ,lag=1)
lines(stroke_pred1.no2,col="green" ,lag=3)
lines(stroke_pred1.no2,col="red" ,lag=7)
lines(stroke_pred1.no2,col="blue" ,lag=14)
legend("topright", 
       legend = c("lag = 0","lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("black", "purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_o3_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.o3,"slices",col="purple",lag=1,
     xlab = "o3", ylab = "RR", main = "脑卒中-o3")
lines(stroke_pred1.o3,col="black" ,lag=0)
lines(stroke_pred1.o3,col="purple" ,lag=1)
lines(stroke_pred1.o3,col="green" ,lag=3)
lines(stroke_pred1.o3,col="red" ,lag=7)
lines(stroke_pred1.o3,col="blue" ,lag=14)
legend("topright", 
       legend = c("lag = 0","lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("black", "purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

#########气象数据

png(file = "二维切片图_temp_avg_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.temp_avg,"slices",col="purple",lag=1,
     xlab = "temp_avg", ylab = "RR", main = "脑卒中-temp_avg")
lines(stroke_pred1.temp_avg,col="green" ,lag=0)
lines(stroke_pred1.temp_avg,col="red" ,lag=1)
lines(stroke_pred1.temp_avg,col="blue" ,lag=3)
legend("topright", 
       legend = c("lag = 0", "lag = 1", "lag = 3"),
       col = c( "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_air_p_avg_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.air_p_avg,"slices",col="purple",lag=1,
     xlab = "air_p_avg", ylab = "RR", main = "脑卒中-air_p_avg")
lines(stroke_pred1.air_p_avg,col="green" ,lag=0)
lines(stroke_pred1.air_p_avg,col="red" ,lag=1)
lines(stroke_pred1.air_p_avg,col="blue" ,lag=3)
legend("topright", 
       legend = c("lag = 0", "lag = 1", "lag = 3"),
       col = c( "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_eva_cap_b_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.eva_cap_b,"slices",col="purple",lag=1,
     xlab = "eva_cap_b", ylab = "RR", main = "脑卒中-eva_cap_b")
lines(stroke_pred1.eva_cap_b,col="green" ,lag=0)
lines(stroke_pred1.eva_cap_b,col="red" ,lag=1)
lines(stroke_pred1.eva_cap_b,col="blue" ,lag=3)
legend("topright", 
       legend = c("lag = 0", "lag = 1", "lag = 3"),
       col = c( "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_humidity_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.humidity,"slices",col="purple",lag=1,
     xlab = "humidity", ylab = "RR", main = "脑卒中-humidity")
lines(stroke_pred1.humidity,col="green" ,lag=0)
lines(stroke_pred1.humidity,col="red" ,lag=1)
lines(stroke_pred1.humidity,col="blue" ,lag=3)
legend("topright", 
       legend = c("lag = 0", "lag = 1", "lag = 3"),
       col = c( "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_rainfall_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.rainfall,"slices",col="purple",lag=1,
     xlab = "rainfall", ylab = "RR", main = "脑卒中-rainfall")
lines(stroke_pred1.rainfall,col="green" ,lag=0)
lines(stroke_pred1.rainfall,col="red" ,lag=1)
lines(stroke_pred1.rainfall,col="blue" ,lag=3)
legend("topright", 
       legend = c("lag = 0", "lag = 1", "lag = 3"),
       col = c( "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()
##########################################################











###############################
#微调尝试
stroke_cb.pm10 = crossbasis(stroke_all$pm10, lag=14, argvar=list(fun="ns", df=5), arglag=list(fun="ns", df=5))
stroke_cb.pm2_5 = crossbasis(stroke_all$pm2_5, lag=14, argvar=list(fun="ns", df=5), arglag=list(fun="ns", df=5))

stroke_model_pm2_5 = glm(counts ~ stroke_cb.pm2_5 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm10 = glm(counts ~ stroke_cb.pm10 + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 


stroke_pred1.pm2_5 = crosspred(stroke_cb.pm2_5, stroke_model_pm2_5, cen=round(median(stroke_all$pm2_5)), bylag=0.2)
stroke_pred1.pm10 = crosspred(stroke_cb.pm10, stroke_model_pm10, cen=round(median(stroke_all$pm10)), bylag=0.2) 



RR_wr <- data.frame()
# 需要循环的变量集合
char_wr <- c("pm2_5", "pm10")
for(ii in char_wr){
  dynamic_var1 <- paste0("stroke_pred1.", ii, "$matRRfit")
  dynamic_var2 <- paste0("stroke_pred1.", ii, "$matRRlow")
  dynamic_var3 <- paste0("stroke_pred1.", ii, "$matRRhigh")
  
  RRfit<-as.data.frame(t(apply(eval(parse(text = dynamic_var1)), 2, max)))
  RRl <- as.data.frame(t(apply(eval(parse(text = dynamic_var2)), 2, max)))
  RRh <- as.data.frame(t(apply(eval(parse(text = dynamic_var3)), 2, max)))
  RR_c<-rbind(RRfit,RRl,RRh)[, c("lag0","lag1","lag4", "lag7","lag8","lag10","lag14")]
  RR_ct<-as.data.frame(t(RR_c))
  RR_ct <- cbind(Variable = rownames(RR_ct), RR_ct)
  RR_ct1<-cbind(ii,RR_ct)
  RR_wr<-rbind(RR_wr,RR_ct1)
}
RR_wr$V1 <- round(RR_wr$V1, 3)
RR_wr$V2 <- round(RR_wr$V2, 3)
RR_wr$V3 <- round(RR_wr$V3, 3)

RR_wr$V4 <-  paste(RR_wr$V1, "(", RR_wr$V2, ",", RR_wr$V3, ")", sep = "")




png(file = "单变量模型_pm2_5_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm2_5,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm2.5",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "单变量模型_pm10_三维_脑卒中.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm10,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm10",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()





png(file = "单变量-累积效应-pm2_5-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm2_5,stroke_model_pm2_5,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 0-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5,stroke_model_pm2_5,cen=20,type="overall",lag=c(0,5))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 0-5 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5,stroke_model_pm2_5,cen=20,type="overall",lag=c(0,7))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm2_5- 0-7 days",cex=0.89)
dev.off()

png(file = "单变量-累积效应-pm10-脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm10,stroke_model_pm10,cen=20,type="overall",lag=c(0,10))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 0-10 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10,stroke_model_pm10,cen=20,type="overall",lag=c(0,20))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 0-20 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10,stroke_model_pm10,cen=20,type="overall",lag=c(0,30))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="单变量-累积效应-pm10- 0-30 days",cex=0.89)
dev.off()








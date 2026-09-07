
library(haven)
library(readxl)
library(dlnm)
library(ggplot2)
library(splines) 
library(writexl)
library(dplyr)


stroke_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_stroke.xlsx")
stroke_all$air_p_avg<- eval(stroke_all$air_p_avg-990)


#向量基
stroke_cb.pm10 = crossbasis(stroke_all$pm10, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="poly", degree=4))
stroke_cb.pm2_5 = crossbasis(stroke_all$pm2_5, lag=14, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=5))
stroke_cb.no2 = crossbasis(stroke_all$no2, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="ns", df=2))


stroke_cb.temp_avg = crossbasis(stroke_all$temp_avg, lag=14, argvar=list(fun="poly", degree=1), arglag=list(fun="ns", df=4))
stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=14, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=2))
stroke_cb.humidity = crossbasis(stroke_all$humidity, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="ns", df=3))
stroke_cb.eva_cap_b = crossbasis(stroke_all$eva_cap_b, lag=14, argvar=list(fun="ns", df=2), arglag=list(fun="ns", df=2))
stroke_cb.air_p_avg = crossbasis(stroke_all$air_p_avg, lag=14, argvar=list(fun="ns", df=1), arglag=list(fun="poly", degree=3))




###################双变量模型##################
#pm2.5
stroke_model_pm2_5_temp_avg = glm(counts ~ stroke_cb.pm2_5 + stroke_cb.temp_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm2_5_rainfall = glm(counts ~ stroke_cb.pm2_5 + stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm2_5_humidity = glm(counts ~ stroke_cb.pm2_5 + stroke_cb.humidity + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm2_5_eva_cap_b = glm(counts ~ stroke_cb.pm2_5 + stroke_cb.eva_cap_b + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm2_5_air_p_avg = glm(counts ~ stroke_cb.pm2_5 + stroke_cb.air_p_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 


#pm10
stroke_model_pm10_temp_avg = glm(counts ~ stroke_cb.pm10 + stroke_cb.temp_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm10_rainfall = glm(counts ~ stroke_cb.pm10 + stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm10_humidity = glm(counts ~ stroke_cb.pm10 + stroke_cb.humidity + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm10_eva_cap_b = glm(counts ~ stroke_cb.pm10 + stroke_cb.eva_cap_b + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm10_air_p_avg = glm(counts ~ stroke_cb.pm10 + stroke_cb.air_p_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 


#NO2
stroke_model_no2_temp_avg = glm(counts ~ stroke_cb.no2 + stroke_cb.temp_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_no2_rainfall = glm(counts ~ stroke_cb.no2 + stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_no2_humidity = glm(counts ~ stroke_cb.no2 + stroke_cb.humidity + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_no2_eva_cap_b = glm(counts ~ stroke_cb.no2 + stroke_cb.eva_cap_b + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_no2_air_p_avg = glm(counts ~ stroke_cb.no2 + stroke_cb.air_p_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
############################################









######################################双变量模型作图程序####################

#######################pm2.5
#pm2.5-temp
stroke_pred1.pm2_5_pm2_5temp_avg = crosspred(stroke_cb.pm2_5, stroke_model_pm2_5_temp_avg, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.temp_avg_pm2_5temp_avg = crosspred(stroke_cb.temp_avg, stroke_model_pm2_5_temp_avg, cen=round(median(stroke_all$co)), bylag=1) 

#pm2.5-airp
stroke_pred1.pm2_5_pm2_5air_p_avg = crosspred(stroke_cb.pm2_5, stroke_model_pm2_5_air_p_avg, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.air_p_avg_pm2_5air_p_avg = crosspred(stroke_cb.air_p_avg, stroke_model_pm2_5_air_p_avg, cen=round(median(stroke_all$co)), bylag=1) 

#pm2.5-eva
stroke_pred1.pm2_5_pm2_5eva_cap_b = crosspred(stroke_cb.pm2_5, stroke_model_pm2_5_eva_cap_b, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.eva_cap_b_pm2_5eva_cap_b = crosspred(stroke_cb.eva_cap_b, stroke_model_pm2_5_eva_cap_b, cen=round(median(stroke_all$co)), bylag=1) 

#pm2.5-humidity
stroke_pred1.pm2_5_pm2_5humidity = crosspred(stroke_cb.pm2_5, stroke_model_pm2_5_humidity, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.humidity_pm2_5humidity = crosspred(stroke_cb.humidity, stroke_model_pm2_5_humidity, cen=round(median(stroke_all$co)), bylag=1) 

#pm2.5-rainfall
stroke_pred1.pm2_5_pm2_5rainfall = crosspred(stroke_cb.pm2_5, stroke_model_pm2_5_rainfall, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.rainfall_pm2_5rainfall = crosspred(stroke_cb.rainfall, stroke_model_pm2_5_rainfall, cen=round(median(stroke_all$co)), bylag=1) 

#######################pm10
#pm10-temp
stroke_pred1.pm10_pm10temp_avg = crosspred(stroke_cb.pm10, stroke_model_pm10_temp_avg, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.temp_avg_pm10temp_avg = crosspred(stroke_cb.temp_avg, stroke_model_pm10_temp_avg, cen=round(median(stroke_all$co)), bylag=1) 

#pm10-airp
stroke_pred1.pm10_pm10air_p_avg = crosspred(stroke_cb.pm10, stroke_model_pm10_air_p_avg, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.air_p_avg_pm10air_p_avg = crosspred(stroke_cb.air_p_avg, stroke_model_pm10_air_p_avg, cen=round(median(stroke_all$co)), bylag=1) 

#pm10-eva
stroke_pred1.pm10_pm10eva_cap_b = crosspred(stroke_cb.pm10, stroke_model_pm10_eva_cap_b, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.eva_cap_b_pm10eva_cap_b = crosspred(stroke_cb.eva_cap_b, stroke_model_pm10_eva_cap_b, cen=round(median(stroke_all$co)), bylag=1) 

#pm10-humidity
stroke_pred1.pm10_pm10humidity = crosspred(stroke_cb.pm10, stroke_model_pm10_humidity, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.humidity_pm10humidity = crosspred(stroke_cb.humidity, stroke_model_pm10_humidity, cen=round(median(stroke_all$co)), bylag=1) 

#pm10-rainfall
stroke_pred1.pm10_pm10rainfall = crosspred(stroke_cb.pm10, stroke_model_pm10_rainfall, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.rainfall_pm10rainfall = crosspred(stroke_cb.rainfall, stroke_model_pm10_rainfall, cen=round(median(stroke_all$co)), bylag=1) 

#######################no2
#no2-temp
stroke_pred1.no2_no2temp_avg = crosspred(stroke_cb.no2, stroke_model_no2_temp_avg, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.temp_avg_no2temp_avg = crosspred(stroke_cb.temp_avg, stroke_model_no2_temp_avg, cen=round(median(stroke_all$co)), bylag=1) 

#no2-airp
stroke_pred1.no2_no2air_p_avg = crosspred(stroke_cb.no2, stroke_model_no2_air_p_avg, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.air_p_avg_no2air_p_avg = crosspred(stroke_cb.air_p_avg, stroke_model_no2_air_p_avg, cen=round(median(stroke_all$co)), bylag=1) 

#no2-eva
stroke_pred1.no2_no2eva_cap_b = crosspred(stroke_cb.no2, stroke_model_no2_eva_cap_b, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.eva_cap_b_no2eva_cap_b = crosspred(stroke_cb.eva_cap_b, stroke_model_no2_eva_cap_b, cen=round(median(stroke_all$co)), bylag=1) 

#no2-humidity
stroke_pred1.no2_no2humidity = crosspred(stroke_cb.no2, stroke_model_no2_humidity, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.humidity_no2humidity = crosspred(stroke_cb.humidity, stroke_model_no2_humidity, cen=round(median(stroke_all$co)), bylag=1) 

#no2-rainfall
stroke_pred1.no2_no2rainfall = crosspred(stroke_cb.no2, stroke_model_no2_rainfall, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.rainfall_no2rainfall = crosspred(stroke_cb.rainfall, stroke_model_no2_rainfall, cen=round(median(stroke_all$co)), bylag=1) 














##################输出RR##############################
 


#####尝试#不要执行#
stroke_pred1.pm2_5_pm2_5temp_avg$matRRfit



RRfitmax_pm2_5_pm2_5tempavg <- as.data.frame(t(apply(stroke_pred1.pm2_5_pm2_5temp_avg$matRRfit, 2, max)))
RRlmax_pm2_5_pm2_5tempavg <- as.data.frame(t(apply(stroke_pred1.pm2_5_pm2_5temp_avg$matRRlow, 2, max)))
RRhmax_pm2_5_pm2_5tempavg <- as.data.frame(t(apply(stroke_pred1.pm2_5_pm2_5temp_avg$matRRhigh, 2, max)))

RR_pm2_5_pm2_5tempavg<-rbind(RRfitmax_pm2_5_pm2_5tempavg,RRlmax_pm2_5_pm2_5tempavg,RRhmax_pm2_5_pm2_5tempavg)[, c("lag7", "lag14")]



##############################################
RR_all<-data.frame()

#建立循环
char_wr <- c("pm2_5", "pm10", "no2")
char_qx <- c("temp_avg","air_p_avg","eva_cap_b","humidity","rainfall")

#一层循环 ii 指双变量模型变量1
for(ii in char_wr){
  #二层循环 jj 指双变量模型变量2
  for(jj in char_qx){
    #三层循环 kk指对谁建模
    for(kk in c(ii,jj)){
      dynamic_var1 <- paste0("stroke_pred1.", kk, "_", ii, jj, "$matRRfit")
      dynamic_var2 <- paste0("stroke_pred1.", kk, "_", ii, jj, "$matRRlow")
      dynamic_var3 <- paste0("stroke_pred1.", kk, "_", ii, jj, "$matRRhigh")
      
      RRfit<-as.data.frame(t(apply(eval(parse(text = dynamic_var1)), 2, max)))
      RRl <- as.data.frame(t(apply(eval(parse(text = dynamic_var2)), 2, max)))
      RRh <- as.data.frame(t(apply(eval(parse(text = dynamic_var3)), 2, max)))
      RR_c<-rbind(RRfit,RRl,RRh)[, c("lag7", "lag14")]
      RR_ct<-as.data.frame(t(RR_c))
      
      
      RR_ct1<-cbind(kk,ii,jj,RR_ct)
      RR_all<-rbind(RR_all,RR_ct1)
    }
  }
}
RR_all$V1 <- round(RR_all$V1, 3)
RR_all$V2 <- round(RR_all$V2, 3)
RR_all$V3 <- round(RR_all$V3, 3)

RR_all$V4 <-  paste(RR_all$V1, "(", RR_all$V2, ",", RR_all$V3, ")", sep = "")
write_xlsx(RR_all,"F:/文章/小论文/出图/结果出图/RR矩阵_双变量.xlsx")












###############查看异常数值  不要执行#########################
dynamic_var1 <- paste0("stroke_pred1.", "air_p_avg", "_", "no2", "air_p_avg", "$matRRfit")
dynamic_var1 <- paste0("stroke_pred1.", "air_p_avg", "_", "no2", "air_p_avg", "$matRRlow")
dynamic_var1 <- paste0("stroke_pred1.", "air_p_avg", "_", "no2", "air_p_avg", "$matRRhigh")

RRfit<-as.data.frame(t(apply(eval(parse(text = dynamic_var1)), 2, max)))
RRl <- as.data.frame(t(apply(eval(parse(text = dynamic_var2)), 2, max)))
RRh <- as.data.frame(t(apply(eval(parse(text = dynamic_var3)), 2, max)))

stroke_pred1.air_p_avg_no2air_p_avg$matRRfit


plot(stroke_pred1.air_p_avg_no2air_p_avg,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_no2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)

#########################











##################三维图##################
work_dir <- "F:/文章/小论文/出图/结果出图"
setwd(work_dir)

#pm2_5
png(file = "双变量模型_pm2_5_temp_avg_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm2_5_pm2_5temp_avg,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm2_5",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "双变量模型_pm2_5_air_p_avg_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm2_5_pm2_5air_p_avg,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm2_5",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "双变量模型_pm2_5_eva_cap_b_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm2_5_pm2_5eva_cap_b,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm2_5",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "双变量模型_pm2_5_humidity_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm2_5_pm2_5humidity,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm2_5",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "双变量模型_pm2_5_rainfall_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm2_5_pm2_5rainfall,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm2_5",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()


#pm10
png(file = "双变量模型_pm10_temp_avg_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm10_pm10temp_avg,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm10",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "双变量模型_pm10_air_p_avg_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm10_pm10air_p_avg,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm10",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "双变量模型_pm10_eva_cap_b_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm10_pm10eva_cap_b,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm10",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "双变量模型_pm10_humidity_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm10_pm10humidity,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm10",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "双变量模型_pm10_rainfall_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.pm10_pm10rainfall,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm10",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()

#no2
png(file = "双变量模型_no2_temp_avg_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.no2_no2temp_avg,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_no2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "双变量模型_no2_air_p_avg_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.no2_no2air_p_avg,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_no2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "双变量模型_no2_eva_cap_b_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.no2_no2eva_cap_b,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_no2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "双变量模型_no2_humidity_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.no2_no2humidity,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_no2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "双变量模型_no2_rainfall_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.no2_no2rainfall,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_no2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()










############累积效应图##############
work_dir <- "F:/文章/小论文/出图/双变量模型_累积效应"
setwd(work_dir)

#pm2_5
#temp
png(file = "双变量pm2_5_temp累积效应pm2_5.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_temp_avg,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_temp 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_temp_avg,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_temp 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_temp_avg,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_temp 1-14 days",cex=0.89)
dev.off()

#air_p
png(file = "双变量pm2_5_air_p_avg累积效应pm2_5.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_air_p_avg,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_air_p_avg 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_air_p_avg,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_air_p_avg 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_air_p_avg,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_air_p_avg 1-14 days",cex=0.89)
dev.off()

#eva
png(file = "双变量pm2_5_eva_cap_b累积效应pm2_5.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_eva_cap_b,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_eva_cap_b 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_eva_cap_b,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_eva_cap_b 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_eva_cap_b,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_eva_cap_b 1-14 days",cex=0.89)
dev.off()



#humidity
png(file = "双变量pm2_5_humidity累积效应pm2_5.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_humidity,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_humidity 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_humidity,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_humidity 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_humidity,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_humidity 1-14 days",cex=0.89)
dev.off()


#rainfall
png(file = "双变量pm2_5_rainfall累积效应pm2_5.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_rainfall,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_rainfall 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_rainfall,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_rainfall 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_rainfall,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_rainfall 1-14 days",cex=0.89)
dev.off()







#pm10
#temp
png(file = "双变量pm10_temp累积效应pm10.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_temp_avg,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_temp 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_temp_avg,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_temp 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_temp_avg,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_temp 1-14 days",cex=0.89)
dev.off()

#air_p
png(file = "双变量pm10_air_p_avg累积效应pm10.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_air_p_avg,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_air_p_avg 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_air_p_avg,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_air_p_avg 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_air_p_avg,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_air_p_avg 1-14 days",cex=0.89)
dev.off()

#eva
png(file = "双变量pm10_eva_cap_b累积效应pm10.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_eva_cap_b,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_eva_cap_b 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_eva_cap_b,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_eva_cap_b 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_eva_cap_b,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_eva_cap_b 1-14 days",cex=0.89)
dev.off()



#humidity
png(file = "双变量pm10_humidity累积效应pm10.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_humidity,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_humidity 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_humidity,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_humidity 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_humidity,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_humidity 1-14 days",cex=0.89)
dev.off()


#rainfall
png(file = "双变量pm10_rainfall累积效应pm10.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_rainfall,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_rainfall 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_rainfall,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_rainfall 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_rainfall,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_rainfall 1-14 days",cex=0.89)
dev.off()






#no2
#temp
png(file = "双变量no2_temp累积效应no2.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_temp_avg,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_temp 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_temp_avg,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_temp 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_temp_avg,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_temp 1-14 days",cex=0.89)
dev.off()

#air_p
png(file = "双变量no2_air_p_avg累积效应no2.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_air_p_avg,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_air_p_avg 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_air_p_avg,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_air_p_avg 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_air_p_avg,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_air_p_avg 1-14 days",cex=0.89)
dev.off()

#eva
png(file = "双变量no2_eva_cap_b累积效应no2.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_eva_cap_b,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_eva_cap_b 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_eva_cap_b,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_eva_cap_b 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_eva_cap_b,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_eva_cap_b 1-14 days",cex=0.89)
dev.off()



#humidity
png(file = "双变量no2_humidity累积效应no2.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_humidity,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_humidity 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_humidity,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_humidity 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_humidity,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_humidity 1-14 days",cex=0.89)
dev.off()


#rainfall
png(file = "双变量no2_rainfall累积效应no2.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_rainfall,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_rainfall 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_rainfall,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_rainfall 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_rainfall,cen=20,type="overall",lag=c(1,14))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_rainfall 1-14 days",cex=0.89)
dev.off()


















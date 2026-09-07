

library(haven)
library(readxl)
library(dlnm)
library(ggplot2)
library(splines) 
library(writexl)




#汇总


hemo_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_hemo.xlsx") #门诊数据总
infar_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_infar.xlsx")




#########################空气污染物#########################
#aqi  pm2.5  pm10  so2  co  no2  o3
#共7个


hemo_cb.aqi = crossbasis(hemo_all$aqi, lag=30, argvar=list(fun="poly", degree=1),arglag=list(fun="poly",degree=1))
infar_cb.aqi = crossbasis(infar_all$aqi, lag=30, argvar=list(fun="ns", df=2), arglag=list(fun="poly", degree=1))


hemo_cb.pm2_5 = crossbasis(hemo_all$pm2_5, lag=30, argvar=list(fun="poly", degree=3),arglag=list(fun="ns",df=4))
infar_cb.pm2_5 = crossbasis(infar_all$pm2_5, lag=30, argvar=list(fun="poly", degree=1), arglag=list(fun="poly", degree=2))


hemo_cb.pm10 = crossbasis(hemo_all$pm10, lag=30, argvar=list(fun="ns", df=2),arglag=list(fun="ns",df=4))
infar_cb.pm10 = crossbasis(infar_all$pm10, lag=30, argvar=list(fun="poly", degree=1), arglag=list(fun="poly", degree=1))


hemo_cb.so2 = crossbasis(hemo_all$so2, lag=30, argvar=list(fun="poly", degree=1),arglag=list(fun="poly",degree=1))
infar_cb.so2 = crossbasis(infar_all$so2, lag=30, argvar=list(fun="poly", degree=1), arglag=list(fun="poly", degree=1))


hemo_cb.co = crossbasis(hemo_all$co, lag=30, argvar=list(fun="poly", degree=1),arglag=list(fun="poly",degree=1))
infar_cb.co = crossbasis(infar_all$co, lag=30, argvar=list(fun="ns", df=1), arglag=list(fun="poly", degree=5))


hemo_cb.no2 = crossbasis(hemo_all$no2, lag=30, argvar=list(fun="ns", df=2),arglag=list(fun="poly",degree=1))
infar_cb.no2 = crossbasis(infar_all$no2, lag=30, argvar=list(fun="poly", degree=1), arglag=list(fun="poly", degree=5))


hemo_cb.o3 = crossbasis(hemo_all$o3, lag=30, argvar=list(fun="poly", degree=1),arglag=list(fun="poly",degree=1))
infar_cb.o3 = crossbasis(infar_all$o3, lag=30, argvar=list(fun="ns", df=2), arglag=list(fun="poly", degree=1))



#构建全因素模型
hemo_model_all = glm(counts ~ 
                       hemo_cb.aqi + hemo_cb.pm2_5 + hemo_cb.pm10 + hemo_cb.so2 + hemo_cb.no2 + hemo_cb.o3 + ns(date,14*7) + 
                       ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday
                       ,
                     family=quasipoisson(), hemo_all) 
#####融入所有空气污染变量 + 日期节段(周期) + 关键气象数据的节段函数 + 周末 + 节假日

infar_model_all = glm(counts ~ 
                        infar_cb.aqi + infar_cb.pm2_5 + infar_cb.pm10 + infar_cb.so2+ + infar_cb.no2 + infar_cb.o3 + ns(date,14*7)+
                        ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday
                      ,
                      family=quasipoisson(), infar_all) # 拟合类 poission 模型


##################################################



#########################气象数据#########################
#rainfall    eva_cap_d  temp_avg  tamp_l  tamp_h  air_p_avg  air_p_water  air_p_l  air_p_h  humidity  humudity_l  w_speed  max_w_speed  peek_w_speed  sunshine
#共15个


hemo_cb.eva_cap_b = crossbasis(hemo_all$eva_cap_b, lag=30, argvar=list(fun="ns", df=2),arglag=list(fun="poly",degree=3))
infar_cb.eva_cap_b = crossbasis(infar_all$eva_cap_b, lag=30, argvar=list(fun="bs", df=4), arglag=list(fun="poly", degree=1))


hemo_cb.temp_avg = crossbasis(hemo_all$temp_avg, lag=30, argvar=list(fun="poly", degree=1),arglag=list(fun="poly",degree=2))
infar_cb.temp_avg = crossbasis(infar_all$temp_avg, lag=30, argvar=list(fun="poly", degree=3), arglag=list(fun="poly", degree=1))


hemo_cb.temp_l = crossbasis(hemo_all$temp_l, lag=30, argvar=list(fun="poly", degree=1),arglag=list(fun="ns",df=3))
infar_cb.temp_l = crossbasis(infar_all$temp_l, lag=30, argvar=list(fun="poly", degree=3), arglag=list(fun="poly", degree=1))


hemo_cb.temp_h = crossbasis(hemo_all$temp_h, lag=30, argvar=list(fun="poly", degree=1),arglag=list(fun="poly",degree=2))
infar_cb.temp_h = crossbasis(infar_all$temp_h, lag=30, argvar=list(fun="ns", df=3), arglag=list(fun="poly", degree=1))


hemo_cb.air_p_avg = crossbasis(hemo_all$air_p_avg, lag=30, argvar=list(fun="poly", degree=1),arglag=list(fun="poly",degree=2))
infar_cb.air_p_avg = crossbasis(infar_all$air_p_avg, lag=30, argvar=list(fun="ns", df=3), arglag=list(fun="poly", degree=1))


hemo_cb.air_p_water = crossbasis(hemo_all$air_p_water, lag=30, argvar=list(fun="poly", degree=1),arglag=list(fun="poly",degree=1))
infar_cb.air_p_water = crossbasis(infar_all$air_p_water, lag=30, argvar=list(fun="ns", df=5), arglag=list(fun="poly", degree=1))


hemo_cb.air_p_l = crossbasis(hemo_all$air_p_l, lag=30, argvar=list(fun="poly", degree=1),arglag=list(fun="poly",degree=1))
infar_cb.air_p_l = crossbasis(infar_all$air_p_l, lag=30, argvar=list(fun="poly", degree=2), arglag=list(fun="poly", degree=1))



hemo_cb.air_p_h = crossbasis(hemo_all$air_p_h, lag=30, argvar=list(fun="poly", degree=2),arglag=list(fun="bs",df=1))
infar_cb.air_p_h = crossbasis(infar_all$air_p_h, lag=30, argvar=list(fun="poly", degree=3), arglag=list(fun="ns", df=1))


hemo_cb.humidity = crossbasis(hemo_all$humidity, lag=30, argvar=list(fun="poly", degree=3),arglag=list(fun="poly",degree=1))
infar_cb.humidity = crossbasis(infar_all$humidity, lag=30, argvar=list(fun="poly", degree=1), arglag=list(fun="poly", degree=1))


hemo_cb.humidity_l = crossbasis(hemo_all$humidity_l, lag=30, argvar=list(fun="poly", degree=2),arglag=list(fun="poly",degree=1))
infar_cb.humidity_l = crossbasis(infar_all$humidity_l, lag=30, argvar=list(fun="poly", degree=1), arglag=list(fun="poly", degree=1))


hemo_cb.w_speed = crossbasis(hemo_all$w_speed, lag=30, argvar=list(fun="ns", df=3),arglag=list(fun="poly",degree=1))
infar_cb.w_speed = crossbasis(infar_all$w_speed, lag=30, argvar=list(fun="poly", degree=1), arglag=list(fun="poly", degree=2))


hemo_cb.max_w_speed = crossbasis(hemo_all$max_w_speed, lag=30, argvar=list(fun="ns", df=3),arglag=list(fun="poly",degree=2))
infar_cb.max_w_speed = crossbasis(infar_all$max_w_speed, lag=30, argvar=list(fun="poly", degree=1), arglag=list(fun="poly", degree=1))


hemo_cb.peek_w_speed = crossbasis(hemo_all$peek_w_speed, lag=30, argvar=list(fun="ns", df=3),arglag=list(fun="poly",degree=1))
infar_cb.peek_w_speed = crossbasis(infar_all$peek_w_speed, lag=30, argvar=list(fun="poly", degree=1), arglag=list(fun="poly", degree=1))



hemo_cb.sunshine = crossbasis(hemo_all$sunshine, lag=30, argvar=list(fun="poly", degree=1),arglag=list(fun="poly",degree=1))
infar_cb.sunshine = crossbasis(infar_all$sunshine, lag=30, argvar=list(fun="poly", degree=1), arglag=list(fun="ns", df=2))


##################################################





#####################################出图函数crosspred###########################
#地址
work_dir <- "F:/文章/小论文/出图/结果出图"
setwd(work_dir)

#DLNM作图程序crosspred
hemo_pred1.aqi = crosspred(hemo_cb.aqi, hemo_model_all, cen=round(median(hemo_all$aqi)), bylag=0.2) # 拟合模型计算 RR 以及 RR 的置信区间， cen 选取对照点作为 RR 分母，bylag 表示 lag 的切片单位。
hemo_pred1.pm2_5 = crosspred(hemo_cb.pm2_5, hemo_model_all, cen=round(median(hemo_all$pm2_5)), bylag=0.2)
hemo_pred1.pm10 = crosspred(hemo_cb.pm10, hemo_model_all, cen=round(median(hemo_all$pm10)), bylag=0.2) 
hemo_pred1.so2 = crosspred(hemo_cb.so2, hemo_model_all, cen=round(median(hemo_all$so2)), bylag=0.2) 
hemo_pred1.no2 = crosspred(hemo_cb.no2, hemo_model_all, cen=round(median(hemo_all$no2)), bylag=0.2) 
hemo_pred1.o3 = crosspred(hemo_cb.o3, hemo_model_all, cen=round(median(hemo_all$o3)), bylag=0.2) 

infar_pred1.aqi = crosspred(infar_cb.aqi, infar_model_all, cen=round(median(infar_all$aqi)), bylag=0.2) # 拟合模型计算 RR 以及 RR 的置信区间， cen 选取对照点作为 RR 分母，bylag 表示 lag 的切片单位。
infar_pred1.pm2_5 = crosspred(infar_cb.pm2_5, infar_model_all, cen=round(median(infar_all$pm2_5)), bylag=0.2)
infar_pred1.pm10 = crosspred(infar_cb.pm10, infar_model_all, cen=round(median(infar_all$pm10)), bylag=0.2) 
infar_pred1.so2 = crosspred(infar_cb.so2, infar_model_all, cen=round(median(infar_all$so2)), bylag=0.2) 
infar_pred1.no2 = crosspred(infar_cb.no2, infar_model_all, cen=round(median(infar_all$no2)), bylag=0.2) 
infar_pred1.o3 = crosspred(infar_cb.o3, infar_model_all, cen=round(median(infar_all$o3)), bylag=0.2)
############################################################################


##################################作图————三维图############################
#缺血性脑卒中
png(file = "调参aqi最优_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.aqi,ticktype='detailed',border='#3366FF',xlab="脑缺血Mean_aqi",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "调参pm2_5最优_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.pm2_5,ticktype='detailed',border='#3366FF',xlab="脑缺血Mean_pm2.5",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "调参pm10最优_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.pm10,ticktype='detailed',border='#3366FF',xlab="脑缺血Mean_pm10",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "调参so2最优_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.so2,ticktype='detailed',border='#3366FF',xlab="脑缺血Mean_so2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "调参no2最优_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.no2,ticktype='detailed',border='#3366FF',xlab="脑缺血Mean_no2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "调参o3最优_三维_缺血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(infar_pred1.o3,ticktype='detailed',border='#3366FF',xlab="脑缺血Mean_o3",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()

#出血性脑卒中
png(file = "调参aqi最优_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.aqi,ticktype='detailed',border='#3366FF',xlab="Mean_aqi",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "调参pm2_5最优_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.pm2_5,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_pm2.5",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "调参pm10最优_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.pm10,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_pm10",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "调参so2最优_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.so2,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_so2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "调参no2最优_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.no2,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_no2",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()
png(file = "调参o3最优_三维_出血性脑卒中.png", width = 4000, height = 3000, res = 300)
plot(hemo_pred1.o3,ticktype='detailed',border='#3366FF',xlab="出血性脑卒中Mean_o3",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
dev.off()

############################################################################

work_dir <- "F:/文章/小论文/出图/结果出图"
setwd(work_dir)

####################################作图——————二维切片图#############################

###############出血性脑卒中
png(file = "二维切片图_aqi_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.aqi,"slices",col="purple",lag=1,
     xlab = "AQI", ylab = "RR", main = "出血性脑卒中-aqi")
lines(hemo_pred1.aqi,col="green" ,lag=7)
lines(hemo_pred1.aqi,col="red" ,lag=15)
lines(hemo_pred1.aqi,col="blue" ,lag=30)
legend("topright", 
       legend = c("Lag = 1", "Lag = 7", "Lag = 15", "Lag = 30"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()


png(file = "二维切片图_no2_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.no2,"slices",col="purple",lag=1,
     xlab = "no2", ylab = "RR", main = "出血性脑卒中-no2")
lines(hemo_pred1.no2,col="green" ,lag=7)
lines(hemo_pred1.no2,col="red" ,lag=15)
lines(hemo_pred1.no2,col="blue" ,lag=30)
legend("topright", 
       legend = c("Lag = 1", "Lag = 7", "Lag = 15", "Lag = 30"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()


png(file = "二维切片图_o3_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.o3,"slices",col="purple",lag=1,
     xlab = "o3", ylab = "RR", main = "出血性脑卒中-o3")
lines(hemo_pred1.o3,col="green" ,lag=7)
lines(hemo_pred1.o3,col="red" ,lag=15)
lines(hemo_pred1.o3,col="blue" ,lag=30)
legend("topright", 
       legend = c("Lag = 1", "Lag = 7", "Lag = 15", "Lag = 30"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()


png(file = "二维切片图_pm2_5_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.pm2_5,"slices",col="purple",lag=1,
     xlab = "pm2_5", ylab = "RR", main = "出血性脑卒中-pm2.5")
lines(hemo_pred1.pm2_5,col="green" ,lag=7)
lines(hemo_pred1.pm2_5,col="red" ,lag=15)
lines(hemo_pred1.pm2_5,col="blue" ,lag=30)
legend("topright", 
       legend = c("Lag = 1", "Lag = 7", "Lag = 15", "Lag = 30"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()


png(file = "二维切片图_pm10_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.pm10,"slices",col="purple",lag=1,
     xlab = "pm10", ylab = "RR", main = "出血性脑卒中-pm10")
lines(hemo_pred1.pm10,col="green" ,lag=7)
lines(hemo_pred1.pm10,col="red" ,lag=15)
lines(hemo_pred1.pm10,col="blue" ,lag=30)
legend("topright", 
       legend = c("Lag = 1", "Lag = 7", "Lag = 15", "Lag = 30"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()


png(file = "二维切片图_so2_出血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(hemo_pred1.so2,"slices",col="purple",lag=1,
     xlab = "so2", ylab = "RR", main = "出血性脑卒中-so2")
lines(hemo_pred1.so2,col="green" ,lag=7)
lines(hemo_pred1.so2,col="red" ,lag=15)
lines(hemo_pred1.so2,col="blue" ,lag=30)
legend("topright", 
       legend = c("Lag = 1", "Lag = 7", "Lag = 15", "Lag = 30"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()



###################缺血性脑卒中
png(file = "二维切片图_aqi_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.aqi,"slices",col="purple",lag=1,
     xlab = "AQI", ylab = "RR", main = "脑缺血-aqi")
lines(infar_pred1.aqi,col="green" ,lag=7)
lines(infar_pred1.aqi,col="red" ,lag=15)
lines(infar_pred1.aqi,col="blue" ,lag=30)
legend("topright", 
       legend = c("Lag = 1", "Lag = 7", "Lag = 15", "Lag = 30"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()


png(file = "二维切片图_no2_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.no2,"slices",col="purple",lag=1,
     xlab = "no2", ylab = "RR", main = "脑缺血-no2")
lines(infar_pred1.no2,col="green" ,lag=7)
lines(infar_pred1.no2,col="red" ,lag=15)
lines(infar_pred1.no2,col="blue" ,lag=30)
legend("topright", 
       legend = c("Lag = 1", "Lag = 7", "Lag = 15", "Lag = 30"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()


png(file = "二维切片图_o3_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.o3,"slices",col="purple",lag=1,
     xlab = "o3", ylab = "RR", main = "脑缺血-o3")
lines(infar_pred1.o3,col="green" ,lag=7)
lines(infar_pred1.o3,col="red" ,lag=15)
lines(infar_pred1.o3,col="blue" ,lag=30)
legend("topright", 
       legend = c("Lag = 1", "Lag = 7", "Lag = 15", "Lag = 30"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()


png(file = "二维切片图_pm2_5_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.pm2_5,"slices",col="purple",lag=1,
     xlab = "pm2_5", ylab = "RR", main = "脑缺血-pm2.5")
lines(infar_pred1.pm2_5,col="green" ,lag=7)
lines(infar_pred1.pm2_5,col="red" ,lag=15)
lines(infar_pred1.pm2_5,col="blue" ,lag=30)
legend("topright", 
       legend = c("Lag = 1", "Lag = 7", "Lag = 15", "Lag = 30"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()


png(file = "二维切片图_pm10_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.pm10,"slices",col="purple",lag=1,
     xlab = "pm10", ylab = "RR", main = "脑缺血-pm10")
lines(infar_pred1.pm10,col="green" ,lag=7)
lines(infar_pred1.pm10,col="red" ,lag=15)
lines(infar_pred1.pm10,col="blue" ,lag=30)
legend("topright", 
       legend = c("Lag = 1", "Lag = 7", "Lag = 15", "Lag = 30"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()


png(file = "二维切片图_so2_缺血性脑卒中.png", width = 2000, height = 1600, res = 300)
plot(infar_pred1.so2,"slices",col="purple",lag=1,
     xlab = "so2", ylab = "RR", main = "脑缺血-so2")
lines(infar_pred1.so2,col="green" ,lag=7)
lines(infar_pred1.so2,col="red" ,lag=15)
lines(infar_pred1.so2,col="blue" ,lag=30)
legend("topright", 
       legend = c("Lag = 1", "Lag = 7", "Lag = 15", "Lag = 30"),
       col = c("purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

######################



######################作图--等高线图###########################################

##################################出血性脑卒中
work_dir <- "F:/文章/小论文/出图/结果出图"
setwd(work_dir)

png(file = "等高线图-aqi-出血性脑卒中.png", width = 2000, height = 1500, res = 300)
plot(hemo_pred1.aqi, "contour", xlab="aqi", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="出血性脑卒中-aqi",ylab="Lag",cex.main=2,cex.lab=1.5))
dev.off()

png(file = "等高线图-no2-出血性脑卒中.png", width = 2000, height = 1500, res = 300)
plot(hemo_pred1.no2, "contour", xlab="no2", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="出血性脑卒中-no2",ylab="Lag",cex.main=2,cex.lab=1.5))
dev.off()

png(file = "等高线图-o3-出血性脑卒中.png", width = 2000, height = 1500, res = 300)
plot(hemo_pred1.o3, "contour", xlab="o3", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="出血性脑卒中-o3",ylab="Lag",cex.main=2,cex.lab=1.5))
dev.off()

png(file = "等高线图-pm2_5-出血性脑卒中.png", width = 2000, height = 1500, res = 300)
plot(hemo_pred1.pm2_5, "contour", xlab="pm2_5", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="出血性脑卒中-pm2_5",ylab="Lag",cex.main=2,cex.lab=1.5))
dev.off()

png(file = "等高线图-pm10-出血性脑卒中.png", width = 2000, height = 1500, res = 300)
plot(hemo_pred1.pm10, "contour", xlab="pm10", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="出血性脑卒中-pm10",ylab="Lag",cex.main=2,cex.lab=1.5))
dev.off()

png(file = "等高线图-so2-出血性脑卒中.png", width = 2000, height = 1500, res = 300)
plot(hemo_pred1.so2, "contour", xlab="so2", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="出血性脑卒中-so2",ylab="Lag",cex.main=2,cex.lab=1.5))
dev.off()

##################################缺血性脑卒中
png(file = "等高线图-aqi-缺血性脑卒中.png", width = 2000, height = 1500, res = 300)
plot(infar_pred1.aqi, "contour", xlab="aqi", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="脑缺血-aqi",ylab="Lag",cex.main=2,cex.lab=1.5))
dev.off()

png(file = "等高线图-no2-缺血性脑卒中.png", width = 2000, height = 1500, res = 300)
plot(infar_pred1.no2, "contour", xlab="no2", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="脑缺血-no2",ylab="Lag",cex.main=2,cex.lab=1.5))
dev.off()

png(file = "等高线图-o3-缺血性脑卒中.png", width = 2000, height = 1500, res = 300)
plot(infar_pred1.o3, "contour", xlab="o3", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="脑缺血-o3",ylab="Lag",cex.main=2,cex.lab=1.5))
dev.off()

png(file = "等高线图-pm2_5-缺血性脑卒中.png", width = 2000, height = 1500, res = 300)
plot(infar_pred1.pm2_5, "contour", xlab="pm2_5", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="脑缺血-pm2_5",ylab="Lag",cex.main=2,cex.lab=1.5))
dev.off()

png(file = "等高线图-pm10-缺血性脑卒中.png", width = 2000, height = 1500, res = 300)
plot(infar_pred1.pm10, "contour", xlab="pm10", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="脑缺血-pm10",ylab="Lag",cex.main=2,cex.lab=1.5))
dev.off()

png(file = "等高线图-so2-缺血性脑卒中.png", width = 2000, height = 1500, res = 300)
plot(infar_pred1.so2, "contour", xlab="so2", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="脑缺血-so2",ylab="Lag",cex.main=2,cex.lab=1.5))
dev.off()
#############################################





#####################################作图 累积关联图################################
work_dir <- "F:/文章/小论文/出图/结果出图"
setwd(work_dir)

###############################################################出血性脑卒中
png(file = "总体累积关联-aqi-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.aqi,hemo_model_all,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_aqi",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-aqi- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.aqi,hemo_model_all,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_aqi",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-aqi- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.aqi,hemo_model_all,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Mean_aqi",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-aqi- 1-30 days",cex=0.89)
dev.off()

png(file = "总体累积关联-no2-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.no2,hemo_model_all,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-no2- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.no2,hemo_model_all,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-no2- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.no2,hemo_model_all,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-no2- 1-30 days",cex=0.89)
dev.off()

png(file = "总体累积关联-o3-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.o3,hemo_model_all,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-o3- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.o3,hemo_model_all,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-o3- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.o3,hemo_model_all,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-o3- 1-30 days",cex=0.89)
dev.off()

png(file = "总体累积关联-pm2_5-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.pm2_5,hemo_model_all,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-pm2_5- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.pm2_5,hemo_model_all,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-pm2_5- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.pm2_5,hemo_model_all,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-pm2_5- 1-30 days",cex=0.89)
dev.off()

png(file = "总体累积关联-pm10-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.pm10,hemo_model_all,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-pm10- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.pm10,hemo_model_all,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-pm10- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.pm10,hemo_model_all,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-pm10- 1-30 days",cex=0.89)
dev.off()

png(file = "总体累积关联-so2-出血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(hemo_cb.so2,hemo_model_all,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-so2- 1-3 days",cex=0.89)
crall <- crossreduce(hemo_cb.so2,hemo_model_all,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-so2- 1-7 days",cex=0.89)
crall <- crossreduce(hemo_cb.so2,hemo_model_all,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-so2- 1-30 days",cex=0.89)
dev.off()

###############################################################缺血性脑卒中
png(file = "总体累积关联-aqi-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.aqi,infar_model_all,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_aqi",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-aqi- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.aqi,infar_model_all,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_aqi",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-aqi- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.aqi,infar_model_all,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Mean_aqi",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-aqi- 1-30 days",cex=0.89)
dev.off()

png(file = "总体累积关联-no2-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.no2,infar_model_all,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-no2- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.no2,infar_model_all,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-no2- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.no2,infar_model_all,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-no2- 1-30 days",cex=0.89)
dev.off()

png(file = "总体累积关联-o3-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.o3,infar_model_all,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-o3- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.o3,infar_model_all,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-o3- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.o3,infar_model_all,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Mean_o3",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-o3- 1-30 days",cex=0.89)
dev.off()

png(file = "总体累积关联-pm2_5-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.pm2_5,infar_model_all,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-pm2_5- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.pm2_5,infar_model_all,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-pm2_5- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.pm2_5,infar_model_all,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-pm2_5- 1-30 days",cex=0.89)
dev.off()

png(file = "总体累积关联-pm10-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.pm10,infar_model_all,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-pm10- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.pm10,infar_model_all,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-pm10- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.pm10,infar_model_all,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-pm10- 1-30 days",cex=0.89)
dev.off()

png(file = "总体累积关联-so2-缺血性脑卒中.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(infar_cb.so2,infar_model_all,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-so2- 1-3 days",cex=0.89)
crall <- crossreduce(infar_cb.so2,infar_model_all,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-so2- 1-7 days",cex=0.89)
crall <- crossreduce(infar_cb.so2,infar_model_all,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Mean_so2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="总体累积关联-so2- 1-30 days",cex=0.89)
dev.off()

################################

####################变量筛选############################
hemo_all<-read_xlsx("F:/文章/小论文/程序/hemo_all_data.xlsx") #门诊数据出血性脑卒中
infar_all<-read_xlsx("F:/文章/小论文/程序/infar_all_data.xlsx") #门诊数据缺血性脑卒中死

#脑出血
hemo_1<- glm(counts ~ hemo_cb.pm2_5 + hemo_cb.pm10 + hemo_cb.so2+ + hemo_cb.no2 + hemo_cb.o3 + ns(date,14*7) + ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_2<- glm(counts ~ hemo_cb.co + hemo_cb.pm10 + hemo_cb.so2+ + hemo_cb.no2 + hemo_cb.o3 + ns(date,14*7) + ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_3<- glm(counts ~ hemo_cb.co + hemo_cb.pm2_5 + hemo_cb.so2+ + hemo_cb.no2 + hemo_cb.o3 + ns(date,14*7) + ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_4<- glm(counts ~ hemo_cb.co + hemo_cb.pm2_5 + hemo_cb.pm10 + hemo_cb.no2 + hemo_cb.o3 + ns(date,14*7) + ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_5<- glm(counts ~ hemo_cb.co + hemo_cb.pm2_5 + hemo_cb.pm10 + hemo_cb.so2 + hemo_cb.o3 + ns(date,14*7) + ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_6<- glm(counts ~ hemo_cb.co + hemo_cb.pm2_5 + hemo_cb.pm10 + hemo_cb.so2 + hemo_cb.no2 + ns(date,14*7) + ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday,family=quasipoisson(), hemo_all) 

#脑缺血
infar_1<- glm(counts ~ infar_cb.pm2_5 + infar_cb.pm10 + infar_cb.so2+ + infar_cb.no2 + infar_cb.o3 + ns(date,14*7) + ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_2<- glm(counts ~ infar_cb.co + infar_cb.pm10 + infar_cb.so2 + infar_cb.no2 + infar_cb.o3 + ns(date,14*7) + ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_3<- glm(counts ~ infar_cb.co + infar_cb.pm2_5 + infar_cb.so2 + infar_cb.no2 + infar_cb.o3 + ns(date,14*7) + ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_4<- glm(counts ~ infar_cb.co + infar_cb.pm2_5 + infar_cb.pm10 + infar_cb.no2 + infar_cb.o3 + ns(date,14*7) + ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_5<- glm(counts ~ infar_cb.co + infar_cb.pm2_5 + infar_cb.pm10 + infar_cb.so2 + infar_cb.o3 + ns(date,14*7) + ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday,family=quasipoisson(), infar_all) 
infar_6<- glm(counts ~ infar_cb.co + infar_cb.pm2_5 + infar_cb.pm10 + infar_cb.so2 + infar_cb.no2 + ns(date,14*7) + ns(humidity,14*7)+ns(sunshine,14*7)+ns(temp_avg,14*7)+ns(rainfall,14*7) + weekend + holiday,family=quasipoisson(), infar_all) 

hp_1<-anova(hemo_model_all,hemo_1,test="LR")[2,"Pr(>Chi)"]
hp_2<-anova(hemo_model_all,hemo_2,test="LR")[2,"Pr(>Chi)"]
hp_3<-anova(hemo_model_all,hemo_3,test="LR")[2,"Pr(>Chi)"]
hp_4<-anova(hemo_model_all,hemo_4,test="LR")[2,"Pr(>Chi)"]
hp_5<-anova(hemo_model_all,hemo_5,test="LR")[2,"Pr(>Chi)"]
hp_6<-anova(hemo_model_all,hemo_6,test="LR")[2,"Pr(>Chi)"]

hc_1<-anova(hemo_model_all,hemo_1,test="LR")[2, "Deviance"]
hc_2<-anova(hemo_model_all,hemo_2,test="LR")[2, "Deviance"]
hc_3<-anova(hemo_model_all,hemo_3,test="LR")[2, "Deviance"]
hc_4<-anova(hemo_model_all,hemo_4,test="LR")[2, "Deviance"]
hc_5<-anova(hemo_model_all,hemo_5,test="LR")[2, "Deviance"]
hc_6<-anova(hemo_model_all,hemo_6,test="LR")[2, "Deviance"]

ip_1<-anova(infar_model_all,infar_1,test="LR")[2,"Pr(>Chi)"]
ip_2<-anova(infar_model_all,infar_2,test="LR")[2,"Pr(>Chi)"]
ip_3<-anova(infar_model_all,infar_3,test="LR")[2,"Pr(>Chi)"]
ip_4<-anova(infar_model_all,infar_4,test="LR")[2,"Pr(>Chi)"]
ip_5<-anova(infar_model_all,infar_5,test="LR")[2,"Pr(>Chi)"]
ip_6<-anova(infar_model_all,infar_6,test="LR")[2,"Pr(>Chi)"]

ic_1<-anova(infar_model_all,infar_1,test="LR")[2, "Deviance"]
ic_2<-anova(infar_model_all,infar_2,test="LR")[2, "Deviance"]
ic_3<-anova(infar_model_all,infar_3,test="LR")[2, "Deviance"]
ic_4<-anova(infar_model_all,infar_4,test="LR")[2, "Deviance"]
ic_5<-anova(infar_model_all,infar_5,test="LR")[2, "Deviance"]
ic_6<-anova(infar_model_all,infar_6,test="LR")[2, "Deviance"]


anova <- data.frame(
  Variable = rep(c("co", "pm2_5", "pm10", "so2", "no2", "o3"), each = 1),
  hemo_chi = c(hc_1, hc_2, hc_3, hc_4, hc_5, hc_6),
  hemo_p = c(hp_1, hp_2, hp_3, hp_4, hp_5, hp_6),
  infar_chi = c(ic_1, ic_2, ic_3, ic_4, ic_5, ic_6),
  infar_p = c(ip_1, ip_2, ip_3, ip_4, ip_5, ip_6)
)







hemo_model_all = glm(counts ~ hemo_cb.aqi + hemo_cb.pm2_5 + hemo_cb.pm10 + hemo_cb.so2 + hemo_cb.no2 + hemo_cb.o3 + ns(date,7) + weekend + holiday
                     ,
                     family=quasipoisson(), hemo_all) 

hemo_1<- glm(counts ~ hemo_cb.pm2_5 + hemo_cb.pm10 + hemo_cb.so2+ + hemo_cb.no2 + hemo_cb.o3 + ns(date,7) +   weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_2<- glm(counts ~ hemo_cb.co + hemo_cb.pm10 + hemo_cb.so2+ + hemo_cb.no2 + hemo_cb.o3 + ns(date,7) +   weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_3<- glm(counts ~ hemo_cb.co + hemo_cb.pm2_5 + hemo_cb.so2+ + hemo_cb.no2 + hemo_cb.o3 + ns(date,7) +   weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_4<- glm(counts ~ hemo_cb.co + hemo_cb.pm2_5 + hemo_cb.pm10 + hemo_cb.no2 + hemo_cb.o3 + ns(date,7) +   weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_5<- glm(counts ~ hemo_cb.co + hemo_cb.pm2_5 + hemo_cb.pm10 + hemo_cb.so2 + hemo_cb.o3 + ns(date,7) +   weekend + holiday,family=quasipoisson(), hemo_all) 
hemo_6<- glm(counts ~ hemo_cb.co + hemo_cb.pm2_5 + hemo_cb.pm10 + hemo_cb.so2 + hemo_cb.no2 + ns(date,7) +   weekend + holiday,family=quasipoisson(), hemo_all) 
summary(hemo_1)

hp_1<-anova(hemo_model_all,hemo_1,test="LR")[2,"Pr(>Chi)"]
hp_2<-anova(hemo_model_all,hemo_2,test="LR")[2,"Pr(>Chi)"]
hp_3<-anova(hemo_model_all,hemo_3,test="LR")[2,"Pr(>Chi)"]
hp_4<-anova(hemo_model_all,hemo_4,test="LR")[2,"Pr(>Chi)"]
hp_5<-anova(hemo_model_all,hemo_5,test="LR")[2,"Pr(>Chi)"]
hp_6<-anova(hemo_model_all,hemo_6,test="LR")[2,"Pr(>Chi)"]

hc_1<-anova(hemo_model_all,hemo_1,test="LR")[2, "Deviance"]
hc_2<-anova(hemo_model_all,hemo_2,test="LR")[2, "Deviance"]
hc_3<-anova(hemo_model_all,hemo_3,test="LR")[2, "Deviance"]
hc_4<-anova(hemo_model_all,hemo_4,test="LR")[2, "Deviance"]
hc_5<-anova(hemo_model_all,hemo_5,test="LR")[2, "Deviance"]
hc_6<-anova(hemo_model_all,hemo_6,test="LR")[2, "Deviance"]







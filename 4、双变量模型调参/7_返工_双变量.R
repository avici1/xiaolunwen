install.packages("tsModel")
install.packages("readxl")
install.packages("writexl")
install.packages("dplyr")


.libPaths()

library(readxl)
library(dlnm)
library(ggplot2)
library(splines) 
library(writexl)
library(dplyr)

stroke_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_stroke.xlsx")

#基向量
stroke_cb.pm10 = crossbasis(stroke_all$pm10, lag=14, argvar=list(fun="ns", df=6), arglag=list(fun="ns", df=6))
stroke_cb.pm2_5 = crossbasis(stroke_all$pm2_5, lag=14, argvar=list(fun="ns", df=6), arglag=list(fun="ns", df=6))
stroke_cb.no2 = crossbasis(stroke_all$no2, lag=14, argvar=list(fun="ns", df=6), arglag=list(fun="ns", df=6))

stroke_cb.temp_avg = crossbasis(stroke_all$temp_avg, lag=4, argvar=list(fun="ns", df=4), arglag=list(fun="ns", df=4))
stroke_cb.eva_cap_b = crossbasis(stroke_all$eva_cap_b, lag=4, argvar=list(fun="ns", df=4), arglag=list(fun="ns", df=4))
stroke_cb.air_p_avg = crossbasis(stroke_all$air_p_avg, lag=4, argvar=list(fun="ns", df=4), arglag=list(fun="ns", df=4))



###################双变量模型##################
#pm2.5
stroke_model_pm2_5_temp_avg = glm(counts ~ stroke_cb.pm2_5 + stroke_cb.temp_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm2_5_eva_cap_b = glm(counts ~ stroke_cb.pm2_5 + stroke_cb.eva_cap_b + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm2_5_air_p_avg = glm(counts ~ stroke_cb.pm2_5 + stroke_cb.air_p_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 


#pm10
stroke_model_pm10_temp_avg = glm(counts ~ stroke_cb.pm10 + stroke_cb.temp_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm10_eva_cap_b = glm(counts ~ stroke_cb.pm10 + stroke_cb.eva_cap_b + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_pm10_air_p_avg = glm(counts ~ stroke_cb.pm10 + stroke_cb.air_p_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 


#NO2
stroke_model_no2_temp_avg = glm(counts ~ stroke_cb.no2 + stroke_cb.temp_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_no2_eva_cap_b = glm(counts ~ stroke_cb.no2 + stroke_cb.eva_cap_b + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
stroke_model_no2_air_p_avg = glm(counts ~ stroke_cb.no2 + stroke_cb.air_p_avg + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all) 
############################################




######################################双变量模型作图程序####################

#######################pm2.5
#pm2.5-temp
stroke_pred1.pm2_5_pm2_5temp_avg = crosspred(stroke_cb.pm2_5, stroke_model_pm2_5_temp_avg, cen=round(median(stroke_all$pm2_5)), bylag=1) 
stroke_pred1.temp_avg_pm2_5temp_avg = crosspred(stroke_cb.temp_avg, stroke_model_pm2_5_temp_avg, cen=round(median(stroke_all$temp_avg)), bylag=1) 

#pm2.5-airp
stroke_pred1.pm2_5_pm2_5air_p_avg = crosspred(stroke_cb.pm2_5, stroke_model_pm2_5_air_p_avg, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.air_p_avg_pm2_5air_p_avg = crosspred(stroke_cb.air_p_avg, stroke_model_pm2_5_air_p_avg, cen=round(median(stroke_all$co)), bylag=1) 

#pm2.5-eva
stroke_pred1.pm2_5_pm2_5eva_cap_b = crosspred(stroke_cb.pm2_5, stroke_model_pm2_5_eva_cap_b, cen=round(median(stroke_all$co)), bylag=1) 
stroke_pred1.eva_cap_b_pm2_5eva_cap_b = crosspred(stroke_cb.eva_cap_b, stroke_model_pm2_5_eva_cap_b, cen=round(median(stroke_all$co)), bylag=1) 


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

##################################################







########################输出最大RR数值######################
RR_wr<-data.frame()
RR_qx<-data.frame()

#建立循环
char_wr <- c("pm2_5", "pm10", "no2")
char_qx <- c("temp_avg","air_p_avg","eva_cap_b")

#一层循环 ii 指双变量模型变量1
for(ii in char_wr){
  #二层循环 jj 指双变量模型变量2
  for(jj in char_qx){
    #三层循环 ii指对谁建模
      dynamic_var1 <- paste0("stroke_pred1.", ii, "_", ii, jj, "$matRRfit")
      dynamic_var2 <- paste0("stroke_pred1.", ii, "_", ii, jj, "$matRRlow")
      dynamic_var3 <- paste0("stroke_pred1.", ii, "_", ii, jj, "$matRRhigh")
      
      RRfit<-as.data.frame(t(apply(eval(parse(text = dynamic_var1)), 2, max)))
      RRl <- as.data.frame(t(apply(eval(parse(text = dynamic_var2)), 2, max)))
      RRh <- as.data.frame(t(apply(eval(parse(text = dynamic_var3)), 2, max)))
      RR_c<-rbind(RRfit,RRl,RRh)[, c("lag0","lag1", "lag7", "lag14")]
      RR_ct<-as.data.frame(t(RR_c))
      RR_ct <- cbind(Variable = rownames(RR_ct), RR_ct)
      RR_ct1<-cbind(ii,ii,jj,RR_ct)
      RR_wr<-rbind(RR_wr,RR_ct1)
  }
}

for(ii in char_wr){
  #二层循环 jj 指双变量模型变量2
  for(jj in char_qx){
    #三层循环 ii指对谁建模
    dynamic_var1 <- paste0("stroke_pred1.", jj, "_", ii, jj, "$matRRfit")
    dynamic_var2 <- paste0("stroke_pred1.", jj, "_", ii, jj, "$matRRlow")
    dynamic_var3 <- paste0("stroke_pred1.", jj, "_", ii, jj, "$matRRhigh")
    
    RRfit<-as.data.frame(t(apply(eval(parse(text = dynamic_var1)), 2, max)))
    RRl <- as.data.frame(t(apply(eval(parse(text = dynamic_var2)), 2, max)))
    RRh <- as.data.frame(t(apply(eval(parse(text = dynamic_var3)), 2, max)))
    RR_c<-rbind(RRfit,RRl,RRh)[, c("lag0", "lag1", "lag3")]
    RR_ct<-as.data.frame(t(RR_c))
    RR_ct <- cbind(Variable = rownames(RR_ct), RR_ct)
    RR_ct1<-cbind(jj,ii,jj,RR_ct)
    RR_qx<-rbind(RR_qx,RR_ct1)
  }
}

RR_wr$V1 <- round(RR_wr$V1, 3)
RR_wr$V2 <- round(RR_wr$V2, 3)
RR_wr$V3 <- round(RR_wr$V3, 3)

RR_wr$V4 <-  paste(RR_wr$V1, "(", RR_wr$V2, ",", RR_wr$V3, ")", sep = "")


RR_qx$V1 <- round(RR_qx$V1, 3)
RR_qx$V2 <- round(RR_qx$V2, 3)
RR_qx$V3 <- round(RR_qx$V3, 3)

RR_qx$V4 <-  paste(RR_qx$V1, "(", RR_qx$V2, ",", RR_qx$V3, ")", sep = "")



write_xlsx(RR_wr,"F:/文章/小论文/出图/2025-1-5/双变量模型/RR_wr.xlsx")
write_xlsx(RR_qx,"F:/文章/小论文/出图/2025-1-5/双变量模型/RR_qx.xlsx")

########################################








##########################输出ANOVA###未完成########################   
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
###############################################################










##########################三维图###################################
work_dir <- "F:/文章/小论文/出图/2025-1-5/双变量模型"
setwd(work_dir)





#空气污染
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

#尝试
png(file = "temp双变量模型_pm2_5_temp_avg_三维.png", width = 4000, height = 3000, res = 300)
plot(stroke_pred1.temp_avg_pm2_5temp_avg,ticktype='detailed',border='#3366FF',xlab="脑卒中Mean_pm2_5",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)
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
#############################################################




###########################累积效应图###################################

work_dir <- "F:/文章/小论文/出图/2025-1-5/双变量模型"
setwd(work_dir)

#pm2_5
#temp
png(file = "双变量pm2_5_temp累积效应pm2_5.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_temp_avg,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_temp 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_temp_avg,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_temp 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_temp_avg,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Mean_pm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_temp 1-14 days",cex=0.89)
dev.off()

#air_p
png(file = "双变量pm2_5_air_p_avg累积效应pm2_5.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_air_p_avg,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_air_p_avg 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_air_p_avg,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_air_p_avg 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_air_p_avg,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_air_p_avg 1-14 days",cex=0.89)
dev.off()

#eva
png(file = "双变量pm2_5_eva_cap_b累积效应pm2_5.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_eva_cap_b,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_eva_cap_b 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_eva_cap_b,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_eva_cap_b 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm2_5, stroke_model_pm2_5_eva_cap_b,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Meanpm2_5",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm2_5_eva_cap_b 1-14 days",cex=0.89)
dev.off()










#pm10
#temp
png(file = "双变量pm10_temp累积效应pm10.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_temp_avg,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_temp 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_temp_avg,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_temp 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_temp_avg,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Mean_pm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_temp 1-14 days",cex=0.89)
dev.off()

#air_p
png(file = "双变量pm10_air_p_avg累积效应pm10.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_air_p_avg,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_air_p_avg 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_air_p_avg,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_air_p_avg 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_air_p_avg,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_air_p_avg 1-14 days",cex=0.89)
dev.off()

#eva
png(file = "双变量pm10_eva_cap_b累积效应pm10.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_eva_cap_b,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_eva_cap_b 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_eva_cap_b,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_eva_cap_b 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.pm10, stroke_model_pm10_eva_cap_b,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Meanpm10",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-pm10_eva_cap_b 1-14 days",cex=0.89)
dev.off()







#no2
#temp
png(file = "双变量no2_temp累积效应no2.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_temp_avg,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_temp 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_temp_avg,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_temp 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_temp_avg,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Mean_no2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_temp 1-14 days",cex=0.89)
dev.off()

#air_p
png(file = "双变量no2_air_p_avg累积效应no2.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_air_p_avg,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_air_p_avg 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_air_p_avg,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_air_p_avg 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_air_p_avg,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_air_p_avg 1-14 days",cex=0.89)
dev.off()

#eva
png(file = "双变量no2_eva_cap_b累积效应no2.png", width = 4500, height = 1600, res = 300)
par(mfrow=c(1,3))
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_eva_cap_b,cen=20,type="overall",lag=c(0,1))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_eva_cap_b 1-3 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_eva_cap_b,cen=20,type="overall",lag=c(0,3))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_eva_cap_b 1-7 days",cex=0.89)
crall <- crossreduce(stroke_cb.no2, stroke_model_no2_eva_cap_b,cen=20,type="overall",lag=c(0,14))
plot(crall,xlab="Meanno2",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="双变量累积效应-no2_eva_cap_b 1-14 days",cex=0.89)
dev.off()

#####################################################





############################二维切片图############
work_dir <- "F:/文章/小论文/出图/2025-1-5/双变量模型"
setwd(work_dir)


png(file = "二维切片图_pm2_5+temp_avg_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.pm2_5_pm2_5temp_avg,"slices",col="purple",lag=1,
     xlab = "pm2_5", ylab = "RR", main = "脑卒中-pm2_5+temp_avg")
lines(stroke_pred1.pm2_5_pm2_5temp_avg,col="black" ,lag=0)
lines(stroke_pred1.pm2_5_pm2_5temp_avg,col="purple" ,lag=1)
lines(stroke_pred1.pm2_5_pm2_5temp_avg,col="green" ,lag=3)
lines(stroke_pred1.pm2_5_pm2_5temp_avg,col="red" ,lag=7)
lines(stroke_pred1.pm2_5_pm2_5temp_avg,col="blue" ,lag=14)
legend("topright", 
       legend = c("lag = 0","lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("black", "purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_pm10+temp_avg_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.pm10_pm10temp_avg,"slices",col="purple",lag=1,
     xlab = "pm10", ylab = "RR", main = "脑卒中-pm10+temp_avg")
lines(stroke_pred1.pm10_pm10temp_avg,col="black" ,lag=0)
lines(stroke_pred1.pm10_pm10temp_avg,col="purple" ,lag=1)
lines(stroke_pred1.pm10_pm10temp_avg,col="green" ,lag=3)
lines(stroke_pred1.pm10_pm10temp_avg,col="red" ,lag=7)
lines(stroke_pred1.pm10_pm10temp_avg,col="blue" ,lag=14)
legend("topright", 
       legend = c("lag = 0","lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("black", "purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

png(file = "二维切片图_no2+temp_avg_脑卒中.png", width = 2000, height = 1600, res = 300)
plot(stroke_pred1.no2_no2temp_avg,"slices",col="purple",lag=1,
     xlab = "no2", ylab = "RR", main = "脑卒中-no2+temp_avg")
lines(stroke_pred1.no2_no2temp_avg,col="black" ,lag=0)
lines(stroke_pred1.no2_no2temp_avg,col="purple" ,lag=1)
lines(stroke_pred1.no2_no2temp_avg,col="green" ,lag=3)
lines(stroke_pred1.no2_no2temp_avg,col="red" ,lag=7)
lines(stroke_pred1.no2_no2temp_avg,col="blue" ,lag=14)
legend("topright", 
       legend = c("lag = 0","lag = 1", "lag = 3", "lag = 7", "lag = 14"),
       col = c("black", "purple", "green", "red", "blue"), 
       lty = 1, 
       bty = "n") 
dev.off()

















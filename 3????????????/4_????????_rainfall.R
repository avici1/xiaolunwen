library(readxl)
library(dlnm)
library(ggplot2)
library(splines) 
library(dplyr)


hemo_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_hemo.xlsx") #门诊数据脑出血
infar_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_infar.xlsx") #门诊数据脑梗死

stroke_all<-read_xlsx("F:/文章/小论文/气象-空气污染物数据/all_data_stroke.xlsx")




######################最终选择模型#####################
hemo_cb.rainfall = crossbasis(hemo_all$rainfall, lag=14, argvar=list(fun="ns", df=1),arglag=list(fun="poly",degree=2))
infar_cb.rainfall = crossbasis(infar_all$rainfall, lag=14, argvar=list(fun="ns", degree=3), arglag=list(fun="poly", df=1))

stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=14, argvar=list(fun="poly", degree=2), arglag=list(fun="ns", df=2))

###############################################################
















#####################脑出血调参###################
#观察lag增大，模型AICBIC变化趋势


#构建一个function用于计算phi logll
qaicbic <- function(model) {
  phi <- summary(model)$dispersion
  logll <- sum(dpois(ceiling(model$y), lambda=exp(predict(model)), log=TRUE))
  cbind((-2*logll + 2*summary(model)$df[3]*phi),
        (-2*logll + log(length(resid(model)))*phi*summary(model)$df[3]))}

hemo_AICBIC <- data.frame()
for (ii in 1:100) {
  hemo_cb.rainfall = crossbasis(hemo_all$rainfall, lag=ii, argvar=list(fun="ns", knots= c(1)),arglag=list(fun="poly",degree=3))
  hemo_model_all = glm(counts ~ hemo_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all)
  summary(hemo_model_all)
  q <- cbind(ii,qaicbic(hemo_model_all))
  hemo_AICBIC <- rbind(hemo_AICBIC,q)
}


infar_AICBIC <- data.frame()
for (ii in 1:100) {
  infar_cb.rainfall = crossbasis(infar_all$rainfall, lag=ii, argvar=list(fun="ns", knots= c(1)),arglag=list(fun="poly",degree=3))
  infar_model_all = glm(counts ~ infar_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), infar_all)
  summary(infar_model_all)
  q <- cbind(ii,qaicbic(infar_model_all))
  infar_AICBIC <- rbind(infar_AICBIC,q)
}

stroke_AICBIC <- data.frame()
for (ii in 1:100) {
  stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=ii, argvar=list(fun="ns", knots= c(1)),arglag=list(fun="poly",degree=3))
  stroke_model_all = glm(counts ~ stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all)
  summary(stroke_model_all)
  q <- cbind(ii,qaicbic(stroke_model_all))
  stroke_AICBIC <- rbind(stroke_AICBIC,q)
}

####################################################


stroke_poly_AICBIC<-data.frame()
#####################argvar fun=poly ,arglag fun=poly#####################
# 1级循环：控制 lag 的值
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=ii, argvar=list(fun="poly", degree=jj), arglag=list(fun="poly", degree=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      stroke_model = glm(counts ~ stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=ns,df=", jj,"arglag=poly,degree=",kk, qaicbic(stroke_model))  
      # 将结果合并到总结果中
      stroke_poly_AICBIC <- rbind(stroke_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=ns ,arglag fun=poly#####################

# 1级循环：控制 lag 的值
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=ii, argvar=list(fun="ns", df=jj), arglag=list(fun="poly", degree=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      stroke_model = glm(counts ~ stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=ns,df=", jj,"arglag=poly,degree=",kk, qaicbic(stroke_model))  
      # 将结果合并到总结果中
      stroke_poly_AICBIC <- rbind(stroke_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=bs ,arglag fun=poly#####################

for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=ii, argvar=list(fun="bs", df=jj), arglag=list(fun="poly", degree=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      stroke_model = glm(counts ~ stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=bs,df=", jj,"arglag=poly,degree=",kk, qaicbic(stroke_model))  
      # 将结果合并到总结果中
      stroke_poly_AICBIC <- rbind(stroke_poly_AICBIC, q)
    }
  }
}


#####################argvar fun=poly ,arglag fun=ns#####################
for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=ii, argvar=list(fun="poly", degree=jj), arglag=list(fun="ns", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      stroke_model = glm(counts ~ stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=poly,degree=", jj,"arglag=ns,df=",kk, qaicbic(stroke_model))  
      # 将结果合并到总结果中
      stroke_poly_AICBIC <- rbind(stroke_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=ns ,arglag fun=ns#####################
for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=ii, argvar=list(fun="ns", df=jj), arglag=list(fun="ns", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      stroke_model = glm(counts ~ stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=ns,df=", jj,"arglag=ns,df=",kk, qaicbic(stroke_model))  
      # 将结果合并到总结果中
      stroke_poly_AICBIC <- rbind(stroke_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=bs ,arglag fun=ns#####################
for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=ii, argvar=list(fun="bs", df=jj), arglag=list(fun="ns", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      stroke_model = glm(counts ~ stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=bs,df=", jj,"arglag=ns,df=",kk, qaicbic(stroke_model))  
      # 将结果合并到总结果中
      stroke_poly_AICBIC <- rbind(stroke_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=poly ,arglag fun=bs#####################
for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=ii, argvar=list(fun="poly", degree=jj), arglag=list(fun="bs", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      stroke_model = glm(counts ~ stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=poly,degree=", jj,"arglag=bs,df=",kk, qaicbic(stroke_model))  
      # 将结果合并到总结果中
      stroke_poly_AICBIC <- rbind(stroke_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=ns ,arglag fun=bs#####################
for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=ii, argvar=list(fun="ns", df=jj), arglag=list(fun="bs", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      stroke_model = glm(counts ~ stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=ns,df=", jj,"arglag=bs,df=",kk, qaicbic(stroke_model))  
      # 将结果合并到总结果中
      stroke_poly_AICBIC <- rbind(stroke_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=bs ,arglag fun=bs#####################
for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      stroke_cb.rainfall = crossbasis(stroke_all$rainfall, lag=ii, argvar=list(fun="bs", df=jj), arglag=list(fun="bs", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      stroke_model = glm(counts ~ stroke_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), stroke_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=bs,df=", jj,"arglag=bs,df=",kk, qaicbic(stroke_model))  
      # 将结果合并到总结果中
      stroke_poly_AICBIC <- rbind(stroke_poly_AICBIC, q)
    }
  }
}
###########################################

#排序AIC BIC
stroke_poly_AICBIC_sortAIC <- stroke_poly_AICBIC %>%
  arrange(V7)
stroke_poly_AICBIC_sortBIC <- stroke_poly_AICBIC %>%
  arrange(V8)

####################################################

####################################################





#################################脑出血-hemo############################
#动态数据框
hemo_poly_AICBIC <- data.frame()


#####################argvar fun=poly ,arglag fun=poly#####################
# 1级循环：控制 lag 的值
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      hemo_cb.rainfall = crossbasis(hemo_all$rainfall, lag=ii, argvar=list(fun="poly", degree=jj), arglag=list(fun="poly", degree=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      hemo_model = glm(counts ~ hemo_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=ns,df=", jj,"arglag=poly,degree=",kk, qaicbic(hemo_model))  
      # 将结果合并到总结果中
      hemo_poly_AICBIC <- rbind(hemo_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=ns ,arglag fun=poly#####################

# 1级循环：控制 lag 的值
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      hemo_cb.rainfall = crossbasis(hemo_all$rainfall, lag=ii, argvar=list(fun="ns", df=jj), arglag=list(fun="poly", degree=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      hemo_model = glm(counts ~ hemo_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=ns,df=", jj,"arglag=poly,degree=",kk, qaicbic(hemo_model))  
      # 将结果合并到总结果中
      hemo_poly_AICBIC <- rbind(hemo_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=bs ,arglag fun=poly#####################

for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      hemo_cb.rainfall = crossbasis(hemo_all$rainfall, lag=ii, argvar=list(fun="bs", df=jj), arglag=list(fun="poly", degree=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      hemo_model = glm(counts ~ hemo_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=bs,df=", jj,"arglag=poly,degree=",kk, qaicbic(hemo_model))  
      # 将结果合并到总结果中
      hemo_poly_AICBIC <- rbind(hemo_poly_AICBIC, q)
    }
  }
}


#####################argvar fun=poly ,arglag fun=ns#####################
for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      hemo_cb.rainfall = crossbasis(hemo_all$rainfall, lag=ii, argvar=list(fun="poly", degree=jj), arglag=list(fun="ns", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      hemo_model = glm(counts ~ hemo_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=poly,degree=", jj,"arglag=ns,df=",kk, qaicbic(hemo_model))  
      # 将结果合并到总结果中
      hemo_poly_AICBIC <- rbind(hemo_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=ns ,arglag fun=ns#####################
for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      hemo_cb.rainfall = crossbasis(hemo_all$rainfall, lag=ii, argvar=list(fun="ns", df=jj), arglag=list(fun="ns", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      hemo_model = glm(counts ~ hemo_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=ns,df=", jj,"arglag=ns,df=",kk, qaicbic(hemo_model))  
      # 将结果合并到总结果中
      hemo_poly_AICBIC <- rbind(hemo_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=bs ,arglag fun=ns#####################
for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      hemo_cb.rainfall = crossbasis(hemo_all$rainfall, lag=ii, argvar=list(fun="bs", df=jj), arglag=list(fun="ns", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      hemo_model = glm(counts ~ hemo_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=bs,df=", jj,"arglag=ns,df=",kk, qaicbic(hemo_model))  
      # 将结果合并到总结果中
      hemo_poly_AICBIC <- rbind(hemo_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=poly ,arglag fun=bs#####################
for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      hemo_cb.rainfall = crossbasis(hemo_all$rainfall, lag=ii, argvar=list(fun="poly", degree=jj), arglag=list(fun="bs", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      hemo_model = glm(counts ~ hemo_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=poly,degree=", jj,"arglag=bs,df=",kk, qaicbic(hemo_model))  
      # 将结果合并到总结果中
      hemo_poly_AICBIC <- rbind(hemo_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=ns ,arglag fun=bs#####################
for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      hemo_cb.rainfall = crossbasis(hemo_all$rainfall, lag=ii, argvar=list(fun="ns", df=jj), arglag=list(fun="bs", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      hemo_model = glm(counts ~ hemo_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=ns,df=", jj,"arglag=bs,df=",kk, qaicbic(hemo_model))  
      # 将结果合并到总结果中
      hemo_poly_AICBIC <- rbind(hemo_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=bs ,arglag fun=bs#####################
for (ii in 1:14) {  
  for (jj in 1:5) {
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      hemo_cb.rainfall = crossbasis(hemo_all$rainfall, lag=ii, argvar=list(fun="bs", df=jj), arglag=list(fun="bs", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      hemo_model = glm(counts ~ hemo_cb.rainfall + ns(date,8*7) + weekend + holiday,family=quasipoisson(), hemo_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=bs,df=", jj,"arglag=bs,df=",kk, qaicbic(hemo_model))  
      # 将结果合并到总结果中
      hemo_poly_AICBIC <- rbind(hemo_poly_AICBIC, q)
    }
  }
}
###########################################

#排序AIC BIC
hemo_poly_AICBIC_sortAIC <- hemo_poly_AICBIC %>%
  arrange(V7)
hemo_poly_AICBIC_sortBIC <- hemo_poly_AICBIC %>%
  arrange(V8)

























##################################脑梗死infar############################

qaicbic <- function(model) {
  phi <- summary(model)$dispersion
  logll <- sum(dpois(ceiling(model$y), lambda=exp(predict(model)), log=TRUE))
  cbind((-2*logll + 2*summary(model)$df[3]*phi),
        (-2*logll + log(length(resid(model)))*phi*summary(model)$df[3]))}






#动态数据框
infar_poly_AICBIC <- data.frame()

#####################argvar fun=poly ,arglag fun=poly#####################
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      infar_cb.rainfall = crossbasis(infar_all$rainfall, lag=ii, argvar=list(fun="poly", degree=jj), arglag=list(fun="poly", degree=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      infar_model = glm(counts ~ infar_cb.rainfall + ns(date, 10*1), family=quasipoisson(), data=infar_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=poly,degree=", jj,"arglag=poly,degree=",kk, qaicbic(infar_model))  
      # 将结果合并到总结果中
      infar_poly_AICBIC <- rbind(infar_poly_AICBIC, q)
    }
  }
}

#####################argvar fun=ns ,arglag fun=poly#####################
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      infar_cb.rainfall = crossbasis(infar_all$rainfall, lag=ii, argvar=list(fun="ns", df=jj), arglag=list(fun="poly", degree=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      infar_model = glm(counts ~ infar_cb.rainfall + ns(date, 10*1), family=quasipoisson(), data=infar_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=ns,df=", jj,"arglag=poly,degree=",kk, qaicbic(infar_model))  
      # 将结果合并到总结果中
      infar_poly_AICBIC <- rbind(infar_poly_AICBIC, q)
    }
  }
}
#####################argvar fun=bs ,arglag fun=poly#####################
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      infar_cb.rainfall = crossbasis(infar_all$rainfall, lag=ii, argvar=list(fun="bs", df=jj), arglag=list(fun="poly", degree=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      infar_model = glm(counts ~ infar_cb.rainfall + ns(date, 10*1), family=quasipoisson(), data=infar_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=bs,df=", jj,"arglag=poly,degree=",kk, qaicbic(infar_model))  
      # 将结果合并到总结果中
      infar_poly_AICBIC <- rbind(infar_poly_AICBIC, q)
    }
  }
}
#####################argvar fun=poly ,arglag fun=ns#####################
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      infar_cb.rainfall = crossbasis(infar_all$rainfall, lag=ii, argvar=list(fun="poly", degree=jj), arglag=list(fun="ns", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      infar_model = glm(counts ~ infar_cb.rainfall + ns(date, 10*1), family=quasipoisson(), data=infar_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=poly,degree=", jj,"arglag=ns,df=",kk, qaicbic(infar_model))  
      # 将结果合并到总结果中
      infar_poly_AICBIC <- rbind(infar_poly_AICBIC, q)
    }
  }
}
#####################argvar fun=ns ,arglag fun=ns#####################
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      infar_cb.rainfall = crossbasis(infar_all$rainfall, lag=ii, argvar=list(fun="ns", df=jj), arglag=list(fun="ns", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      infar_model = glm(counts ~ infar_cb.rainfall + ns(date, 10*1), family=quasipoisson(), data=infar_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=ns,df=", jj,"arglag=ns,df=",kk, qaicbic(infar_model))  
      # 将结果合并到总结果中
      infar_poly_AICBIC <- rbind(infar_poly_AICBIC, q)
    }
  }
}
#####################argvar fun=bs ,arglag fun=ns#####################
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      infar_cb.rainfall = crossbasis(infar_all$rainfall, lag=ii, argvar=list(fun="bs", df=jj), arglag=list(fun="ns", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      infar_model = glm(counts ~ infar_cb.rainfall + ns(date, 10*1), family=quasipoisson(), data=infar_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=bs,df=", jj,"arglag=ns,df=",kk, qaicbic(infar_model))  
      # 将结果合并到总结果中
      infar_poly_AICBIC <- rbind(infar_poly_AICBIC, q)
    }
  }
}
#####################argvar fun=poly ,arglag fun=bs#####################
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      infar_cb.rainfall = crossbasis(infar_all$rainfall, lag=ii, argvar=list(fun="poly", degree=jj), arglag=list(fun="bs", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      infar_model = glm(counts ~ infar_cb.rainfall + ns(date, 10*1), family=quasipoisson(), data=infar_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=poly,degree=", jj,"arglag=bs,df=",kk, qaicbic(infar_model))  
      # 将结果合并到总结果中
      infar_poly_AICBIC <- rbind(infar_poly_AICBIC, q)
    }
  }
}
#####################argvar fun=ns ,arglag fun=bs#####################
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      infar_cb.rainfall = crossbasis(infar_all$rainfall, lag=ii, argvar=list(fun="ns", df=jj), arglag=list(fun="bs", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      infar_model = glm(counts ~ infar_cb.rainfall + ns(date, 10*1), family=quasipoisson(), data=infar_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=ns,df=", jj,"arglag=ns,df=",kk, qaicbic(infar_model))  
      # 将结果合并到总结果中
      infar_poly_AICBIC <- rbind(infar_poly_AICBIC, q)
    }
  }
}
#####################argvar fun=bs ,arglag fun=bs#####################
for (ii in 1:14) {  
  # 2级循环：控制argvar的值
  for (jj in 1:5) {
    # 3级循环：控制arglag的值
    for ( kk in 1:5){
      # 创建交叉基函数，其中 lag 由外部循环 jj 控制
      infar_cb.rainfall = crossbasis(infar_all$rainfall, lag=ii, argvar=list(fun="bs", df=jj), arglag=list(fun="bs", df=kk))
      # 拟合广义线性模型，使用 quasipoisson 分布
      infar_model = glm(counts ~ infar_cb.rainfall + ns(date, 10*1), family=quasipoisson(), data=infar_all)
      # 获取 AIC 和 BIC 值
      q <- cbind("lag=",ii,"argvar=bs,df=", jj,"arglag=ns,df=",kk, qaicbic(infar_model))  
      # 将结果合并到总结果中
      infar_poly_AICBIC <- rbind(infar_poly_AICBIC, q)
    }
  }
}


infar_poly_AICBIC_sortAIC <- infar_poly_AICBIC %>%
  arrange(V7)
infar_poly_AICBIC_sortBIC <- infar_poly_AICBIC %>%
  arrange(V8)







# Task2
data <- read.csv("data_task2.csv")
pre_data <- data.frame(age=seq(55,110,by = 1))
reg_data <- data.frame(age=seq(55,74,by = 1))
library(ggplot2)
library(dplyr)  #for using group_by()
library(lme4) #to use mixed-effect model
# scatter plot
ggplot(data, aes(x = age, y = DV_amyloid)) +
  geom_point() +
  labs(title = "DV Amyloid vs Age", x = "Age", y = "DV Amyloid") +
  theme_minimal()
#mean value plot (group by age)
average_amyloid <- data %>%
  group_by(age) %>%
  summarize(mean_amyloid = mean(DV_amyloid, na.rm = TRUE))
# 查看结果
# print(average_amyloid)
ggplot(average_amyloid,(aes(x = age, y = mean_amyloid))) +
   geom_point() +
   labs(title = "mean DV Amyloid vs Age", x = "Age", y = "mean DV Amyloid") +
   theme_minimal()

# construct linear model
attach(average_amyloid)
model_linear <- lm(mean_amyloid ~ age)
detach(average_amyloid)
summary(model_linear)
# coefficients(model_linear)

# plot prediction

attach(data)
par(mfrow=c(1,1))
plot(age,DV_amyloid)#绘制散点图
abline(model_linear,col="blue",lwd = 2)#添加拟合直线
par(mfrow=c(2,2))
plot(model_linear)#绘制回归曲线的图
detach(data)


# construct logarithmic model
attach(average_amyloid)
model_log <- lm(mean_amyloid ~ log(age))
detach(average_amyloid)
summary(model_log)
# coefficients(model_log)

# 绘制预测散点图和拟合曲线
# 不需要 attach 和 detach
par(mfrow=c(1,1))  # 单图模式
# 绘制原始数据的散点图
plot(average_amyloid$age, average_amyloid$mean_amyloid, 
     main = "回归模型与原始数据对比", 
     xlab = "Age", 
     ylab = "Mean Amyloid",
     pch = 19)
# 绘制回归拟合曲线
lines(average_amyloid$age, fitted(model_log), col = "blue", lwd = 2)
# 绘制回归诊断图
par(mfrow=c(2,2))  # 切换到 2x2 图模式
plot(model_log)  # R 会自动生成 4 个诊断图


# construct polynomial model(2)
attach(average_amyloid)
model_poly2 <- lm(mean_amyloid ~ poly(age,2))
detach(average_amyloid)
summary(model_poly2)
# coefficients(model_poly2)

# plot prediction
par(mfrow=c(1,1))
plot(average_amyloid$age, average_amyloid$mean_amyloid, 
     main = "二次多项式回归拟合",
     xlab = "Age", ylab = "Mean Amyloid", pch = 19)
lines(average_amyloid$age, fitted(model_poly2),col="blue",lwd = 2)
par(mfrow=c(2,2))
plot(model_poly2)#绘制回归曲线的图


# construct mixed-effect model
# 创建新的分组变量 Y
data$Y <- floor(data$X / 17)
# 使用lmer函数构建模型，age为固定效应，Y为随机效应
model_mixed <- lmer(DV_amyloid ~ age + (1|Y), data = data)

# 输出模型摘要，查看固定效应和随机效应的结果
summary(model_mixed)
# plot prediction
set.seed(123)  # 为了可重复性，设置随机种子
reg_data$Y <- sample(data$Y, 20, replace = TRUE)  # 随机选择 20 个值，允许重复
reg_data$mix_amyloid <- predict(model_mixed, newdata=reg_data)
par(mfrow=c(1,1))
plot(data$age, data$DV_amyloid, 
     main = "mixed effect",
     xlab = "Age", ylab = "Amyloid", pch = 19, col = rgb(0.1,0.7,0.6,0.5))

lines(average_amyloid$age, reg_data$mix_amyloid,col="blue",lwd = 2)




## predict age from 75 to 110
pre_data$amy_linear <- predict(model_linear, newdata=pre_data)
pre_data$amy_log <- predict(model_log, newdata=pre_data)
pre_data$amy_poly2 <- predict(model_poly2, newdata=pre_data)
# 1. 绘制 data 数据的散点图 (age 从 55 到 74)
par(mfrow=c(1,1))
plot(data$age, data$DV_amyloid, 
     main = "DV_amyloid 和模型预测对比图", 
     xlab = "Age", 
     ylab = "DV_amyloid", 
     pch = 19, 
     col = "darkblue", 
     xlim = c(55, 110),  # 设置 X 轴范围覆盖两个数据集
     ylim = c(50,300))

# 2. 添加 pre_data 中线性模型的预测曲线 (age 从 75 到 110)
lines(pre_data$age, pre_data$amy_linear, col = "red", lwd = 2)

# 3. 添加 pre_data 中对数模型的预测曲线
lines(pre_data$age, pre_data$amy_log, col = "blue", lwd = 2)

# 4. 添加 pre_data 中二次多项式模型的预测曲线
lines(pre_data$age, pre_data$amy_poly2, col = "green", lwd = 2)

# 5. 添加图例
legend("topleft", legend = c("原始数据", "线性模型", "对数模型", "二次多项式模型"), 
       col = c("darkblue", "red", "blue", "green"), 
       pch = c(19, NA, NA, NA), 
       lwd = c(NA, 2, 2, 2))

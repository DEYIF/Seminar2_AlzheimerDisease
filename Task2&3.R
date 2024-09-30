# Task2
data_2 <- read.csv("data_task2.csv")
# pre_data <- data.frame(age=seq(55,110,by = 1))
reg_data <- data.frame(age=seq(55,74,by = 1))
library(ggplot2)
library(dplyr)  #for using group_by()
library(lme4) #to use mixed-effect model

# scatter plot
ggplot(data_2, aes(x = age, y = DV_amyloid)) +
  geom_point(pch = 19, col = rgb(0.15,0.6,0.96,0.6)) +
  labs(title = "DV Amyloid vs Age", x = "Age", y = "DV Amyloid") +
  theme_minimal()
#mean value plot (group by age)
average_amyloid <- data_2 %>%
  group_by(age) %>%
  summarize(mean_amyloid = mean(DV_amyloid, na.rm = TRUE))
# 查看结果
# print(average_amyloid)
ggplot(average_amyloid,(aes(x = age, y = mean_amyloid))) +
   geom_point(pch = 19, col = rgb(0.15,0.6,0.96,0.6)) +
   labs(title = "mean DV Amyloid vs Age", x = "Age", y = "mean DV Amyloid") +
   theme_minimal()

# construct Linear Model
model_linear <- lm(average_amyloid$mean_amyloid ~ average_amyloid$age)
summary(model_linear)
coefficients(model_linear)

# plot prediction
ggplot(data_2, aes(x = age, y = DV_amyloid)) +
  geom_point(color = rgb(0.15, 0.6, 0.96, 0.6), size = 2) +  # 绘制散点
  geom_smooth(method = "lm", color = "red", se = FALSE, size = 1) +  # 添加线性回归直线
  labs(title = "regression line & original data", x = "Age", y = "DV Amyloid") +
  theme_minimal()
ggplot(average_amyloid, aes(x = age, y = mean_amyloid)) +
  geom_point(color = rgb(0.15, 0.6, 0.96, 0.6), size = 2) +  # 绘制散点
  geom_smooth(method = "lm", color = "red", se = FALSE, size = 1) +  # 添加线性回归直线
  labs(title = "regression line & mean data", x = "Age", y = "Mean DV Amyloid") +
  theme_minimal()

# 残差 vs 拟合值
ggplot(average_amyloid, aes(x = fitted(model_linear), y = residuals(model_linear))) +
  geom_point(color = rgb(0.15, 0.6, 0.96, 0.6), size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
# 正态 Q-Q 图
ggplot(data.frame(residuals = residuals(model_linear)), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", size = 1.2) +
  labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
# Shapiro-Wilk test
shpiro_result <- shapiro.test(residuals(model_linear))
print(shpiro_result)


# construct logarithmic model
model_log <- lm((average_amyloid$mean_amyloid) ~ log(average_amyloid$age))
summary(model_log)
# plot prediction
new_data <- data.frame(age = seq(min(average_amyloid$age), max(average_amyloid$age)))
new_data$predicted_log_Amyloid <- predict(model_log, newdata = new_data)
ggplot() +
  geom_point(data = data_2, aes(x= age, y=DV_amyloid), color = rgb(0.15, 0.6, 0.96, 0.6), size = 2) +  # 绘制散点
  geom_line(data = new_data,aes(x = age, y = predicted_log_Amyloid), color = "red", size = 1.2) +  # 添加线性回归直线
  labs(title = "Logarithmic Fit vs. Age", 
       x = "Age", 
       y = "Mean DV Amyloid") +
  theme_minimal()

# 残差 vs 拟合值
ggplot(average_amyloid, aes(x = fitted(model_log), y = residuals(model_log))) +
  geom_point(color = rgb(0.15, 0.6, 0.96, 0.6), size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
# 正态 Q-Q 图
ggplot(data.frame(residuals = residuals(model_log)), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", size = 1.2) +
  labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
# Shapiro-Wilk test
shpiro_result <- shapiro.test(residuals(model_log))
print(shpiro_result)
# 绘制原始数据的散点图



# construct polynomial model(2)
model_poly2 <- lm(average_amyloid$mean_amyloid ~ poly(average_amyloid$age,2))
summary(model_poly2)

# plot prediction
new_data$predicted_poly2_Amyloid <- predict(model_poly2, newdata = new_data)
ggplot() +
  geom_point(data = data_2, aes(x= age, y=DV_amyloid), color = rgb(0.15, 0.6, 0.96, 0.6), size = 2) +  # 绘制散点
  geom_line(data = new_data,aes(x = age, y = predicted_poly2_Amyloid), color = "red", size = 1.2) +  # 添加线性回归直线
  labs(title = "polynomial2 Fit vs. Age", 
       x = "Age", 
       y = "Mean DV Amyloid") +
  theme_minimal()
dev.off()
# 残差 vs 拟合值
ggplot(average_amyloid, aes(x = fitted(model_poly2), y = residuals(model_poly2))) +
  geom_point(color = rgb(0.15, 0.6, 0.96, 0.6), size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
# 正态 Q-Q 图
ggplot(data.frame(residuals = residuals(model_poly2)), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", size = 1.2) +
  labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
# Shapiro-Wilk test
shpiro_result <- shapiro.test(residuals(model_poly2))
print(shpiro_result)


# construct mixed-effect model
# 创建新的分组变量 Y
data_2$Y <- floor(data_2$X / 17)
# 使用lmer函数构建模型，age为固定效应，Y为随机效应
model_mixed <- lmer(DV_amyloid ~ age + (1|Y), data = data_2)
# 输出模型摘要，查看固定效应和随机效应的结果
summary(model_mixed)

# plot prediction

# plot prediction
new_data <- data.frame(age = seq(min(average_amyloid$age), max(average_amyloid$age)))
set.seed(123)  # 为了可重复性，设置随机种子
new_data$Y <- sample(data_2$Y, length(reg_data[1]), replace = TRUE)  # 随机选择 20 个值，允许重复
new_data$predicted_mix_Amyloid <- predict(model_mixed, newdata = new_data)

ggplot() +
  geom_point(data = data_2, aes(x= age, y=DV_amyloid), color = rgb(0.15, 0.6, 0.96, 0.6), size = 2) +  # 绘制散点
  geom_line(data = new_data,aes(x = age, y = predict(model_poly2)), color = "red", size = 1.2) +  # 添加线性回归直线
  labs(title = "mixed effect Fit vs. Age", 
       x = "Age", 
       y = "Mean DV Amyloid") +
  theme_minimal()

# 正态 Q-Q 图
ggplot(data.frame(residuals = residuals(model_log)), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", size = 1.2) +
  labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
# Shapiro-Wilk test
shpiro_result <- shapiro.test(residuals(model_log))
print(shpiro_result)
# 绘制原始数据的散点图

lines(average_amyloid$age, reg_data$mix_amyloid,col="blue",lwd = 2)




## predict age from 75 to 110
pre_data$amy_linear <- predict(model_linear, newdata=pre_data)
pre_data$amy_log <- predict(model_log, newdata=pre_data)
pre_data$amy_poly2 <- predict(model_poly2, newdata=pre_data)
# 1. 绘制 data_2 数据的散点图 (age 从 55 到 74)
par(mfrow=c(1,1))
plot(data_2$age, data_2$DV_amyloid, 
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

# Task3
data_3 <- read.csv("data_task3.csv")
data_3$Y <- sample(data_2$Y, nrow(data_3), replace = TRUE)
data_3$preamy_lin <- predict(model_linear, newdata = data_3)
data_3$preamy_log <- predict(model_log, newdata = data_3)
data_3$preamy_poly2 <- predict(model_poly2, newdata = data_3)
# 生成 scatter plot 和预测点、预测线
ggplot(data_3, aes(x = age, y = DV_amyloid)) +
  geom_point(color = "blue", shape =1, size = 3) +  # 绘制原始数据的散点图
  geom_point(aes(y = preamy_lin), color = "red", shape = 0, size = 3) +  # 绘制预测点
  geom_line(aes(y = preamy_lin), color = "red", size = 1) +  # 绘制预测线
  geom_point(aes(y = preamy_log), color = "red", shape = 1, size = 3) +  # 绘制预测点
  geom_line(aes(y = preamy_log), color = "red", size = 1) +  # 绘制预测线
  geom_point(aes(y = preamy_poly2), color = "red", shape = 2, size = 3) +  # 绘制预测点
  geom_line(aes(y = preamy_poly2), color = "red", size = 1) +  # 绘制预测线
  labs(title = "DV Amyloid vs Age with Prediction", 
       x = "Age", 
       y = "DV Amyloid") +
  theme_minimal()
# 添加图例
legend("topleft", legend = c("原始数据", "线性模型", "对数模型", "二次多项式模型"), 
       col = c("darkblue", "red", "blue", "green"), 
       pch = c(19, NA, NA, NA), 
       
       lwd = c(NA, 2, 2, 2))

# draw the comparison plot between the original data and the predicted data
ggplot() +
  geom_point(data=data_3, aes(x = age, y = DV_amyloid),color = "red", shape = 9, size = 2) +  # predict data
  geom_point(data=data_2, aes(x=age, y = DV_amyloid), color = "blue", shape = 1, size = 2) +  # original data
  labs(title = "DV Amyloid vs Age with Prediction", 
       x = "Age", 
       y = "DV Amyloid") +
  theme_minimal()

# improve:

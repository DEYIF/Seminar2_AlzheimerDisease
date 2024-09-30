# Task2
library(ggplot2)
library(dplyr)  #for using group_by()
library(lme4) #to use mixed-effect model
data_2 <- read.csv("data_task2.csv")
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

# construct linear model
model_linear <- lm(mean_amyloid ~ age, data = average_amyloid)
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
# model_log <- lm((average_amyloid$mean_amyloid) ~ log(average_amyloid$age))
model_log <- lm(mean_amyloid ~ log(age), data = average_amyloid)
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
  labs(title = "logarithmic model Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
# 正态 Q-Q 图
ggplot(data.frame(residuals = residuals(model_log)), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", size = 1.2) +
  labs(title = "logarithmic model Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
# Shapiro-Wilk test
shpiro_result <- shapiro.test(residuals(model_log))
print(shpiro_result)



# construct polynomial model(2)
# model_poly2 <- lm(average_amyloid$mean_amyloid ~ poly(average_amyloid$age,2))
model_poly2 <- lm(mean_amyloid ~ poly(age,2), data = average_amyloid)
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
  labs(title = "polynomial model(2) Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
# 正态 Q-Q 图
ggplot(data.frame(residuals = residuals(model_poly2)), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", size = 1.2) +
  labs(title = "polynomial model(2) Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
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
set.seed(123)  # 为了可重复性，设置随机种子
new_data$Y <- sample(data_2$Y,20,replace = TRUE)  # 随机选择 20 个值，允许重复
new_data$predicted_mix_Amyloid <- predict(model_mixed, newdata = new_data)

ggplot() +
  geom_point(data = data_2, aes(x= age, y=DV_amyloid), color = rgb(0.15, 0.6, 0.96, 0.6), size = 2) +  # 绘制散点
  geom_line(data = new_data,aes(x = age, y=predicted_mix_Amyloid), color = "red", size = 1.2) +  # 添加线性回归直线
  labs(title = "mixed effect Fit vs. Age", 
       x = "Age", 
       y = "Mean DV Amyloid") +
  theme_minimal()
dev.off()
# 残差 vs 拟合值
res <- average_amyloid$mean_amyloid - new_data$predicted_mix_Amyloid
ggplot(average_amyloid, aes(x = new_data$predicted_mix_Amyloid, y = res)) +
  geom_point(color = rgb(0.15, 0.6, 0.96, 0.6), size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "mixed-effect model Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()
# 正态 Q-Q 图
ggplot(data.frame(residual = res), aes(sample = residual)) +
  stat_qq() +
  stat_qq_line(color = "red", size = 1.2) +
  labs(title = "mixed effect model Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()
# Shapiro-Wilk test
shpiro_result <- shapiro.test(residuals(model_mixed))
print(shpiro_result)




## predict age from 75 to 110
pre_data <- data.frame(age=seq(75,110,by = 1))
pre_data$Y <- sample(data_2$Y, nrow(pre_data), replace = TRUE)
pre_data$amy_linear <- predict(model_linear, newdata=pre_data)
pre_data$amy_log <- predict(model_log, newdata=pre_data)
pre_data$amy_poly2 <- predict(model_poly2, newdata=pre_data)
pre_data$amy_mixed <- predict(model_mixed, newdata=pre_data)
data_combined <- rbind(
  data.frame(age = data_2$age, DV_amyloid = data_2$DV_amyloid, model = "Observed"),
  data.frame(age = pre_data$age, DV_amyloid = pre_data$amy_linear, model = "Linear"),
  data.frame(age = pre_data$age, DV_amyloid = pre_data$amy_log, model = "Log"),
  data.frame(age = pre_data$age, DV_amyloid = pre_data$amy_poly2, model = "Polynomial"),
  data.frame(age = pre_data$age, DV_amyloid = pre_data$amy_mixed, model = "Mixed")
)

# 绘制图形
ggplot(data_combined, aes(x = age, y = DV_amyloid, color = model, shape = model)) +
  geom_point(size = 1.5) +
  labs(title = "Prediction of DV Amyloid vs Age", x = "Age", y = "DV Amyloid") +
  scale_color_manual(values = c("Observed" = rgb(0.15, 0.6, 0.96, 0.6), 
                                "Linear" = rgb(0.25, 0.8, 0.4, 0.6), 
                                "Log" = rgb(0.55, 0.25, 0.8, 0.6), 
                                "Polynomial" = rgb(0.9, 0.7, 0.1, 0.6), 
                                "Mixed" = rgb(0.9, 0.15, 0.15, 0.6))) +
  theme_minimal()



# Task3
# 读取数据
data_3 <- read.csv("data_task3.csv")
data_3$model <- "Ground Truth"
pre_data <- subset(data_combined, model != "Observed")

# 绘制散点图
ggplot() +
  geom_point(data = pre_data, aes(x = age, y = DV_amyloid, color = model, shape = model), size = 1.5) +
  geom_point(data = data_3, aes(x = age, y = DV_amyloid, color = model), size = 2) +  # 将颜色映射到 "Ground Truth"
  labs(title = "Prediction of DV Amyloid vs Age", x = "Age", y = "DV Amyloid") +
  scale_color_manual(values = c("Linear" = rgb(0.25, 0.8, 0.4, 0.6), 
                                "Log" = rgb(0.55, 0.25, 0.8, 0.6), 
                                "Polynomial" = rgb(0.9, 0.7, 0.1, 0.6), 
                                "Mixed" = rgb(0.9, 0.15, 0.15, 0.6),
                                "Ground Truth" = rgb(0.15, 0.6, 0.96, 0.6))) +
  theme_minimal()


# improve:


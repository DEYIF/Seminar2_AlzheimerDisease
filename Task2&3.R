# Task2
data <- read.csv("data_task2.csv")
library(ggplot2)
library(dplyr)  #for using group_by()
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
lm.linear <- lm(mean_amyloid ~ age)
detach(average_amyloid)
summary(lm.linear)
# coefficients(lm.linear)

# plot prediction
attach(average_amyloid)
plot(age,mean_amyloid)#绘制散点图
abline(lm.linear)#添加拟合直线
par(mfrow=c(2,2))
plot(lm.linear)#绘制回归曲线的图
detach(average_amyloid)

# construct logistic model
attach(average_amyloid)
lm.log <- lm(mean_amyloid ~ age + I(age^2))
detach(average_amyloid)
summary(lm.log)
# coefficients(lm.log)

# plot prediction
attach(average_amyloid)
par(mfrow=c(1,1))
plot(age,mean_amyloid)#绘制散点图
lines(age,fitted(lm.log),col="blue")
par(mfrow=c(2,2))
plot(lm.log)#绘制回归曲线的图
detach(average_amyloid)

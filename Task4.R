# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)      # For ANOVA
library(multcomp) # For pairwise comparisons

# Load the dataset
data <- read.csv("data_task4.csv")

# Remove unnecessary index column
data <- data[, -1]

# Summary statistics for each group
summary_stats <- data %>%
  group_by(GROUP) %>%
  summarise(Mean_AB = mean(AMYLOIDB), SD_AB = sd(AMYLOIDB), 
            Min_AB = min(AMYLOIDB), Max_AB = max(AMYLOIDB))

print(summary_stats)

# 1. 正态性检验
# 使用 Shapiro-Wilk 检验每个组的正态性
shapiro_test_control <- shapiro.test(data$AMYLOIDB[data$GROUP == "control"])
shapiro_test_gene_exp1 <- shapiro.test(data$AMYLOIDB[data$GROUP == "gene_exp1"])
shapiro_test_gene_exp2 <- shapiro.test(data$AMYLOIDB[data$GROUP == "gene_exp2"])

print(shapiro_test_control)
print(shapiro_test_gene_exp1)
print(shapiro_test_gene_exp2)

# 也可以通过 QQ 图来可视化正态性
ggplot(data, aes(sample = AMYLOIDB, color = GROUP)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~GROUP) +
  theme_minimal()

# 对 AMYLOIDB 数据进行对数变换
data$log_AMYLOIDB <- log(data$AMYLOIDB)
data$root_AMYLOIDB <- sqrt(data$AMYLOIDB)

# 对数变换后的正态性检验
shapiro_test_log_control <- shapiro.test(data$log_AMYLOIDB[data$GROUP == "control"])
shapiro_test_log_gene_exp1 <- shapiro.test(data$log_AMYLOIDB[data$GROUP == "gene_exp1"])
shapiro_test_log_gene_exp2 <- shapiro.test(data$log_AMYLOIDB[data$GROUP == "gene_exp2"])

print(shapiro_test_log_control)
print(shapiro_test_log_gene_exp1)
print(shapiro_test_log_gene_exp2)

ggplot(data, aes(sample = log_AMYLOIDB, color = GROUP)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~GROUP) +
  theme_minimal()

# 平方根变换后的正态性检验
shapiro_test_log_control <- shapiro.test(data$root_AMYLOIDB[data$GROUP == "control"])
shapiro_test_log_gene_exp1 <- shapiro.test(data$root_AMYLOIDB[data$GROUP == "gene_exp1"])
shapiro_test_log_gene_exp2 <- shapiro.test(data$root_AMYLOIDB[data$GROUP == "gene_exp2"])

print(shapiro_test_log_control)
print(shapiro_test_log_gene_exp1)
print(shapiro_test_log_gene_exp2)

ggplot(data, aes(sample = root_AMYLOIDB, color = GROUP)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~GROUP) +
  theme_minimal()

# 2. 方差齐性检验
# 使用 Levene's Test 检查方差齐性
levene_test <- leveneTest(root_AMYLOIDB ~ GROUP, data = data)
print(levene_test)

# 如果正态性或方差齐性不满足，可以考虑数据变换（如对数变换）或者使用非参数检验。

# Perform ANOVA
anova_result <- aov(root_AMYLOIDB ~ GROUP, data = data)
summary(anova_result)

# Post-hoc pairwise comparison using Tukey's HSD
posthoc_result <- TukeyHSD(anova_result)
print(posthoc_result)

# Linear regression to check the effects of Age and Years Since Diagnosis
lm_model <- lm(AMYLOIDB ~ AGE + DIAGNOSIS_YRS, data = data)
summary(lm_model)

# Visualize Amyloid Beta distribution by group
ggplot(data, aes(x = GROUP, y = AMYLOIDB, fill = GROUP)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Amyloid Beta Levels by Gene Expression Group", 
       x = "Gene Expression Group", y = "Amyloid Beta (pg/mL)")

# Pairwise comparison plots
plot(posthoc_result)

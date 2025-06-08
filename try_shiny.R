shiny::runGitHub("Dynamic-prediction-of-overall-survival", username = "ccckyx", ref = "main")

install.packages("pec")

library(pROC)
power.roc.test(
  auc = 0.80,           # 预期 AUC（您的目标值）
  null.auc = 0.65,      # 零假设 AUC（文献/临床基准）
  power = 0.85,         # 目标效能（85%）
  alpha = 0.05,         # 显著性水平
  alternative = "one.sided", # 单尾检验（您的研究设定）
  kappa = 1.5,          # 病例/对照比例（需根据队列设计调整）
  ncases = NULL         # 留空以计算所需样本量
)

pmsampsize(type = "b", rsquared = 0.15, 
           parameters = 10, prevalence = 0.3,
           shrinkage = 0.9, cstatistic = 0.75)

print(validation_size)

# 设置参数
Z <- 1.96        # 对应 95% CI
SE <- 0.025      # 可接受的 AUC 标准误差
p_event <- 0.25   # 预期结局发生率（例如 MTX 治疗失败率）

# 样本量计算公式（Riley 2021 for AUC validation）
n_total <- (Z * sqrt(2 * p_event * (1 - p_event)) / SE)^2
n_total <- ceiling(n_total)  # 向上取整

# 计算事件与非事件数
n_event <- ceiling(n_total * p_event)
n_nonevent <- n_total - n_event

# 输出结果
cat("✅ 最小总样本量:", n_total, "\n")
cat("🔸 其中事件数:", n_event, "\n")
cat("🔸 非事件数:", n_nonevent, "\n")

library(pmsampsize)

library(pmsampsize)

size_day7 <- pmsampsize(
  type = "s",
  nagrsquared = 0.25,   # 模型解释度
  parameters = 13,      # 模型参数数
  rate = 0.2,           # 假设整体事件率
  timepoint = 7,        # 第7天预测事件
  meanfup = 40          # 平均随访40天
)

summary(size_day7)

summary(riley_size)



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
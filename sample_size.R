library(pmsampsize)

#── 设定公共信息 ──────────────────────────────────
k   <- 13          # 自由度
r2  <- 0.20        # 假设 Nagelkerke R²

#── Day 1 ─
size_d1 <- pmsampsize(
  type        = "b",
  parameters  = k,
  nagrsquared = r2,
  prevalence  = 0.05      # 1天累计失败率
)

#── Day 4 ─
size_d4 <- pmsampsize(
  type        = "b",
  parameters  = k,
  nagrsquared = r2,
  prevalence  = 0.10      # 4天累计失败率
)

#── Day 7 ─
size_d7 <- pmsampsize(
  type        = "b",
  parameters  = k,
  nagrsquared = r2,
  prevalence  = 0.20      # 7天累计失败率
)

extract_ss <- function(x) {
  s <- summary(x)
  data.frame(
    Samp_size = s$Samp_size["Final"],
    Events    = s$Events["Final"],
    EPP       = s$EPP["Final"],
    row.names = NULL
  )
}

results <- dplyr::bind_rows(
  Day1 = extract_ss(size_d1),
  Day4 = extract_ss(size_d4),
  Day7 = extract_ss(size_d7),
  .id  = "Day"
)
print(results)

d0 <- pmsampsize(type = "s",
                 cstatistic = 0.75,
                 prevalence  = 0.30,
                 parameters  = 10,
                 shrinkage   = 0.90)

# Day 4（假设手术率已下降到 20%）
d4 <- pmsampsize(type = "b",
                 cstatistic = 0.75,
                 prevalence  = 0.20,
                 parameters  = 10,
                 shrinkage   = 0.90)

# Day 7（假设手术率仅 10%）
d7 <- pmsampsize(type = "b",
                 cstatistic = 0.75,
                 prevalence  = 0.10,
                 parameters  = 10,
                 shrinkage   = 0.90)

# 最终结果

library(pmsampsize)

# ---- 先准备必填参数 ----
csR2       <- 0.052   # 换成上一步或你自己算出的值
event_rate <- 0.0067  # 每天手术率 (= 0.25 / 30)
mean_fup   <- 30      # 平均随访天数
t_pred     <- 7       # 预测窗口
p          <- 13      # 自由度

ssurv <- pmsampsize(
  type       = "s",
  csrsquared = csR2,      # 或 nagrsquared =
  parameters = p,
  rate       = event_rate,
  meanfup    = mean_fup,
  timepoint  = t_pred,
  shrinkage  = 0.90
)

print(ssurv)
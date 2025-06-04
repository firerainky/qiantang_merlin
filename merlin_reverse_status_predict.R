library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(merlin)

# 读取数据
df <- read_csv("qiantang_data.csv")
#df <- read_csv("qingyuan_data.csv")

# 把整数日期转换为 Date 类型（注意：必须先转为字符）
df <- df %>%
  mutate(
    across(
      c(time.in.hospital, mtx.date, time1, time2, time3, time4, time5, time6, time7, time8, time9, time10, surgery.date),
      ~ ymd(as.character(.x))
    )
  )

# 确保时间列按顺序排列
time_cols <- paste0("time", 10:1)  # 逆序排列，方便找到“最后一个非 NA 日期”

# 计算 stime（跟踪时间）
df <- df %>%
  mutate(
    # 找到最后一个非 NA 的 time 列的值（按 time10 到 time1 顺序）
    last_time = pmap_dfr(across(all_of(time_cols)), ~ {
      # ..1, ..2, ..., ..10 都是 time10 到 time1 的值
      # 找到第一个非 NA 的时间（从 time10 向 time1 看）
      vals <- list(...)
      tibble(last_time = reduce(vals, function(a, b) if (!is.na(a)) a else b))
    })$last_time,
    
    # 计算 stime，做手术的就是手术 - mtx.date 的天数
    # 不做手术的就是最后一次测量指标 - mtx.date 的天数
    stime = case_when(
      status == 1 ~ as.numeric(surgery.date - mtx.date, units = "days"),
      status == 0 ~ as.numeric(last_time - mtx.date, units = "days"),
      TRUE ~ NA_real_
    )
  )

# 计算矫正后的 menopause_days，就是在使用 mtx 时的停经天数
df <- df %>%
  mutate(
    menopause_days = menopause.days - as.numeric(time.in.hospital - mtx.date)
  )

# 计算 log(hcg) 斜率
df_with_slope <- df %>%
  rowwise() %>%  # 针对每一行（一个个体）逐一执行后续操作
  mutate(
    result = list({  # 返回值用 list 包裹，便于之后展开为多列
      # 1. 收集当前个体的 hcg 值和对应的时间点
      hcg_vals <- c(HCG1, HCG2, HCG3, HCG4, HCG5, HCG6, HCG7, HCG8, HCG9, HCG10)
      time_vals <- c(time1, time2, time3, time4, time5, time6, time7, time8, time9, time10)
      
      # 2. 仅保留非 NA，且测量时间早于或等于 mtx.date 的数据点
      valid_idx <- which(!is.na(hcg_vals) & !is.na(time_vals) & time_vals <= mtx.date)
      hcg_vals <- hcg_vals[valid_idx]
      time_vals <- as.numeric(time_vals[valid_idx] - mtx.date)  # 转换为距离 mtx.date 的天数（负数）
      
      # 3. 如果只有一个测量点，添加一个虚拟的“起点”用于计算斜率
      if (length(hcg_vals) == 1) {
        hcg_vals <- c(0.5, hcg_vals)  # 添加一个 log(hcg) = log(0.5) 的参考点
        time_vals <- c(as.numeric(time.in.hospital - mtx.date) - menopause.days, time_vals)
        # 新增时间点设置为：住院日相对于 mtx.date 的时间差，再减去绝经天数
      }
      
      # 4. 至少两个点才可进行回归，计算 log(hcg) 相对于时间的斜率
      slope <- if (length(hcg_vals) >= 2) {
        coef(lm(log(hcg_vals) ~ time_vals))[2]  # 提取回归系数中的斜率项
      } else {
        NA_real_
      }
      
      # 5. 返回三个变量：斜率、hcg 值数组、时间数组（均为 list 元素）
      list(
        hcg_slope = slope,
        hcg_vals = list(hcg_vals),    # list 包裹以保留为单列，不展开成多行
        time_vals = list(time_vals)
      )
    })
  ) %>%
  unnest_wider(result) %>%  # 将 result 列展开为多个新列：hcg_slope, hcg_vals, time_vals
  dplyr::select(number, stime, hcg_slope)

# 转成长格式
df_long <- df %>%
  pivot_longer(
    cols = c(time1, time2, time3, time4, time5, time6, time7, time8, time9, time10),
    names_to = "time_point",
    values_to = "time"
  ) %>%
  mutate(
    hcg = case_when(
      time_point == "time1"  ~ HCG1,
      time_point == "time2"  ~ HCG2,
      time_point == "time3"  ~ HCG3,
      time_point == "time4"  ~ HCG4,
      time_point == "time5"  ~ HCG5,
      time_point == "time6"  ~ HCG6,
      time_point == "time7"  ~ HCG7,
      time_point == "time8"  ~ HCG8,
      time_point == "time9"  ~ HCG9,
      time_point == "time10" ~ HCG10,
      TRUE ~ NA_real_
    ),
    log_hcg = log(hcg)
  ) %>%
  filter(!is.na(hcg), !is.na(time)) %>%
  dplyr::select(number, age, mtx.date, time, hcg, log_hcg, menopause_days, status, time.in.hospital) 

# 计算长格式中的时间（相对于使用 mtx 的天数）
df_long <- df_long %>%
  mutate(
    time_days = as.numeric(time - mtx.date)
  )

# 把长格式与斜率整合在一起
df_merged <- df_long %>%
  left_join(
    df_with_slope %>% dplyr::select(number, stime, hcg_slope),
    by = "number"
  )

df_merged <- df_merged %>%
  rename(id = number) %>%
  mutate(id = as.numeric(id))

# 从 tbl_df 类型转化为 data.frame
df_merged <- as.data.frame(df_merged)

# status 0 -> 1, 1 -> 0
df_merged$status <- 1 - df_merged$status

# 每个 subject 的 stime 和 status 只有第一行有值（merlin 要求的数据格式）
df_merged$stime[duplicated(df_merged$id)] <- NA
df_merged$status[duplicated(df_merged$id)] <- NA

# 线性回归模型
m1 <- merlin(
  model = log_hcg ~ age + time_days + menopause_days,
  family = "gaussian",
  data = df_merged
)
summary(m1)

# 生存模型
m2 <- merlin(
  model = Surv(stime, status) ~ hcg_slope + age + menopause_days,
  family = "weibull",
  data = df_merged
)
summary(m2)

# 联合模型
m3 <- merlin(
  model = list(
    Surv(stime, status) ~ hcg_slope + menopause_days + M1[id],
    log_hcg ~ age + time_days + menopause_days + M1[id] * 1
  ),
  timevar = c("stime", "time_days"),
  levels = c("id"),
  family = c("weibull", "gaussian"),
  data = df_merged
)
summary(m3)

# 预测特定病人在指定时间点（如30天）的生存概率
# 保留唯一结果（去重）
unique(
  predict(m3,
          stat = "cif",
          type = "marginal",
          predmodel = 1,
          at = c("hcg_slope" = 0.056529902))
)

df_predict <- df_merged %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()
df_predict$surv_prob <- pmap_dbl(
  list(
    hcg_slope = df_predict$hcg_slope,
    stime = df_predict$stime
  ),
  function(hcg_slope, stime) {
    pred <- predict(
      m3,
      stat = "cif",
      type = "marginal",
      predmodel = 1,
      at = list(
        hcg_slope = hcg_slope,
        stime = stime
      )
    )
    pred[1]  # 返回第一个（也是唯一）结果
  }
)

library(pROC)

roc_obj <- roc(response = df_predict$status, predictor = df_predict$surv_prob)
plot(roc_obj, main = "ROC Curve for 30-day Recovery Prediction")
auc(roc_obj)  # 返回 AUC（0.5~1之间，越高越好）
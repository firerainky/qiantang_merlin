# 1. 加载必要的库
library(tidyverse)
library(lubridate)

# 2. 读取数据
df <- read_csv("qiantang_data.csv")

# 3. 把整数日期转换为 Date 类型（注意：必须先转为字符）
df <- df %>%
  mutate(
    across(
      c(time.in.hospital, mtx.date, time1, time2, time3, time4, time5, time6, time7, time8, time9, time10, time11, time12, time13, time14, surgery.date),
      ~ ymd(as.character(.x))
    )
  )

# 确保时间列按顺序排列
time_cols <- paste0("time", 10:1)  # 逆序排列，方便找到“最后一个非 NA 日期”

df <- df %>%
  mutate(
    # 找到最后一个非 NA 的 time 列的值（按 time14 到 time1 顺序）
    last_time = pmap_dfr(across(all_of(time_cols)), ~ {
      # ..1, ..2, ..., ..14 都是 time14 到 time1 的值
      # 找到第一个非 NA 的时间（从 time14 向 time1 看）
      vals <- list(...)
      tibble(last_time = reduce(vals, function(a, b) if (!is.na(a)) a else b))
    })$last_time,
    
    # 计算 stime
    stime = case_when(
      status == 1 ~ as.numeric(surgery.date - mtx.date, units = "days"),
      status == 0 ~ as.numeric(last_time - mtx.date, units = "days"),
      TRUE ~ NA_real_
    )
  )

# Step 1: 转成长格式
df_long <- df %>%
  pivot_longer(
    cols = starts_with("time"),
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
    )
  ) %>%
  filter(!is.na(hcg), !is.na(time))

# Step 2: 找出每个样本 mtx.date 前最近的 HCG
df_base_hcg <- df_long %>%
  filter(time <= mtx.date) %>%
  group_by(number) %>%
  slice_max(time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(number, base_hcg = hcg)

# Step 3: 合并回原始数据
df <- df %>%
  left_join(df_base_hcg, by = "number")

# 5. 计算矫正后的 menopause.days
df <- df %>%
  mutate(
    menopause_days = menopause.days - as.numeric(time.in.hospital - mtx.date)
  )

library(dplyr)
library(tidyr)

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
  unnest_wider(result)  # 将 result 列展开为多个新列：hcg_slope, hcg_vals, time_vals

# 6. 最终数据集，适用于 merlin 模型
df_merlin <- df_with_slope %>%
  select(number, age, menopause_days, stime, status, base_hcg, hcg_slope) %>%
  rename(id = number)

df_merlin <- df_merlin %>%
  mutate(id = as.numeric(id))

# 从 tbl_df 类型转化为 data.frame
df_merlin <- as.data.frame(df_merlin)

df_merlin %>% filter(stime <= 0)

summary(df_merlin$stime)
any(is.na(df_merlin$stime))     # 检查 NA
any(df_merlin$stime <= 0)       # 检查非正数

summary(df_merlin$hcg_slope)
any(is.na(df_merlin$hcg_slope))
any(is.infinite(df_merlin$hcg_slope))
any(is.nan(df_merlin$hcg_slope))

df_missing_hcg <- df_merlin %>% filter(is.na(hcg_slope))

df_merlin_clean <- df_merlin %>%
  filter(!is.na(stime), stime > 0, !is.na(status), !is.na(base_hcg), !is.na(log_hcg_slope))

df_merlin <- df_merlin %>%
  mutate(log_base_hcg = log(base_hcg + 1))

library(merlin)

# log_base_hcg over menopause_days?
# + slope before MTX.date
m1 <- merlin(
  model = Surv(stime, status) ~ hcg_slope + age + menopause_days + log_base_hcg,
  family = "weibull",
  data = df_merlin
)

summary(m1)

summary(df_merlin_clean$base_hcg)
sum(is.na(df_merlin_clean$base_hcg))
sum(is.infinite(df_merlin_clean$base_hcg))

# TODO 把 log_hcg 模型 m1
# TODO 治疗前 hcg 作为 base_hcg, hcg ~ base_hcg + age + menopause_days + time
# time 是 MTX 治疗天数
# 构建时间-生存模型需要用到治疗前 hcg 作为 base_hcg

df_merlin <- df_merlin %>%
  mutate(
    log_hcg = log(hcg + 1),
    age_z = scale(age),
    time_z = scale(time),
    menopause_z = scale(menopause_days)
  )

m2 <- merlin(
  model = log_hcg ~ age_z + time_z + I(time_z^2) + menopause_z + M1[id],
  family = "gaussian",
  data = df_merlin
)
summary(m2)

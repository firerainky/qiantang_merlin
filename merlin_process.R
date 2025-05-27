# 1. 加载必要的库
library(tidyverse)
library(lubridate)

# 2. 读取数据
df <- read_csv("qiantang_data.csv")

# 3. 把整数日期转换为 Date 类型（注意：必须先转为字符）
df <- df %>%
  mutate(
    across(
      c(time.in.hospital, mtx.date, time1, time2, time3, time4, time5, time6, time7, time8, time9, time10),
      ~ ymd(as.character(.x))
    )
  )

# 对 HCG1 ~ HCG10 做“每个样本一行”的标准化
df_z <- df
df_z[ , paste0("HCG", 1:10)] <- t(scale(t(df[ , paste0("HCG", 1:10)])))

# 4. 创建长格式数据（包含 time1 ~ time10）
df_long <- df_z %>%
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
    )
  ) %>%
  select(number, age, mtx.date, time, hcg, menopause.days, time.in.hospital) %>%
  filter(!is.na(time), !is.na(hcg))  # 去除缺失值

# 5. 计算矫正后的 time 和 menopause.days
df_long <- df_long %>%
  mutate(
    time_days = as.numeric(time - mtx.date),
    menopause_days = menopause.days - as.numeric(time.in.hospital - mtx.date)
  )

# 6. 最终数据集，适用于 merlin 模型
df_merlin <- df_long %>%
  select(number, age, time_days, hcg, menopause_days) %>%
  rename(time = time_days) %>%
  rename(id = number)

df_merlin <- df_merlin %>%
  mutate(id = as.numeric(id))

# 从 tbl_df 类型转化为 data.frame
df_merlin <- as.data.frame(df_merlin)

# 对 hcg 取对数
df_merlin <- df_merlin %>%
  mutate(
    log_hcg = log(hcg + 1)
  )

library(merlin)

m1 <- merlin(
  # model = menopause_days ~ age,
  model = hcg ~ age + menopause_days + time,
  family = "gaussian",
  data = df_merlin
)

summary(m1)

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

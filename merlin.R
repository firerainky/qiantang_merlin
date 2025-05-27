# 1. 加载必要的库
library(tidyverse)
library(lubridate)

# 2. 读取数据
df <- read_csv("qiantang_data.csv")

hcg_cols <- paste0("HCG", 1:10)

# 找出不在正常范围 [100, 80000] 的 HCG 值
abnormal_hcg <- df %>%
  select(number, all_of(hcg_cols)) %>%
  pivot_longer(cols = all_of(hcg_cols), names_to = "hcg_var", values_to = "hcg_value") %>%
  filter(!is.na(hcg_value) & (hcg_value <= 0.4 | hcg_value > 80000))

# 查看异常值
print(abnormal_hcg)

cols_to_check <- c("time.in.hospital", "mtx.date", paste0("time", 1:10))

for (col in cols_to_check) {
  message("Checking ", col)
  failed <- df %>%
    mutate(tmp_date = ymd(as.character(.data[[col]]))) %>%
    filter(!is.na(.data[[col]]) & is.na(tmp_date)) %>%
    select(all_of(col))
  
  if (nrow(failed) > 0) {
    print(failed)
  } else {
    message("✅ All values in ", col, " parsed successfully.\n")
  }
}


# 3. 把整数日期转换为 Date 类型（注意：必须先转为字符）
df <- df %>%
  mutate(
    across(
      c(time.in.hospital, mtx.date, time1, time2, time3, time4, time5, time6, time7, time8, time9, time10),
      ~ ymd(as.character(.x))
    )
  )

# 4. 创建长格式数据（包含 time1 ~ time10）
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

# 查看前几行结果确认
head(df_merlin)

# 从 tbl_df -> data.frame
df_merlin <- as.data.frame(df_merlin)
str(df_merlin$hcg)

library(merlin)

m1 <- merlin(
  # model = menopause_days ~ age,
  model = hcg ~ age + time + menopause_days,
  family = "gaussian",
  data = df_merlin
)

summary(m1)

data(pbc.merlin, package = "merlin")

# Linear fixed-effects model
m2 <- merlin(logb ~ year,
       family = "gaussian",
       data = pbc.merlin)
library("joineRML")
data(heart.valve, package = "joineRML")
heart.valve <- heart.valve[!is.na(heart.valve$grad), ]

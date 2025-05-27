library(dplyr)
library(tidyr)
library(lubridate)

extract_base_hcg <- function(df) {
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
        time_point == "time11" ~ HCG11,
        time_point == "time12" ~ HCG12,
        time_point == "time13" ~ HCG13,
        time_point == "time14" ~ HCG14,
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
  df_out <- df %>%
    left_join(df_base_hcg, by = "number")
  
  return(df_out)
}

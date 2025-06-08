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
    p = case_when(
      time_point == "time1"  ~ P1,
      time_point == "time2"  ~ P2,
      time_point == "time3"  ~ P3,
      time_point == "time4"  ~ P4,
      time_point == "time5"  ~ P5,
      time_point == "time6"  ~ P6,
      time_point == "time7"  ~ P7,
      time_point == "time8"  ~ P8,
      time_point == "time9"  ~ P9,
      time_point == "time10" ~ P10,
      TRUE ~ NA_real_
    ),
    log_hcg = log(hcg)
  ) %>%
  filter(!is.na(hcg), !is.na(time)) %>%
  #filter(!is.na(hcg), !is.na(time), !is.na(p)) %>%   # <- 多加这一项
  dplyr::select(number, age, mtx.date, time, hcg, log_hcg, p, menopause_days, status, time.in.hospital) 

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
# df_merged$status <- 1 - df_merged$status

# 每个 subject 的 stime 和 status 只有第一行有值（merlin 要求的数据格式）
df_merged$stime[duplicated(df_merged$id)] <- NA
df_merged$status[duplicated(df_merged$id)] <- NA

mod <- merlin(
  model = list(
    log_hcg ~ time_days + menopause_days + hcg_slope + M1[id] * time_days,     # 生物标志物：固定效应 time，加随机截距M1和随机斜率M1*time
    status ~ EV[log_hcg]              # 成功率：以 bio 的当前期望值 EV[bio] 作为协变量
  ),
  family = c("gaussian", "bernoulli"),  # 分别指定子模型的分布
  # link   = c("identity", "logit"),      # 链接函数：连续模型默认identity，二分类模型logit
  levels = "id",                        # 指定随机效应的层级
  timevar = c("time_days", "time_days"),          # 指定各子模型的时间变量（此处均为 time 列）
  data = df_merged                         # 包含 id、time、bio、success 等列的数据框
)

mod <- merlin(
  model = list(
    # 纵向子模型：固定效应 + 随机截距 + 随机斜率 + 两个新协变量
    log_hcg ~ time_days
      + hcg_slope
      + menopause_days
      + M1[id]*1,      # 随机斜率
    # p ~ time_days + menopause_days + M1[id]*1,
    
    # 二分类子模型：当前值 + 两个新协变量
    status  ~ EV[log_hcg]
  ),
  family  = c("gaussian", "bernoulli"),
  levels  = "id",
  timevar = c("time_days", "time_days"),
  data    = df_merged
)

summary(mod)

# 行数
nrow(df_merged)
# 查看这两列的缺失情况
sum(is.na(df_merged$hcg_slope))
sum(is.na(df_merged$menopause_days))
str(df_merged$hcg_slope)
str(df_merged$menopause_days)

results <- data.frame()
time_points <- c(0, 4, 7)

for (t in time_points) {
  # 子集：只包含 time_days <= t 的观测（历史）
  newdata_t <- subset(df_merged, time_days <= t)
  
  # 获取当前的 id（有观测记录的）
  subject_ids <- unique(newdata_t$id)
  
  # 预测（返回每个 id 一个概率）
  pred_vals <- predict(mod,
                       stat = "mu",
                       predmodel = 2,
                       type = "marginal",
                       newdata = newdata_t)
  
  # 拼接结果
  results_t <- data.frame(id = subject_ids,
                          time_days = t,
                          predicted_prob = pred_vals)
  
  results <- rbind(results, results_t)
}

# 合并真实 status
df_status <- df_merged[!is.na(df_merged$status), c("id", "status")]
results <- merge(results, df_status, by = "id")

results$pred_class <- ifelse(results$predicted_prob >= 0.5, 1, 0)

library(dplyr)

results %>%
  group_by(time_days) %>%
  summarise(
    Accuracy = mean(pred_class == status),
    N = n()
  )

library(pROC)

for (t in unique(results$time_days)) {
  temp <- results %>% filter(time_days == t)
  roc_obj <- roc(temp$status, temp$predicted_prob)
  auc_val <- auc(roc_obj)
  cat("第", t, "天预测 AUC = ", round(auc_val, 3), "\n")
}

# 预测时间点（天）
time_points <- c(0, 4, 7)
# 存储每个时间点的预测概率
predictions <- list()

for(t in time_points){
  # 构造历史子数据，只保留 time <= t 的观测
  newdata_t <- subset(df_merged, time_days <= t)
  
  # 调用 merlin predict：predmodel=2 表示针对第二个模型(status)
  pred_vals <- predict(mod,
                       stat = "mu",
                       predmodel = 2,
                       type = "marginal",
                       newdata = newdata_t)
  
  # 将预测值按 id 分组，每个 id 取最后一条记录的概率
  # 假设 newdata_t 中按 id 排序
  newdata_t$pred_status <- pred_vals
  last_pred <- aggregate(pred_status ~ id, data = newdata_t,
                         FUN = function(x) tail(x, 1))
  
  # 记录结果
  colnames(last_pred)[2] <- paste0("P_status_t", t)
  if(t == time_points[1]){
    predictions_df <- last_pred
  } else {
    predictions_df <- merge(predictions_df, last_pred, by = "id", all = TRUE)
  }
}

# predictions_df 中即包含每个个体在不同时间点的预测成功概率
print(predictions_df)


library(dplyr)

# 筛出所有有 status（即结局）的行，用于对比
df_status <- df_merged %>% filter(!is.na(status)) %>% select(id, status)

# 创建预测结果框架
results <- data.frame()

# 设置预测时间点
times <- c(0, 4, 7)

pred <- predict(mod,
                stat = "mu",
                predmodel = 2,
                type = "marginal")

predict(mod, stat="mu", type="marginal", predmodel=2)

for (t in times) {
  # 用 full dataset + at=time 实现基于历史信息的预测
  pred <- predict(mod,
                  stat = "mu",
                  predmodel = 2,
                  type = "marginal",
                  at = list(time_days = t),
                  newdata = df_merged)
  
  # 保留每个个体在该时间点的预测值（注意 predict 会按行顺序重复）
  ids <- unique(df_merged$id)
  pred_t <- data.frame(id = ids, time_days = t, predicted_prob = pred[!duplicated(df_merged$id)])
  
  results <- bind_rows(results, pred_t)
}



# 从原始数据中筛出每位个体的 status 行（一般只有一个）
df_status <- df_merged[!is.na(df_merged$status), ]

# 预测每位个体在他 status 那一行（对应时间）下的成功概率
pred_prob <- predict(mod, stat = "mu", predmodel = 2,
                     type = "marginal", newdata = df_status)

# 添加预测值列
df_status$pred_prob <- pred_prob

df_status$pred_class <- ifelse(df_status$pred_prob >= 0.5, 1, 0)

mean(df_status$pred_class == df_status$status)  # 准确率（Accuracy）

library(pROC)

roc_obj <- roc(df_status$status, df_status$pred_prob)
auc(roc_obj)  # 输出 AUC 值
plot(roc_obj) # ROC 曲线

pred0 <- predict(mod, stat="mu", type="marginal", predmodel = 2, at=list(time_days=0))
pred4 <- predict(mod, stat="mu", type="marginal", predmodel = 2, at=list(time_days=4))
pred7 <- predict(mod, stat="mu", type="marginal", predmodel = 2, at=list(time_days=7))

subject_ids <- unique(df_merged$id)
# 定义要预测的时间点
times <- c(0, 4, 7)

# 对每个时间点进行循环，计算所有个体的预测概率
for (t in times) {
  # 构造一个新数据集，每行对应一个个体在时间 t 的情形
  newdata <- data.frame(id = subject_ids, time_days = t)
  # 预测概率：stat="mu" 表示响应的期望值（概率），predmodel=2 表示第二个子模型（二分类）
  pred_prob <- predict(mod, stat = "mu", predmodel = 2,
                       type = "marginal", newdata = newdata)
  # 将预测结果与新数据合并
  newdata$predicted_prob <- pred_prob
  cat("时间 =", t, "预测成功概率：\n")
  print(newdata)
}

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
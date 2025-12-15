# 套件載入
library(dplyr)
library(readr)
library(lubridate)
library(zoo)
library(stabledist)
library(fBasics)
setwd("/Users/johnnyjheng/Documents/MSCA & ASE/04 Crypto Currency Returns/CC returns/Code/20250324 CC Genus Proximum")

# 載入報酬資料
dd <- read_csv("Data/DD price.csv")
dd <- as.data.frame(dd)
rownames(dd) <- dd[[1]]
dd <- dd[, -1]
dd <- as.data.frame(dd)
dd <- na.locf(dd)  # 若有需要前向填補缺失值

# 計算 log return
dd_return <- log(dd / dplyr::lag(dd, 1))
dd_return <- dd_return[-1, ]

# 移除全為 NA 或出現 0 值超過 200 天的資產
dd_return <- dd_return[, colSums(dd_return == 0, na.rm = TRUE) <= 200]

dd_index <- read_csv("Data/DD Index.csv")

# 計算 rolling window 起始參數
start_window <- floor(nrow(dd_return) / 3)
window_size <- floor(nrow(dd_return) / 3)
step_size <- 21
start <- 0

# 擷取子視窗
dd_window <- dd_return[(start + 1):(start + window_size), ]
dd_date <- tail(rownames(dd_window), 1)

# 清理視窗內資料
dd_window <- dd_window[, colSums(is.na(dd_window)) == 0]  # 無缺值
dd_window <- dd_window[, colSums(dd_window == 0, na.rm = TRUE) <= (window_size / 10)]  # 零值過多排除
dd_window <- dd_window[, apply(dd_window, 2, sd, na.rm = TRUE) != 0]  # 常數欄排除

# 結合 dd_index
dd_factors <- data.frame(
  Date = rep(dd_date, length(colnames(dd_window))),
  Asset = colnames(dd_window),
  stringsAsFactors = FALSE
)
dd_factors <- left_join(dd_factors, dd_index, by = "Asset")

# 擷取單一資產測試資料
test <- dd_window[, "bitcoin-plus", drop = TRUE]
test <- na.omit(test)


# 執行穩定分布的最大概似估計（Maximum Likelihood）
fit <- stableFit(test)

# 查看完整摘要
summary(fit)

# 擷取 alpha 和 gamma
alpha <- fit@fit$estimate["alpha"]
gamma <- fit@fit$estimate["gamma"]

# 顯示結果
cat("Estimated alpha =", alpha, "\n")
cat("Estimated gamma =", gamma, "\n")

# ggplotのれんしゅう
# MFから持ってきたcsvでグラフを書く

library(dplyr)
library(tidyr)
library(data.table)
library(readxl)
library(ggplot2)
library(lubridate)
library(stringr)

# csv読み込み
files <- list.files(path="./csv", pattern="*.csv", full.names=T)

myreadcsv <- function(file) {
  tmp <- read.csv(file, fileEncoding="cp932") %>%
    rename_with(~str_remove_all(., "\\..+\\."))
  colnames(tmp)[3] <- "現預金"  # 途中で列名が変わっているので
  return(tmp)
}

data <- sapply(files, myreadcsv) %>%
  bind_rows() %>%
  replace_na(list(FX = 0)) %>%
  mutate(日付 = as.Date(日付)) %>%
  mutate(現預金 = 現預金 + FX + ポイント) %>%
  select(-c(FX, ポイント)) %>%
  pivot_longer(-c(日付, 合計), names_to="type", values_to="value") %>%
  mutate(rate = value / 合計) %>%
  mutate(type = factor(type, levels=c("株式", "投資信託", "現預金")))  # 描画の順序制御用

# 面グラフ
g <- ggplot(data=data) +
  geom_area(mapping = aes(x = 日付, y = value, group = type, colour = type, fill = type)) +
  scale_y_continuous(label = scales::comma) +  # y軸をカンマ区切りに
  # scale_x_date(date_breaks = "30 day", date_labels = "%Y/%m/%d") +  # x軸の目盛を減らす
  scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m/%d") +  # x軸の目盛を減らす
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 目盛を斜めに
plot(g)

# 面グラフ（割合）
g2 <- ggplot(data=data) +
  geom_area(mapping = aes(x = 日付, y = rate, group = type, colour = type, fill = type)) +
  scale_y_continuous(label = scales::percent) +  # y軸をパーセント表示に
  scale_x_date(date_breaks = "1 month", date_labels = "%Y/%m/%d") +  # x軸の目盛を減らす
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 目盛を斜めに
plot(g2)

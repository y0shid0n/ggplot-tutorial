# irisでggplotのれんしゅう

library(dplyr)
library(ggplot2)
library(GGally)

# 散布図
g <- ggplot(data = iris) +
  geom_point(mapping = aes(x=Petal.Length, y=Sepal.Length, group=Species, colour=Species))
plot(g)

# 対散布図はこれが手っ取り早い
g <- ggpairs(data = iris, mapping = aes(group=Species, colour=Species))
g

# バイオリン図
g <- ggplot(data = iris, mapping = aes(x=Species, y=Sepal.Length, group=Species, colour=Species)) +
  geom_violin() +
  geom_point(position = position_jitter(width=0.1, height=0, seed=72))  # 点をずらして全て表示できるようにする
  # geom_jitter(position = position_jitter(width=0.1, height=0, seed=72))
plot(g)

# 棒グラフ（mean±SE）
g <- ggplot(data = iris, mapping = aes(x=Species, y=Sepal.Length, group=Species)) +
  theme_classic() +  # 背景を白のみにする
  stat_summary(geom="bar", fun="mean", fill="grey") +
  stat_summary(geom="pointrange", fun.data="mean_se") +
  coord_flip() +  # X軸とY軸を入れ替え
  labs(title = "Sepal"
      , subtitle = "Length"
      , caption = "iris"
      , x = "Species"
      , y = "length (cm)")
plot(g)

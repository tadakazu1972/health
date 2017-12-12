#主成分分析

#ファイル読み込み
data <- read.csv("csv/deathH25.csv")

#区名を項目名に置き換え
row.names(data) = data[,1]
data = data[,2:17]

#描画２種類
par(family="HiraKakuProN-W3")
biplot(prcomp(data))
biplot(prcomp(data, scale=TRUE))
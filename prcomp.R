#主成分分析
#https://oku.edu.mie-u.ac.jp/~okumura/stat/pca.html
#http://www.statsbeginner.net/entry/2014/07/27/121214


#ファイル読み込み
data <- read.csv("csv/deathH25.csv")

#区名を項目名に置き換え
row.names(data) = data[,1]
data = data[,2:17]

#描画 ２種類
par(family="HiraKakuProN-W3")
biplot(prcomp(data, scale=TRUE))

#主成分にかかる固有ベクトルがわかる
result <- prcomp(data, scale=TRUE)
print(result)

#寄与率と累積寄与率
summary(result)

#主成分得点を見る
result$x
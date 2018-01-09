#年齢調整死亡率　主成分分析

#ライブラリ
library(dplyr)

#作業ディレクトリ変更
setwd("~/Documents/health")

#ファイル読み込み
data <- read.csv("csv/deathH25Male.csv")

#区名を項目名に置き換え
row.names(data) = data[,1]

#dataの範囲を区名以外の数値のみにする
data <- data[,2:8]

#まずはbiplotで主成分分析を可視化
par(family="HiraKakuProN-W3", xpd=TRUE)
biplot(prcomp(data, scale=TRUE), scale=1)

#主成分分析を計算
pca <- prcomp(data, scale=TRUE)

#寄与率と累積寄与率
summary(pca)

#主成分得点を見る
pca$x

#################################
# plotで描画する場合

#plotで設定値をつくるため町丁目のx座標とy座標のmax-minを確認
#それぞれ大きい数値をplotで採用
#x座標
min(pca$x[,1])
max(pca$x[,1])

#y座標
min(pca$x[,2])
max(pca$x[,2])

#ライブラリ
library(maptools)

#描画
par(las=1, family="HiraKakuProN-W3")
plot(x=NULL, type="n", xlab="PC1", ylab="PC2", xlim=c(-10,10), ylim=c(-5,5), xaxs="i", yaxs="i", xaxt="n", yaxt="n", bty="n", main="大阪市　年齢調整死亡率（主な死因）平成25年　男性　主成分分析")
axis(side=1, at=seq(-10,10,2), tck=1.0, lty="dotted", lwd=0.5, col="#dddddd", labels=expression(-10,-8,-6,-4,-2,0,2,4,6,8,10))
axis(side=2, at=seq(-5,5,1), tck=1.0, lty="dotted", lwd=0.5, col="#dddddd", labels=expression(-5,-4,-3,-2,-1,0,1,2,3,4,5))
points(x=pca$x[,1], y=pca$x[,2], pch=16, col="#ff8c00")
pointLabel(x=pca$x[,1], y=pca$x[,2], labels=rownames(data), cex=0.5)
box(bty="l")
grid()

#################
#因子負荷量のプロット
#x座標
min(pca$rotation[,1])
max(pca$rotation[,1])

#y座標
min(pca$rotation[,2])
max(pca$rotation[,2])

#描画
par(las=1, family="HiraKakuProN-W3")
plot(x=NULL, type="n", xlab="PC1", ylab="PC2", xlim=c(-0.5,0.5), ylim=c(-0.5,0.5), xaxs="i", yaxs="i", xaxt="n", yaxt="n", bty="n", main="大阪市　年齢調整死亡率（主な死因）平成25年 男性　主成分分析")
axis(side=1, at=seq(-0.5,0.5,0.1), tck=1.0, lty="dotted", lwd=0.5, col="#dddddd", labels=expression(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5))
axis(side=2, at=seq(-0.5,0.5,0.1), tck=1.0, lty="dotted", lwd=0.5, col="#dddddd", labels=expression(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5))
for(i in 1:7){
  arrows(0,0,pca$rotation[i,1],pca$rotation[i,2], col=2, length=0.1)
}
pointLabel(x=pca$rotation[,1], y=pca$rotation[,2], labels=rownames(pca$rotation), cex=0.5)
box(bty="l")
grid()

#年齢調整死亡率　主成分分析

#ライブラリ
library(dplyr)

#作業ディレクトリ変更
setwd("~/Documents/health")

#ファイル読み込み
data <- read.csv("csv/deathH25.csv")

#区名を項目名に置き換え
row.names(data) = data[,1]

#dataの範囲を区名以外の数値のみにする
data <- data[,2:15]

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
plot(x=NULL, type="n", xlab="PC1", ylab="PC2", xlim=c(-10,10), ylim=c(-5,5), xaxs="i", yaxs="i", xaxt="n", yaxt="n", bty="n", main="大阪市　年齢調整死亡率（主な死因）平成25年　主成分分析")
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
plot(x=NULL, type="n", xlab="PC1", ylab="PC2", xlim=c(-0.5,0.5), ylim=c(-0.5,0.5), xaxs="i", yaxs="i", xaxt="n", yaxt="n", bty="n", main="大阪市　年齢調整死亡率（主な死因）平成25年　主成分分析")
axis(side=1, at=seq(-0.5,0.5,0.1), tck=1.0, lty="dotted", lwd=0.5, col="#dddddd", labels=expression(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5))
axis(side=2, at=seq(-0.5,0.5,0.1), tck=1.0, lty="dotted", lwd=0.5, col="#dddddd", labels=expression(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5))
for(i in 1:14){
  arrows(0,0,pca$rotation[i,1],pca$rotation[i,2], col=2, length=0.1)
}
pointLabel(x=pca$rotation[,1], y=pca$rotation[,2], labels=rownames(pca$rotation), cex=0.5)
box(bty="l")
grid()

#################################
#単年度で、町丁目ごとに年齢構成をファイルに書き出し -> 主成分分析と照らし合わせる
#町丁目名はいつものnamesではなくて、上で処理したrow.names(data)を使う
for(i in 1:104){
 quartz(type="pdf", file=sprintf("sumiyoshiJyukiH2909_%d%s.pdf",i,row.names(data)[i]))
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 plot(c(0:100), data[i,1:101], type="l", col=c(i), xlim=c(0, 100), ylim=c(0, 100), main="住吉区　住民基本台帳　各年齢別　人口　平成29年9月現在", xlab="年齢", ylab="人")
 text(50+0.55, 80, labels=row.names(data)[i], cex=1.5)
 par(xaxt="s")
 axis(side=1, at=0:100, labels=c(0:100))
 dev.off()
}



#参考
#################################
# ggbiplotで描画する場合

#パッケージのインストール
install.packages("devtools")
devtools::install_github("vqv/ggbiplot")

#ライブラリ
library("ggbiplot")

#主成分分析を計算
pca <- prcomp(data, scale=TRUE)

#描画 choices:プロットする主成分
#暫定措置：ggbiplotでラベルを設定しているが文字化け回避できていないため、0.1でとりあえず回避
ggbiplot(pcobj=pca, choices=1:2, obs.scale=1, var.scale=1, labels=rownames(data), labels.size=0.1, var.axes=FALSE) + geom_text(aes(label=rownames(data)), size=2, family="HiraKakuProN-W3")

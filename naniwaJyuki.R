#浪速区

###################################################
#住民基本台帳人口　町丁目　各年齢別
#前処理1：各ファイル最後の総数３行を削除
#前処理2：平成27年3月まで最後の項目「不詳」を「不明」に修正
#前処理3：平成28年3月から男女別の項目「計」を「総」に修正 -> 62個が置換されればOK
#ライブラリ
library(dplyr)

setwd("~/Documents/浪速区/")
lf <- list.files(path="~/Documents/浪速区/jyuki", full.names=T)
data <- lapply(lf, read.csv)
data_bind <- do.call(rbind, data)

#男女別が「総」のデータだけ抽出
total <- data_bind %>% filter(data_bind$男女別=="総")

name <- total[1:62,2]

#全項目
for(i in 1:62){
 p <- total %>% filter(total$町丁目名==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,8:108]), col=c(1:100), xlim=c(1, 14), ylim=c(0, 150), main="浪速区　住民基本台帳　各年齢別　人口", xlab="住民基本台帳", ylab="人")
 par(xaxt="s")
 axis(side=1, at=1:14, labels=c("H23/3", "H23/9", "H24/3", "H24/9", "H25/3", "H25/9", "H26/3", "H26/9", "H27/3", "H27/9", "H28/3", "H28/9", "H29/3", "H29/9"))
}

#シングル項目　ラベル付き
for(i in 1:62){
 p <- total %>% filter(total$町丁目名==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,8]), col=c(2), xlim=c(1, 14), ylim=c(0, 150), main=paste("浪速区  ", colnames(total)[8], sep= ""), xlab="住民基本台帳", ylab="人")
 text(14+0.55, p[14,8], labels=name[i], cex=0.5)
 par(xaxt="s")
 axis(side=1, at=1:14, labels=c("H23/3", "H23/9", "H24/3", "H24/9", "H25/3", "H25/9", "H26/3", "H26/9", "H27/3", "H27/9", "H28/3", "H28/9", "H29/3", "H29/9"))
}

#総数　各項目ごとにファイルに書き出し
#町丁目ごとに色を変更する
for(j in 8:108){
  quartz(type="pdf", file=sprintf("naniwaJyukiH2303_H2909_%d.pdf",j-8))
  for(i in 1:62){
    p <- total %>% filter(total$町丁目名==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	ts.plot(ts(p[,j]), col=c(i), xlim=c(1, 14), ylim=c(0, 100), main=paste("浪速区  ", colnames(data_bind)[j], sep= ""), xlab="住民基本台帳", ylab="人")
	text(14+0.55, p[14,j], labels=name[i], cex=0.5)
	par(xaxt="s")
	axis(side=1, at=1:14, labels=c("H23/3", "H23/9", "H24/3", "H24/9", "H25/3", "H25/9", "H26/3", "H26/9", "H27/3", "H27/9", "H28/3", "H28/9", "H29/3", "H29/9"))
 }
 dev.off()
}

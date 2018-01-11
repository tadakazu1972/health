#住吉区

###################################################
#住民基本台帳人口　町丁目　各年齢別
#前処理1：平成27年3月以降は後部の無駄な列を削除
#前処理2：Xを0に置換
#前処理3：平成27年3月まで最後の項目「不詳」を「不明」に修正
#前処理4：平成27年9月まで男女別の項目「総,」を「計」に置換
#ライブラリ
library(dplyr)

setwd("~/Documents/住吉区/")
lf <- list.files(path="~/Documents/住吉区/jyuki", full.names=T)
data <- lapply(lf, read.csv)
data_bind <- do.call(rbind, data)

#男女別が「計」のデータだけ抽出
total <- data_bind %>% filter(data_bind$男女別=="計")

#最後が総数のため全行を14ファイル分で割って-1にして読み込まないようにする
last <- length(total$町丁目)/14-1
name <- total[1:last,2]

#国勢調査データで使うために、町丁目データをcsvファイルに書き出しておく
write.csv(name, "NAME.csv")

#全項目　一枚描き
for(i in 1:last){
 p <- total %>% filter(total$町丁目名==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,8:108]), col=c(1:100), xlim=c(1, 14), ylim=c(0, 150), main="住吉区　住民基本台帳　各年齢別　人口", xlab="住民基本台帳", ylab="人")
 par(xaxt="s")
 axis(side=1, at=1:14, labels=c("H23/3", "H23/9", "H24/3", "H24/9", "H25/3", "H25/9", "H26/3", "H26/9", "H27/3", "H27/9", "H28/3", "H28/9", "H29/3", "H29/9"))
}

#シングル項目　ラベル付き
for(i in 1:last){
 p <- total %>% filter(total$町丁目名==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,8]), col=c(2), xlim=c(1, 14), ylim=c(0, 150), main=paste("住吉区  ", colnames(total)[8], sep= ""), xlab="住民基本台帳", ylab="人")
 text(14+0.55, p[14,8], labels=name[i], cex=0.5)
 par(xaxt="s")
 axis(side=1, at=1:14, labels=c("H23/3", "H23/9", "H24/3", "H24/9", "H25/3", "H25/9", "H26/3", "H26/9", "H27/3", "H27/9", "H28/3", "H28/9", "H29/3", "H29/9"))
}

#総数　0歳〜100歳以上まで各年齢別にファイルに書き出し
#町丁目ごとに色を変更する
for(j in 8:108){
  quartz(type="pdf", file=sprintf("sumiyoshiJyukiH2303_H2909_%d.pdf",j-8))
  for(i in 1:last){
    p <- total %>% filter(total$町丁目名==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	ts.plot(ts(p[,j]), col=c(i), xlim=c(1, 14), ylim=c(0, 100), main=paste("住吉区  ", colnames(data_bind)[j], sep= ""), xlab="住民基本台帳", ylab="人")
	text(14+0.55, p[14,j], labels=name[i], cex=0.5)
	par(xaxt="s")
	axis(side=1, at=1:14, labels=c("H23/3", "H23/9", "H24/3", "H24/9", "H25/3", "H25/9", "H26/3", "H26/9", "H27/3", "H27/9", "H28/3", "H28/9", "H29/3", "H29/9"))
 }
 dev.off()
}

###############################################
# GIFアニメ用 pngファイル生成
#総数　0歳〜100歳以上まで各年齢別にファイルに書き出し
#町丁目ごとに色を変更する
for(j in 8:108){
  quartz(type="png", file=sprintf("sumiyoshiJyukiH2303_H2909_%d.png",j-8), dpi=144, bg="white")
  for(i in 1:last){
    p <- total %>% filter(total$町丁目名==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	ts.plot(ts(p[,j]), col=c(i), xlim=c(1, 14), ylim=c(0, 100), main=paste("住吉区  ", colnames(data_bind)[j], sep= ""), xlab="住民基本台帳", ylab="人")
	text(14+0.55, p[14,j], labels=name[i], cex=0.5)
	par(xaxt="s")
	axis(side=1, at=1:14, labels=c("H23/3", "H23/9", "H24/3", "H24/9", "H25/3", "H25/9", "H26/3", "H26/9", "H27/3", "H27/9", "H28/3", "H28/9", "H29/3", "H29/9"))
 }
 dev.off()
}

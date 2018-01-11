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

#住基14回分を色を変えて、町丁目ごとに全年齢を描画
for(j in 1:14){
  for(i in 1:last){
   p <- total %>% filter(total$町丁目名==name[i])
   par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
   plot(c(0:100), p[j,8:108], type="l", col=c(j), xlim=c(0, 100), ylim=c(0, 100), main="住吉区　住民基本台帳　各年齢別　人口", xlab="年齢", ylab="人")
   par(xaxt="s")
   axis(side=1, at=0:100, labels=c(0:100))
  }
}

#住基14回別に、町丁目ごとに色を変えて全年齢を描画
#ファイルに書き出し
for(j in 1:14){
  quartz(type="pdf", file=sprintf("sumiyoshiJyuki2H2303_H2909_%d.pdf",j))
  .main=paste("住吉区　住民基本台帳　各年齢別　人口　", p[j,1], sep="")
  for(i in 1:last){
   p <- total %>% filter(total$町丁目名==name[i])
   par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
   plot(c(0:100), p[j,8:108], type="l", col=c(i), xlim=c(0, 100), ylim=c(0, 100), main=.main, xlab="年齢", ylab="人")
   par(xaxt="s")
   axis(side=1, at=0:100, labels=c(0:100))
  }
  dev.off()
}

#町丁目ごとに住基14回分をファイルに書き出し
for(i in 1:last){
  quartz(type="pdf", file=sprintf("sumiyoshiJyuki2H2303_H2909_%d%s.pdf", i, name[i]))
  for(j in 1:14){    
    p <- total %>% filter(total$町丁目名==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	plot(c(0:100), p[j,8:108], type="l", col=c(j), xlim=c(0, 100), ylim=c(0, 100), main="住吉区　住民基本台帳　各年齢別　人口　", xlab="年齢", ylab="人")
	text(50+0.55, 80, labels=name[i], cex=1.5)
	par(xaxt="s")
	axis(side=1, at=0:100, labels=c(0:100))
  }
  dev.off()
}

######################################################
# GIFアニメ用 pngファイル生成
#住基14回別に、町丁目ごとに色を変えて全年齢を描画
for(j in 1:14){
  quartz(type="png", file=sprintf("sumiyoshiJyuki2H2303_H2909_%d.png",j), dpi=144, bg="white")
  .main=paste("住吉区　住民基本台帳　各年齢別　人口　", p[j,1], sep="")
  for(i in 1:last){
   p <- total %>% filter(total$町丁目名==name[i])
   par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
   plot(c(0:100), p[j,8:108], type="l", col=c(i), xlim=c(0, 100), ylim=c(0, 100), main=.main, xlab="年齢", ylab="人")
   par(xaxt="s")
   axis(side=1, at=0:100, labels=c(0:100))
  }
  dev.off()
}

#町丁目ごとに住基14回分をファイルに書き出し
for(i in 1:last){
  quartz(type="png", file=sprintf("sumiyoshiJyuki2H2303_H2909_%d%s.png", i, name[i]), dpi=144, bg="white")
  for(j in 1:14){    
    p <- total %>% filter(total$町丁目名==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	plot(c(0:100), p[j,8:108], type="l", col=c(j), xlim=c(0, 100), ylim=c(0, 100), main="住吉区　住民基本台帳　各年齢別　人口　", xlab="年齢", ylab="人")
	text(50+0.55, 80, labels=name[i], cex=1.5)
	par(xaxt="s")
	axis(side=1, at=0:100, labels=c(0:100))
  }
  dev.off()
}
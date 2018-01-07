#浪速区

###################################################
#住民基本台帳人口　町丁目別　各年齢構成　
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

#住基14回分を色を変えて、町丁目ごとに全年齢を描画
for(j in 1:14){
  for(i in 1:62){
   p <- total %>% filter(total$町丁目名==name[i])
   par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
   plot(c(0:100), p[j,8:108], type="l", col=c(j), xlim=c(0, 100), ylim=c(0, 100), main="浪速区　住民基本台帳　各年齢別　人口", xlab="年齢", ylab="人")
   par(xaxt="s")
   axis(side=1, at=0:100, labels=c(0:100))
  }
}

#住基14回別に、町丁目ごとに色を変えて全年齢を描画
#ファイルに書き出し
for(j in 1:14){
  quartz(type="pdf", file=sprintf("naniwaJyuki2H2303_H2909_%d.pdf",j))
  for(i in 1:62){
   p <- total %>% filter(total$町丁目名==name[i])
   par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
   plot(c(0:100), p[j,8:108], type="l", col=c(i), xlim=c(0, 100), ylim=c(0, 100), main=paste("浪速区　住民基本台帳　各年齢別　人口　", p[j,1], sep=""), xlab="年齢", ylab="人")
   text(100+0.55, p[j,68], labels=name[i], cex=0.5)
   par(xaxt="s")
   axis(side=1, at=0:100, labels=c(0:100))
  }
  dev.off()
}

#GIFアニメにする前提でpngファイルに書き出し -> GIFアニメ作成ソフトで作業
#住基14回別に、町丁目ごとに色を変えて全年齢を描画
for(j in 1:14){
  quartz(type="png", file=sprintf("naniwaJyuki2H2303_H2909_%d.png",j))
  for(i in 1:62){
   p <- total %>% filter(total$町丁目名==name[i])
   par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
   plot(c(0:100), p[j,8:108], type="l", col=c(i), xlim=c(0, 100), ylim=c(0, 100), main=paste("浪速区　住民基本台帳　各年齢別　人口　", p[j,1], sep=""), xlab="年齢", ylab="人")
   text(100+0.55, p[j,68], labels=name[i], cex=0.5)
   par(xaxt="s")
   axis(side=1, at=0:100, labels=c(0:100))
  }
  dev.off()
}


#単年度で、町丁目ごとにファイルに書き出し -> 主成分分析と照らし合わせる
#最後の住基H29/9
for(i in 1:62){
 quartz(type="png", file=sprintf("naniwaJyuki2BH2303_H2909_%d%s.png",i,name[i]))
 p <- total %>% filter(total$町丁目名==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 plot(c(0:100), p[14,8:108], type="l", col=c(i), xlim=c(0, 100), ylim=c(0, 100), main=paste("浪速区　住民基本台帳　各年齢別　人口　", p[14,1], sep=""), xlab="年齢", ylab="人")
 text(50+0.55, 80, labels=name[i], cex=1.5)
 par(xaxt="s")
 axis(side=1, at=0:100, labels=c(0:100))
 dev.off()
}

#町丁目ごとに住基14回分をファイルに書き出し
for(i in 1:62){
  quartz(type="pdf", file=sprintf("naniwaJyuki2BH2303_H2909_%d.pdf",i))
  for(j in 1:14){    
    p <- total %>% filter(total$町丁目名==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	plot(c(0:100), p[j,8:108], type="l", col=c(j), xlim=c(0, 100), ylim=c(0, 100), main="浪速区　住民基本台帳　各年齢別　人口　", xlab="年齢", ylab="人")
	text(50+0.55, 80, labels=name[i], cex=1.5)
	par(xaxt="s")
	axis(side=1, at=0:100, labels=c(0:100))
  }
  dev.off()
}
###################################################
# 世帯数　H12-H27　４回分の推移を見る
# 注意１　H27のヘッダーの最後、「当たり」を「当り」に上書きしないとエラー
#ライブラリ
library(dplyr)

setwd("~/Documents/住吉区")

lf <- list.files(path="~/Documents/住吉区/setai", full.names=T)
data <- lapply(lf, read.csv)
data_bind <- do.call(rbind, data)

#HOSYOが3のデータだけ抽出(町丁目個別のデータのみ)
hyosyo <- data_bind %>% filter(data_bind$HYOSYO=="3")

name <- hyosyo[1:102,4]

#全項目一覧　描画
for(i in 1:102){
 p <- data_bind %>% filter(data_bind$NAME==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,9]), ts(p[,10]), ts(p[,11]), ts(p[,12]), ts(p[,13]), col=c(1:5), xlim=c(1, 4), ylim=c(0, 2000), main="住吉区　世帯人員別(1-5人)世帯数", xlab="国勢調査実施年", ylab="世帯数")
 text(4+0.15, p[4,9], labels=name[i], cex=0.5)
 par(xaxt="s")
 axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
}

#各項目ごとにファイルに書き出し
for(j in 9:13){
  quartz(type="pdf", file=sprintf("sumiyoshiSetaiH12_H27_%d.pdf",j-8))
  for(i in 1:102){
    p <- data_bind %>% filter(data_bind$NAME==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	ts.plot(ts(p[,j]), col=c(j-8), xlim=c(1, 4), ylim=c(0, 2000), main=paste("住吉区  ", colnames(data_bind)[j], sep= ""), xlab="国勢調査実施年", ylab="世帯数")
	text(4+0.15, p[4,j], labels=name[i], cex=0.5)
	par(xaxt="s")
	axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
 }
 dev.off()
}

#世帯人員2人　レンジを変えて描画
for(i in 1:102){
 p <- data_bind %>% filter(data_bind$NAME==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,10]), col=c(2), xlim=c(1, 4), ylim=c(0, 500), main=paste("住吉区  ", colnames(data_bind)[10], sep= ""), xlab="国勢調査実施年", ylab="世帯数")
 text(4+0.15, p[4,10], labels=name[i], cex=0.5)
 par(xaxt="s")
 axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
}

#世帯人員4人　レンジを変えて描画
for(i in 1:102){
 p <- data_bind %>% filter(data_bind$NAME==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,12]), col=c(4), xlim=c(1, 4), ylim=c(0, 100), main=paste("住吉区  ", colnames(data_bind)[12], sep= ""), xlab="国勢調査実施年", ylab="世帯数")
 text(4+0.15, p[4,12], labels=name[i], cex=0.5)
 par(xaxt="s")
 axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
}

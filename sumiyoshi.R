#ライブラリ読込
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

#作業ディレクトリ指定
setwd("~/Desktop/xxxx")

#必要なファイル読込
shape <- st_read("h27kaxxxxx.shp") #地図境界線、人口

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")
col_km <- shape$JINKO %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(shape[4], col=col_km, main="住吉区　人口")
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]+0.0005, labels=shape$MOJI, cex=0.5)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.0005, labels=shape$JINKO, cex=0.7)

#####################################################
# csvデータを結合して描画
#前処理1：１行目を削除して2行目をヘッダーにする(ヘッダー移設する)
#前処理2：エディタで色付け計算のためにｘや-,NAは0置換

data1 <- read_csv("xxxx.csv")

#シェープファイルと結合
data <- left_join(shape, data1, by=c("MOJI"="NAME")

#地図描画
par(family="HiraKakuProN-W3")
col_km <- data$総数７５歳以上 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"OrRd"))
plot(shape[4], col=col_km, main="住吉区　総数75歳以上")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]+0.0005, labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0005, labels=data$総数７５歳以上, cex=0.7)

###################################################
# H12-H27　４回分の推移を見る
#ライブラリ
library(dplyr)

setwd("~/Desktop/sumiyoshi/")
lf <- list.files(path="~/Desktop/sumiyoshi/5ages", full.names=T)
data <- lapply(lf, read.csv)
data_bind <- do.call(rbind, data)

#HOSYOが3のデータだけ抽出(町丁目個別のデータのみ)
hyosyo <- data_bind %>% filter(data_bind$HYOSYO=="3")

name <- hyosyo[1:102,4]

#全項目
for(i in 1:102){
 p <- data_bind %>% filter(data_bind$NAME==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,9]), ts(p[,10]), ts(p[,11]), ts(p[,12]), ts(p[,13]), ts(p[,14]), ts(p[,15]), ts(p[,16]), ts(p[,17]), ts(p[,18]), ts(p[,19]), ts(p[,20]), ts(p[,21]), ts(p[,22]), ts(p[,23]), col=c(1:15), xlim=c(1, 4), ylim=c(0, 500), main="住吉区　年齢５歳階級別　人口", xlab="国勢調査実施年", ylab="人")
 par(xaxt="s")
 axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
}

#シングル項目　ラベル付き
for(i in 1:102){
 p <- data_bind %>% filter(data_bind$NAME==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,9]), col=c(2), xlim=c(1, 4), ylim=c(0, 500), main=paste("住吉区　", colnames(data_bind)[9], sep= ""), xlab="国勢調査実施年", ylab="人")
 text(4+0.15, p[4,9], labels=name[i], cex=0.5)
 par(xaxt="s")
 axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
}

#総数　各項目ごとにファイルに書き出し
for(j in 9:23){
  quartz(type="pdf", file=sprintf("sumiyoshi5agesH12_H27_%d.pdf",j-8))
  for(i in 1:102){
    p <- data_bind %>% filter(data_bind$NAME==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	ts.plot(ts(p[,j]), col=c(j-8), xlim=c(1, 4), ylim=c(0, 500), main=paste("住吉区  ", colnames(data_bind)[j], sep= ""), xlab="国勢調査実施年", ylab="人")
	text(4+0.15, p[4,j], labels=name[i], cex=0.5)
	par(xaxt="s")
	axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
 }
 dev.off()
}

#男のみ　各項目ごとにファイルに書き出し
for(j in 29:43){
  quartz(type="pdf", file=sprintf("sumiyoshi5agesMaleH12_H27_%d.pdf",j-28))
  for(i in 1:102){
    p <- data_bind %>% filter(data_bind$NAME==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	ts.plot(ts(p[,j]), col=c(j-28), xlim=c(1, 4), ylim=c(0, 500), main=paste("住吉区  ", colnames(data_bind)[j], sep= ""), xlab="国勢調査実施年", ylab="人")
	text(4+0.15, p[4,j], labels=name[i], cex=0.5)
	par(xaxt="s")
	axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
 }
 dev.off()
}

#女のみ　各項目ごとにファイルに書き出し
for(j in 49:63){
  quartz(type="pdf", file=sprintf("sumiyoshi5agesFemaleH12_H27_%d.pdf",j-48))
  for(i in 1:102){
    p <- data_bind %>% filter(data_bind$NAME==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	ts.plot(ts(p[,j]), col=c(j-48), xlim=c(1, 4), ylim=c(0, 500), main=paste("住吉区  ", colnames(data_bind)[j], sep= ""), xlab="国勢調査実施年", ylab="人")
	text(4+0.15, p[4,j], labels=name[i], cex=0.5)
	par(xaxt="s")
	axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
 }
 dev.off()
}

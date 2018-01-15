#浪速区

#ライブラリ読込
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

#作業ディレクトリ指定
setwd("~/Documents/浪速区")

#必要なファイル読込
shape <- st_read("h27ka27111.shp") #地図境界線、人口

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")
col_km <- shape$JINKO %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(shape[4], col=col_km, main="浪速区　人口")
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2], labels=shape$MOJI, cex=0.5)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.0007, labels=shape$JINKO, cex=0.7)

#####################################################
# csvデータを結合して描画
#前処理1：１行目を削除して2行目をヘッダーにする(ヘッダー移設する)
#前処理2：ｘや-,NAは0置換

data1 <- read_csv("xxxx.csv")

#シェープファイルと結合
data <- left_join(shape, data1, by=c("MOJI"="NAME")

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")
col_km <- data$総数１５歳未満 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(data[4], col=col_km, main="浪速区　総数15歳未満")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$総数１５歳未満, cex=0.7)

###################################################
# 年齢５歳階級別人口　H12-H27　４回分の推移を見る
#ライブラリ
library(dplyr)

setwd("~/Documents/浪速区/")
lf <- list.files(path="~/Documents/浪速区/5ages", full.names=T)
data <- lapply(lf, read.csv)
data_bind <- do.call(rbind, data)

#浪速区はたまたま町丁目が3だけで事足りるので、以下の方法でよい。
#そうでない場合は、住基から町丁目ファイルを作成してマッチングする必要あり。住吉区参照。
#HYOSYOが3のデータだけ抽出(町丁目個別のデータのみ)
hyosyo <- data_bind %>% filter(data_bind$HYOSYO=="3")

#国調4回分で割った数=町丁目の総数、あとでループに使う
last <- length(hyosyo$NAME)/4

#名前配列に確保
name <- hyosyo[1:last,4]

#全項目
for(i in 1:last){
 p <- data_bind %>% filter(data_bind$NAME==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,9:23]), col=c(1:15), xlim=c(1, 4), ylim=c(0, 500), main="浪速区　年齢５歳階級別　人口", xlab="国勢調査実施年", ylab="人")
 par(xaxt="s")
 axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
}

#シングル項目　ラベル付き
for(i in 1:last){
 p <- data_bind %>% filter(data_bind$NAME==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,9]), col=c(i), xlim=c(1, 4), ylim=c(0, 500), main=paste("浪速区  ", colnames(data_bind)[9], sep= ""), xlab="国勢調査実施年", ylab="人")
 text(4+0.15, p[4,9], labels=name[i], cex=0.5)
 par(xaxt="s")
 axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
}

#総数　各項目ごとにファイルに書き出し
for(j in 9:23){
  quartz(type="pdf", file=sprintf("naniwa5agesH12_H27_%s.pdf", colnames(data_bind)[j]))
  for(i in 1:last){
    p <- data_bind %>% filter(data_bind$NAME==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	ts.plot(ts(p[,j]), col=c(i), xlim=c(1, 4), ylim=c(0, 500), main=paste("浪速区  ", colnames(data_bind)[j], sep= ""), xlab="国勢調査実施年", ylab="人")
	text(4+0.15, p[4,j], labels=name[i], cex=0.5)
	par(xaxt="s")
	axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
 }
 dev.off()
}

#男のみ　各項目ごとにファイルに書き出し
for(j in 29:43){
  quartz(type="pdf", file=sprintf("naniwa5agesH12_H27_%s.pdf", colnames(data_bind)[j]))
  for(i in 1:last){
    p <- data_bind %>% filter(data_bind$NAME==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	ts.plot(ts(p[,j]), col=c(i), xlim=c(1, 4), ylim=c(0, 500), main=paste("浪速区  ", colnames(data_bind)[j], sep= ""), xlab="国勢調査実施年", ylab="人")
	text(4+0.15, p[4,j], labels=name[i], cex=0.5)
	par(xaxt="s")
	axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
 }
 dev.off()
}

#女のみ　各項目ごとにファイルに書き出し
for(j in 49:63){
  quartz(type="pdf", file=sprintf("naniwa5agesH12_H27_%s.pdf", colnames(data_bind)[j]))
  for(i in 1:last){
    p <- data_bind %>% filter(data_bind$NAME==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	ts.plot(ts(p[,j]), col=c(i), xlim=c(1, 4), ylim=c(0, 500), main=paste("浪速区  ", colnames(data_bind)[j], sep= ""), xlab="国勢調査実施年", ylab="人")
	text(4+0.15, p[4,j], labels=name[i], cex=0.5)
	par(xaxt="s")
	axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
 }
 dev.off()
}

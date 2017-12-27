#浪速区

#ライブラリ読込
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

#作業ディレクトリ指定
setwd("~/Desktop/naniwa")

#必要なファイル読込
shape <- st_read("h27ka27111.shp") #地図境界線、人口
school <- read_csv("elementaryschool.csv") #小学校
kenchiki <- read_csv("facilities201712.csv") #検知器

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- shape$JINKO %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(shape[4], col=col_km, main="浪速区　人口")
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2], labels=shape$MOJI, cex=0.5)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.0007, labels=shape$JINKO, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

####################################################################
#５歳別人口
#
nenrei <- read_csv("nenrei.csv") #５歳別人口 -は0に置換しないとnumにならないので前処理必要、項目名も変更

#シェープファイルと年齢データを結合
data <- left_join(shape, nenrei, by="MOJI")

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$総数１５歳未満 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(data[4], col=col_km, main="浪速区　総数15歳未満")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$総数１５歳未満, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
#総数５〜９歳

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$TOTAL5_9 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(data[4], col=col_km, main="浪速区　総数5〜9歳")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$TOTAL5_9, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
#総数10〜14歳
#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$TOTAL10_14 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(data[4], col=col_km, main="浪速区　総数10〜14歳")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$TOTAL10_14, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
###################################################################
#男５〜９歳

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$MALE5_9 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Blues"))
plot(data[4], col=col_km, main="浪速区　男5〜9歳")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$MALE5_9, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
#男10〜14歳
#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$MALE10_14 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Blues"))
plot(data[4], col=col_km, main="浪速区　男10〜14歳")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$MALE10_14, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
###################################################################
#女５〜９歳

#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$FEMALE5_9 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Reds"))
plot(data[4], col=col_km, main="浪速区　女5〜9歳")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$FEMALE5_9, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################################
#女10〜14歳
#地図　描画（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")

#地図と人口　描画
col_km <- data$FEMALE10_14 %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Reds"))
plot(data[4], col=col_km, main="浪速区　女10〜14歳")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2], labels=data$MOJI, cex=0.5)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.0007, labels=data$FEMALE10_14, cex=0.7)

#小学校　描画
points(school$X, school$Y, lwd=7, col="blue")
text(school$X, school$Y+0.0007, labels=school$学校名, cex=1)

#検知器　描画
points(kenchiki$経度, kenchiki$緯度, pch=16, col="red")

###################################################
# H12-H27　４回分の推移を見る
#ライブラリ
library(dplyr)

setwd("~/Desktop/naniwa_mimamori/")
lf <- list.files(path="~/Desktop/naniwa_mimamori/5ages", full.names=T)
data <- lapply(lf, read.csv)
data_bind <- do.call(rbind, data)

#HOSYOが3のデータだけ抽出(町丁目個別のデータのみ)
hyosyo <- data_bind %>% filter(data_bind$HYOSYO=="3")

name <- hyosyo[1:124,4]

#全項目
for(i in 1:124){
 p <- data_bind %>% filter(data_bind$NAME==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,9]), ts(p[,10]), ts(p[,11]), ts(p[,12]), ts(p[,13]), ts(p[,14]), ts(p[,15]), ts(p[,16]), ts(p[,17]), ts(p[,18]), ts(p[,19]), ts(p[,20]), ts(p[,21]), ts(p[,22]), ts(p[,23]), col=c(1:15), xlim=c(1, 4), ylim=c(0, 500), main="浪速区　年齢５歳階級別　人口", xlab="国勢調査実施年", ylab="人")
 par(xaxt="s")
 axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
}

#シングル項目　ラベル付き
for(i in 1:124){
 p <- data_bind %>% filter(data_bind$NAME==name[i])
 par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
 ts.plot(ts(p[,9]), col=c(2), xlim=c(1, 4), ylim=c(0, 500), main=paste("浪速区  ", colnames(data_bind)[9], sep= ""), xlab="国勢調査実施年", ylab="人")
 text(4+0.15, p[4,9], labels=name[i], cex=0.5)
 par(xaxt="s")
 axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
}

#すべての項目を各項目ごとにファイルに書き出し
for(j in 9:23){
  quartz(type="pdf", file=sprintf("naniwa5agesH12_H27_%d.pdf",j-8))
  for(i in 1:124){
    p <- data_bind %>% filter(data_bind$NAME==name[i])
	par(new=TRUE, family="HiraKakuProN-W3", xpd=TRUE, xaxt="n")
	ts.plot(ts(p[,j]), col=c(j-8), xlim=c(1, 4), ylim=c(0, 500), main=paste("浪速区  ", colnames(data_bind)[j], sep= ""), xlab="国勢調査実施年", ylab="人")
	text(4+0.15, p[4,j], labels=name[i], cex=0.5)
	par(xaxt="s")
	axis(side=1, at=1:4, labels=c("平成12年", "平成17年", "平成22年", "平成27年"))
 }
 dev.off()
}
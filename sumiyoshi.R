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
#区の町丁目可視化
#総務省e-Statから世界測地系のshpファイル入手
#e-Stat > 地図で見る統計（統計GIS） > データダウンロード
#フォルダの名前を適宜変更（ダウンロードした４つのファイル全部が必要）

#ライブラリ読込
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

#作業ディレクトリ指定　(環境によって変わるので注意)
setwd("C:/Users/ユーザー名/Desktop/フォルダ名")　

#区丁目シェープファイル読込
shape <- st_read("h27ka27115.shp")

#シェープファイルの属性確認
str(shape)

#csvファイル読込
population <- read_csv("xxxx.csv")

#シェープファイルとcsvファイルを町丁目のMOJIをキーとして結合
data <- inner_join(shape, population, by="MOJI")

#描画１（これはe-Statにもとから人口が入っているシェープファイル）
par(family="HiraKakuProN-W3")
col_km <- shape$JINKO %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(shape[4], col=col_km)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2], labels=shape$MOJI, cex=0.5)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.001, labels=shape$JINKO, cex=0.5)

#描画２（上記で読み込んだcsvをバインドしたシェープファイル)
par(family="HiraKakuProN-W3")
col_km <- shape[,32] %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(shape[4], col=col_km)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2], labels=shape$MOJI, cex=0.5)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.001, labels=shape[,32], cex=0.5)
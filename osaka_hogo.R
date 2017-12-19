#ライブラリ読込
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

#作業ディレクトリ指定
setwd("~/Desktop/osaka_hogo")

#必要なファイル読込
shape <- st_read("h27_did_27.shp")
hogo <- read_csv("xxxx.csv")

#結合後、23区のみ残す
data <- inner_join(shape, hogo, by="CITYNAME")

#地図描画
par(family="HiraKakuProN-W3")
col_km <- data$生活保護世帯 %>% classIntervals(., n=8, style="kmeans") %>% findColours(.,pal=brewer.pal(8, "YlOrRd"))
plot(shape[1:24,4], col=col_km, main="大阪市24区　生活保護世帯数 平成29年3月")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]+0.005, labels=data$CITYNAME, cex=1)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.005, labels=data$生活保護世帯, cex=1)

#################################

par(family="HiraKakuProN-W3")
col_km <- data$生活保護人員 %>% classIntervals(., n=8, style="kmeans") %>% findColours(.,pal=brewer.pal(8, "YlGnBu"))
plot(shape[1:24,4], col=col_km, main="大阪市24区　生活保護人員数 平成29年3月")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]+0.005, labels=data$CITYNAME, cex=1)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.005, labels=data$生活保護人員, cex=1)

#################################

par(family="HiraKakuProN-W3")
col_km <- data$保護率 %>% classIntervals(., n=8, style="kmeans") %>% findColours(.,pal=brewer.pal(8, "Reds"))
plot(shape[1:24,4], col=col_km, main="大阪市24区　保護率(千分率) 平成29年3月")
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]+0.005, labels=data$CITYNAME, cex=1)
text(st_coordinates(data %>% st_centroid)[,1], st_coordinates(data %>% st_centroid)[,2]-0.005, labels=data$保護率, cex=1)

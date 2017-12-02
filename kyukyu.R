set救急搬送データ　plot
・22万件なのでMacのNumbersは使用不可（最大行数が6万件）
・WindowsでLibreOfficeで読み込んだが、結局Rですべて作業できる
・dplyrはデータベースライブラリ。SQL的に使える。1000万行、100MBを扱える

#ライブラリ読込
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(oplenxlsx)

#作業ディレクトリ指定 
setwd("~/Desktop/救急分析") # Mac
#setwd("C:/Users/tadakazu/Desktop/救急分析/") # Windows

#エクセルファイル読み込み
#救急搬送データと国交省から作成した街区緯度経度（数値は救急に合わせ全角で置換）
kyukyu <- read.xlsx("kyukyu.xlsx")

#OsakaCityAddressの一列目は後でマッチさせるため”発生場所・番"に変更・保存
address <- read.xlsx("OsakaCityAddress.xlsx")

#左に位置する救急搬送データに住所でマッチさせて緯度経度のカラムを追加
data <- left_join(kyukyu, address, by="発生場所・番")

#淀川第１救急隊を抽出
yodogawa <- data %>% filter(data[,1]=="淀川第１救急隊")

#描画(緯度経度のXYが前後しているので注意。項番25,24の順)
shape <- st_read("h27_did_27.shp")
par(family="HiraKakuProN-W3")
plot(shape[1:24,3], col="gray", main=yodogawa[1,1]) #タイトルを隊名に
points(yodogawa[,24], yodogawa[,23], lwd=1, col="red")

#分析例
#入電-現着（分数値）が30分以上を青で描画
ebisu_30 <- ebisu %>% filter(ebisu[,7] >= 30)
points(ebisu_30[,24], ebisu_30[,23], pch=16, col="blue")

#その現場は？
print(ebisu_30[,2])


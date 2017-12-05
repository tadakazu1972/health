set救急搬送データ　plot
・22万件なのでMacのNumbersは使用不可（最大行数が6万件）
・WindowsでLibreOfficeで読み込んだが、結局Rですべて作業できる
・dplyrはデータベースライブラリ。SQL的に使える。1000万行、100MBを扱える

#ライブラリ読込
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(openxlsx)

#作業ディレクトリ指定 
setwd("~/Desktop/kyukyu") # Mac
#setwd("C:/Users/tadakazu/Desktop/kyukyu/") # Windows

#エクセルファイル読み込み
#救急搬送データと国交省から作成した街区緯度経度（数値は救急に合わせ全角で置換）
kyukyu <- read.xlsx("kyukyu.xlsx")

#OsakaCityAddressの一列目は後でマッチさせるため”発生場所・番"に変更・保存
address <- read.xlsx("OsakaCityAddress2.xlsx")

#左に位置する救急搬送データに住所でマッチさせて緯度経度のカラムを追加
data <- left_join(kyukyu, address, by="発生場所・番")

#淀川第１救急隊を抽出
yodogawa <- data %>% filter(data[,1]=="淀川第１救急隊")

#描画(緯度経度のXYが前後しているので注意。項番24,23の順)
shape <- st_read("h27_did_27.shp")
par(family="HiraKakuProN-W3")
plot(shape[1:24,3], col="gray", main=yodogawa[1,1]) #タイトルを隊名に
points(yodogawa[,24], yodogawa[,23], lwd=1, col="red")

#分析例
#入電-現着（分数値）が30分以上を青で描画
yodogawa30 <- yodogawa %>% filter(yodogawa[,7] >= 30)
points(yodogawa30[,24], yodogawa30[,23], pch=16, col="blue")

#その現場は？
print(yodogawa30[,2])

#全隊描画：現場を赤でプロット、30分以上は青でプロット
#60隊の名前を読み込み (都島第１は都島に変更)
tainame <- read_xlsx("tai.xlsx")

for(i in 1:60){
 tai <- data %>% filter(data[,1]==tainame[i,1])
 quartz(type="pdf", file=sprintf("kyukyutaiH28_%d.pdf", i))
 par(family="HiraKakuProN-W3")
 plot(shape[1:24,3], col="gray", main=tai[i,1])
 points(tai[,24], tai[,23], lwd=1, col="red")
 tai30 <- tai %>% filter(tai[,7] >=30)
 points(tai30[,24], tai30[,23], pch=16, col="blue")
 dev.off()
}

#発生現場を集計 ２番目の項目である現場毎に集計、緯度経度も合わせて持つため引数で指定
genba <- data %>% group_by(data[,2], data[,24], data[,23]) %>% summarize(count=n())

#発生現場　多い順トップ20
head(arrange(genba, desc(count)), 20) #降順がミソ

#それを地図にプロット 
genba20 <- as.matrix(genba)        #pointsするにはdata.frameをmatrixに変換する必要あり
shape <- st_read("h27_did_27.shp") #シェープ読み込み
plot(shape[1:24,4], col="gray")    #白地図を描画
points(genba20[,2], genba20[,3], lwd=1, col="red")


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
tainame <- read.xlsx("tai.xlsx")

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
genba20 <- head(arrange(genba, desc(count)), 20) %>% as.matrix #pointsするにはdata.frameをmatrixに変換する必要あり
shape <- st_read("h27_did_27.shp") #シェープ読み込み
plot(shape[1:24,4], col="gray")    #白地図を描画
points(genba20[,2], genba20[,3], lwd=2, col="red")

#搬送先の緯度経度を追加
hansousaki <- read.xlsx("hansousaki.xlsx") #事前に１項目目を「搬送先・医療機関（名称）」に変更すべし
data2 <- left_join(data, hansousaki, by="搬送先・医療機関（名称）")

#北救急隊の現場から搬送先を線で描く
kita <- data2 %>% filter(kita[,1]=="北救急隊") #北救急隊を抽出
points(kita[,24],kita[,23], lwd=1, col="red") #プロット
points(kita[,27],kita[,26], pch=16, col="blue") #搬送先をプロット
lines(x=c(kita[,24],kita[,27]), y=c(kita[,23],kita[,26]),lty=1, col="green") #線でつなぐ

#全隊の現場(赤)、搬送先(青)でプロット、間を緑の線でつなぐ(欠損多め)
#先に線を描いて、後で現場と搬送先をプロットしないと見えなくなる
#隊名読み込み
tainame <- read.xlsx("tai.xlsx")

for(i in 1:60){
 tai <- data2 %>% filter(data2[,1]==tainame[i,1])
 quartz(type="pdf", file=sprintf("kyukyutaiH28_%dB.pdf", i))
 par(family="HiraKakuProN-W3")
 plot(shape[1:24,3], col="gray", main=tai[i,1])
 lines(x=c(tai[,24], tai[,27]), y=c(tai[,23], tai[,26]), lty=1, col="green")
 points(tai[,24], tai[,23], lwd=1, col="red")
 points(tai[,27], tai[,26], pch=16, col="blue")
 dev.off()
}

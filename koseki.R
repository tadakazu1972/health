#ライブラリ
library(dplyr)

setwd("~/Desktop/koseki")
lf <- list.files(path="~/Desktop/koseki", full.names=T)
data <- lapply(lf, read.csv)
data_bind <- do.call(rbind, data)

#北区だけデータ抽出
ku <- data_bind %>% filter(data_bind$区名=="北区")

#出生数、婚姻届、離婚届、死亡届、入籍届、転籍届、その他を色を変えつつタイムラインとして描画
ts.plot(ts(ku[,4]), ts(ku[,5]), ts(ku[,6]), ts(ku[,7]), ts(ku[,8]), ts(ku[,9]), ts(ku[,10]), col=c(1,2,3,4,5,6,7))

#区名を抜き出して格納
kuname <- data_bind[1:31,1]

#24区を全部一枚に描画
for(i in 1:24){
  ku <- data_bind %>% filter(data_bind$区名==kuname[i])
  par(new=T)
  ts.plot(ts(ku[,4]), ts(ku[,5]), ts(ku[,6]), ts(ku[,7]), ts(ku[,8]), ts(ku[,9]), ts(ku[,10]), col=c(1:7), ylim=c(0,4000))
}

#24区とセンター含む31項ごとにファイルに描画
for(i in 1:31){
  ku <- data_bind %>% filter(data_bind$区名==kuname[i])
  quartz(type="pdf", file=sprintf("koseki%d_%s.pdf", i, kuname[i]))
  par(family="HiraKakuProN-W3")
  ts.plot(ts(ku[,4]), ts(ku[,5]), ts(ku[,6]), ts(ku[,7]), ts(ku[,8]), ts(ku[,9]), ts(ku[,10]), col=c(1,2,3,4,5,6,7))
  dev.off()
}



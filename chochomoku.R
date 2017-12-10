#区の町丁目可視化
#総務省e-Statから世界測地系のshpファイル入手
#e-Stat > 地図で見る統計（統計GIS） > データダウンロード

par(family="HiraKakuProN-W3")
col_km <- shape$JINKO %>% classIntervals(.,n=8,style=“means”) %>% findColours(.,pal=brewer.pal(8,”Reds”))
plot(shape[4], col=col_km)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2], labels=shape$MOJI, cex=0.5)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.001, labels=shape$JINKO, cex=0.5)

library(openxlsx)
library(maptools)
#library(foreign)
library(spsurvey)
library(RColorBrewer)
library(ggplot2)
library(spdep)

## ƒf[ƒ^o—Í & ‰ÁH
setwd("~/Desktop/StudyGroup2017")
kyukyu <- read.xlsx("kyukyu.xlsx")
kyukyu.copy <- kyukyu
osaka.address <- read.xlsx("incidentsites_gis.xlsx")
osaka.add.ll <- cbind(osaka.address[,5],osaka.address[,4])
shozaichi <- read.xlsx("shozaichi.xlsx")
match.vec <- numeric(nrow(kyukyu.copy))
for(i in 1:nrow(kyukyu.copy)){
	match.vec[i] <- match(kyukyu.copy[i,2], osaka.address[,1])
}
sub <- NULL
sub.na <- NULL
for(i in 1:nrow(kyukyu.copy)){
	if(is.na(match.vec[i])==TRUE){
		sub.na <- c(sub.na, i)
	}else{
		sub <- c(sub, i)
	}
}
taisho <- matrix(0, 60, 3)
for(i in 1:60){
	num <- match(shozaichi[i,2], osaka.address[,1])
	taisho[i,1] <- i
	taisho[i,2:3] <- osaka.add.ll[num,]
}
for(i in 1:length(sub)){
	if(kyukyu.copy[sub[i],1]=="“s“‡‹~‹}‘à"){
		kyukyu.copy[sub[i],1] <- "“s“‡‘æ‚P‹~‹}‘à"
	}
}
same <- numeric(length(sub))
for(i in 1:length(sub)){
	same[i] <- match(kyukyu.copy[sub[i],1], shozaichi[,1])
}
sub1.na <- NULL
for(i in 1:length(sub)){
	if(is.na(same[i])==TRUE){
		sub1.na <- c(sub1.na, i)
	}
}
new.sub <- sub[-sub1.na]
###
for(i in 1:nrow(kyukyu.copy)){
	if(kyukyu.copy[i,1]=="“s“‡‹~‹}‘à"){
		kyukyu.copy[i,1] <- "“s“‡‘æ‚P‹~‹}‘à"
	}
}
same1 <- numeric(nrow(kyukyu.copy))
for(i in 1:nrow(kyukyu.copy)){
	same1[i] <- match(kyukyu.copy[i,1], shozaichi[,1])
}
same1.na <- NULL
for(i in 1:length(same1)){
	if(is.na(same1[i])==TRUE){
		same1.na <- c(same1.na, i)
	}
}
same1 <- same1[-same1.na]
each.tai.full <- list()
for(i in 1:60){
	each.tai <- NULL
	for(j in 1:length(same1)){
		if(is.na(same1[j])==FALSE){
			if(same1[j]-i==0){
				each.tai <- c(each.tai, j)
			}
		}
	}
	each.tai.list <- list(each.tai)
	names(each.tai.list) <- shozaichi[i,1]
	each.tai.full <- c(each.tai.full, each.tai.list)
}
each.tai.num <- numeric(60)
for(i in 1:60){
	each.tai.num[i] <- length(each.tai.full[[i]])
}
each.tai.num <- c(each.tai.num, nrow(kyukyu)-sum(each.tai.num))
each.tai.num <- data.frame(each.tai.num)
for(i in 1:60){
	rownames(each.tai.num)[i] <- shozaichi[i,1]
}
rownames(each.tai.num)[61] <- "‚»‚Ì‘¼"
###
place <- "ˆê”Ê"
for(i in 1:length(new.sub)){
	mat <- match(kyukyu[new.sub[i],3], place)
	if(is.na(mat)==TRUE){
		place <- c(place, kyukyu[new.sub[i],3])
	}
}
shubetsu <- "ä¸€èˆ¬"
for(i in 1:length(new.sub)){
	mat <- match(kyukyu[new.sub[i],5], shubetsu)
	if(is.na(mat)==TRUE){
		shubetsu <- c(shubetsu, kyukyu[new.sub[i],5])
	}
}
##place <- c("Z‘î", "‚»‚Ì‘¼‚Ì“¹˜H", "ŒöOo“ü‚èêŠ", "d–ê", "‚»‚Ì‘¼‚Ì‰®ŠO", "‹O“¹ã", "ŒöO‚Ìæ~‚·‚é‰^—A‹@ŠÖ“à", "‚»‚Ì‘¼‚Ì‰®“à", "‚»‚Ì‘¼‚Ì‰^—A‹@ŠÖ“à", "ã_‚‘¬“¹", "…ã", "‹ß‹E©“®Ô“¹") 
##shubetsu <- c("ˆê”Ê", "‹}•a", "‰ÁŠQ", "©‘¹", "“]‰@”À‘—", "Œğ’Ê", "‰ÎĞ", "‚»‚Ì‘¼", "˜JĞ", "‰^“®", "…“ï", "©‘R", "‘ŠíŞ—A‘—", "ˆãt”À‘—")
place.tag <- numeric(length(new.sub))
for(i in 1:length(new.sub)){
	place.tag[i] <- match(kyukyu[new.sub[i],3], place)
}
shubetsu.tag <- numeric(length(new.sub))
for(i in 1:length(new.sub)){
	shubetsu.tag[i] <- match(kyukyu[new.sub[i],5], shubetsu) 
}

ll.data <- matrix(0, length(new.sub), 4)
for(i in 1:length(new.sub)){
	ll.data[i,1] <- new.sub[i]
	ll.data[i,2:3] <- osaka.add.ll[match.vec[new.sub[i]],]
	ll.data[i,4] <- same[-sub1.na][i]
}
ll.data <- cbind(ll.data, place.tag, shubetsu.tag)
colnames(ll.data) <- c("tag", "Œo“x", "ˆÜ“x", "‹~‹}‘à", "êŠ", "í—Ş")

## ‰ÁHÏ‚İƒf[ƒ^‘‚«o‚µ & o—Í
write(ll.data, file="data.txt")
write(taisho, file="taisho.txt")
write.table(each.tai.num, file="each.taisho.txt", row.names=TRUE)
ll.data <- matrix(scan("data.txt"), 226530, 6)
taisho <- matrix(scan("taisho.txt"), 60, 3)
each.tai.num <- read.table("each.taisho.txt")
comb.taisho <- list()
for(i in 1:60){
	pare <- NULL
	for(j in 1:nrow(ll.data)){
		if(ll.data[j,4]-i==0){
			pare <- c(pare, j)
		}  
	}
	comb.taisho <- c(comb.taisho, list(pare))
}
comb.num1 <- numeric(60)
for(i in 1:60){
	comb.num1[i] <- length(comb.taisho[[i]])
}
shubetsu <- c("ˆê”Ê", "‹}•a", "‰ÁŠQ", "©‘¹", "“]‰@”À‘—", "Œğ’Ê", "‰ÎĞ", "‚»‚Ì‘¼", "˜JĞ", "‰^“®", "…“ï", "©‘R", "‘ŠíŞ—A‘—", "ˆãt”À‘—")
comb.taisho.shubetsu <- list()
for(i in 1:60){
	comb.shubetsu <- list()
	for(j in 1:14){
		pare <- NULL
		for(k in 1:comb.num1[i]){
			if(ll.data[comb.taisho[[i]],6][k]-j==0){
				pare <- c(pare, comb.taisho[[i]][k])
		    } 
		}
		if(is.null(pare)==FALSE){
			list.pare <- list(pare)
			names(list.pare) <- shubetsu[j]
			comb.shubetsu <- c(comb.shubetsu, list.pare)
		}
	}
	comb.taisho.shubetsu <- c(comb.taisho.shubetsu, list(comb.shubetsu))
}
comb.place <- list()
for(i in 1:12){
	pare <- NULL
	for(j in 1:nrow(ll.data)){
		if(ll.data[j,5]-i==0){
			pare <- c(pare, j)
		}  
	}
	comb.place <- c(comb.place, list(pare))
}
comb.num2 <- numeric(12)
for(i in 1:12){
	comb.num2[i] <- length(comb.place[[i]])
}
comb.shubetsu <- list()
for(i in 1:14){
	pare <- NULL
	for(j in 1:nrow(ll.data)){
		if(ll.data[j,6]-i==0){
			pare <- c(pare, j)
		}  
	}
	comb.shubetsu <- c(comb.shubetsu, list(pare))
}
comb.num3 <- numeric(14)
for(i in 1:14){
	comb.num3[i] <- length(comb.shubetsu[[i]])
}
bar <- as.vector(each.tai.num$each.tai.num)
names(bar) <- c(1:61)
names.num <- c(1:61)
barnames <- numeric(61)
for(i in 1:61){
	barnames[i] <- paste(names.num[i], " ", rownames(each.tai.num)[i])
}
par(mar=c(3,3,4,14), xpd=TRUE, bty="L", family="HiraKakuProN-W3")
barplot(bar[31:61])
legend(par()$usr[2],par()$usr[4], legend=barnames[31:61])
 
## mapping (shape data)
setwd("~/Desktop/StudyGroup2017/shape/osaka")
map <- read.shape("h27_did_27.shp")
map.dbf <- read.dbf("h27_did_27.dbf")
map.cod <- coordinates(map)
#kuname <- c("Miyakojima", "Fukushima", "Konohana", "Nishi", "Minato", "Taisho", "Tennoji", "Naniwa", "Nishiyodogawa", "Higashiyodogawa", "Higashinari", "Ikuno", "Asahi", "Joto", "Abeno", "Sumiyoshi", "Higashisumiyoshi", "Nishinari", "Yodogawa", "Tsurumi", "Suminoe", "Hirano", "Kita", "Chuo")
kuname <- c("“s“‡", "•Ÿ“‡", "Ÿ‰Ô", "¼", "`", "‘å³", "“V‰¤›", "˜Q‘¬", "¼—„ì", "“Œ—„ì", "“Œ¬", "¶–ì", "ˆ®", "é“Œ", "ˆ¢”{–ì", "Z‹g", "“ŒZ‹g", "¼¬", "—„ì", "’ßŒ©", "Z”V]", "•½–ì", "–k", "’†‰›")
k <- 55
par(mar=c(0.5,0.5,5,7), xpd=TRUE, bty="L", family="HiraKakuProN-W3")
plot(map[1:24,], col="gray") 
text(map.cod[1:24,1], map.cod[1:24,2], kuname, col="white")
#points(ll.data[,2], ll.data[,3], col="blue", pch=20)
#points(ll.data[comb.taisho[[k]],2],ll.data[comb.taisho[[k]],3], col="blue", pch=20)

cols <- rainbow(14)
for(i in 1:length(comb.taisho.shubetsu[[k]])){
	points(ll.data[comb.taisho.shubetsu[[k]][[i]],2],ll.data[comb.taisho.shubetsu[[k]][[i]],3], col=cols[i], pch=20)
}
points(taisho[k,2],taisho[k,3], col="red", pch=20)
legend(par()$usr[2],par()$usr[4], legend=c(names(comb.taisho.shubetsu[[k]]), "‹~‹}‘à"), col=c(cols[1:length(comb.taisho.shubetsu[[k]])], "black"), pch=20)



points(ll.data[comb.place[[k]],2],ll.data[comb.place[[k]],3], col="blue", pch=20)
points(ll.data[comb.shubetsu[[k]],2],ll.data[comb.shubetsu[[k]],3], col="blue", pch=20)


#setwd("~/Desktop/StudyGroup2017/ƒ}ƒbƒvƒiƒr‚¨‚¨‚³‚©")
#chocode <- read.csv(file('chocode.csv', encoding='Shift_JIS'), header=T)
#jusho <- read.csv(file('jusho.csv', encoding='Shift_JIS'), header=T)
#kucode <- read.csv(file('kucode.csv', encoding='Shift_JIS'), header=T)
#iryou <- read.csv(file('mapnavoskdat_iryou.csv', encoding='Shift_JIS'), header=T)
#keisatsu <- read.csv(file('mapnavoskdat_keisatsu.csv', encoding='Shift_JIS'), header=T)


## mapping (Google map) terrain, satellite, roadmap, hybrid
library(ggmap)
ll.data1 <- data.frame(Lon=ll.data[,2], Lat=ll.data[,3])
ll.data2 <- data.frame(Lon=taisho[,2], Lat=taisho[,3])
LonLat <- geocode("osaka", source="google")
GMapData <- get_googlemap(center=c(lon=LonLat[1,1]-0.020, lat=LonLat[1,2]-0.01800), zoom=12, size=c(640,640), scale=2, format="png8", maptype="terrain", language="jpn", sensor=FALSE, messaging=FALSE, urlonly=FALSE, filename="ggmapTemp", color="color", force=FALSE)
k <- 1
ggmap(GMapData)+geom_point(data=ll.data1[comb.taisho[[k]],], aes(x=Lon, y=Lat), color="blue")+geom_point(data=ll.data2[k,], aes(x=Lon, y=Lat), color="red")
ggmap(GMapData)+geom_point(data=ll.data1[comb.place[[k]],], aes(x=Lon, y=Lat), color="blue")
ggmap(GMapData)+geom_point(data=ll.data1[comb.shubetsu[[2]],], aes(x=Lon, y=Lat), color="blue")

## Google map + shape data
points <- fortify(map[1:24,], region="area_mdm")
k <- 1
ggmap(GMapData)+geom_polygon(aes(x=long, y=lat, group=group, alpha=0.25), data=points, fill="white")+geom_polygon(aes(x=long, y=lat, group=group, alpha=0.25), data=points, color="black", fill=NA)+geom_point(data=ll.data1[comb.taisho[[k]],], aes(x=Lon, y=Lat), color="blue")+geom_point(data=ll.data2[k,], aes(x=Lon, y=Lat), color="red")



asd <- list()
for(i in 1:length(comb.shubetsu[[1]])){
	for(j in 1:length(comb.shubetsu[[2]])){
		if(comb.shubetsu[[1]][i]-comb.shubetsu[[2]][j]==0){
			asd <- c(asd, list(c(i,j)))
		}
	}
}





















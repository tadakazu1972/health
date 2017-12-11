#��̒����ډ���
#������e-Stat���琢�E���n�n��shp�t�@�C������
#e-Stat > �n�}�Ō��铝�v�i���vGIS�j > �f�[�^�_�E�����[�h
#�t�H���_�̖��O��K�X�ύX�i�_�E�����[�h�����S�̃t�@�C���S�����K�v�j

#���C�u�����Ǎ�
library(dplyr)
library(sf)
library(readr)
library(RColorBrewer)
library(classInt)

#��ƃf�B���N�g���w��@(���ɂ���ĕς��̂Œ���)
setwd("C:/Users/���[�U�[��/Desktop/�t�H���_��")�@

#�撚�ڃV�F�[�v�t�@�C���Ǎ�
shape <- st_read("h27ka27115.shp")

#�V�F�[�v�t�@�C���̑����m�F
str(shape)

#csv�t�@�C���Ǎ�
population <- read_csv("xxxx.csv")

#�V�F�[�v�t�@�C����csv�t�@�C���𒬒��ڂ�MOJI���L�[�Ƃ��Č���
data <- inner_join(shape, population, by="MOJI")

#�`��P�i�����e-Stat�ɂ��Ƃ���l���������Ă���V�F�[�v�t�@�C���j
par(family="HiraKakuProN-W3")
col_km <- shape$JINKO %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(shape[4], col=col_km)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2], labels=shape$MOJI, cex=0.5)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.001, labels=shape$JINKO, cex=0.5)

#�`��Q�i��L�œǂݍ���csv���o�C���h�����V�F�[�v�t�@�C��)
par(family="HiraKakuProN-W3")
col_km <- shape[,32] %>% classIntervals(.,n=8,style="kmeans") %>% findColours(.,pal=brewer.pal(8,"Greens"))
plot(shape[4], col=col_km)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2], labels=shape$MOJI, cex=0.5)
text(st_coordinates(shape %>% st_centroid)[,1], st_coordinates(shape %>% st_centroid)[,2]-0.001, labels=shape[,32], cex=0.5)
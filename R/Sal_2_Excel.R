#--------------------------------------------------------------------
# Title: Xuất kết quả từ model Sal từ sal sang csv
# Purpose:
# Author:Nguyen Trung Nam
# Short description:
# Created: Thu Mar 01 08:12:03 2018
#--------------------------------------------------------------------

# Library:+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(sp)
library(maptools)
library(rgdal)
library(rgeos)
library(tidyr)
library(here)
library(stringr)
library(readxl)
library(dplyr)
library(ggplot2)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("sp", "here", "stringr", "XLConnect", "sp", "maptools","purrr","plyr","readxl","dplyr")
ipak(packages)

## Khai bao thu mục và số lần chạy nếu cần
pa = "PA0_BOD_0"  # Thư mục lưu kq
lan = ""       # số lần chạy

# Load danh sach tram ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df = read_excel(paste0(here::here(),"/rawdata/","ds_Export_Mc_RU.xlsx"),sheet = 1)

# Xuất mực nước giờ tại Hydro station ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_HH = read.table(paste0(here::here(),"/rawdata/sal_S/",pa,"/HD_SAL.HHH"),skip = 3,header = F)
## Lay ten tram do
n = na.omit(as.character(df$mc))
name = c("tt","h","d","m","y",paste0("mc",n))
colnames(df_HH) = name
write.table(df_HH, file = paste0(here::here(),"/delta_sim/out_hd","/df_H_",pa,"_",lan,".csv"), sep = ",", row.names = F)
df_HH = gather(df_HH,mc,value,-h,-d,-m,-y,-tt)
## vẽ biểu đồ tự động
df_HH$date = as.Date(paste(df_HH$d,df_HH$m,df_HH$y,sep ="-"),format = "%d-%m-%Y")
lplotr <- function(x, na.rm = TRUE, ...){
  sta_list = unique(x$mc)
  for (i in seq_along(sta_list)) {
    p <-ggplot(subset(x,x$mc == sta_list[i]),aes(date,value,colour = mc))
    plots = p + geom_line()+
      ggtitle(paste("Dien bien H tại MC",sta_list[i],pa))+
      scale_y_continuous("unit(m)")
    setwd(paste0(here::here(),"/figs/"))
    ggsave(plots,filename= paste("H-",sta_list[i],"_",pa,lan,".png",sep=""),width = 22, height = 11,scale = 0.5)
  }
}
lplotr(df_HH)

# Xuất măn giờ tại Hydro station ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_HH = read.table(paste0(here::here(),"/rawdata/sal_S/",pa,"/HD_SAL.SSS"),skip = 3,header = F)
## Lay ten tram do
n = na.omit(as.character(df$man))
name = c("tt","h","d","m","y",paste0("mc",n))
colnames(df_HH) = name
write.table(df_HH, file = paste0(path_out,"/df_SSS_",lan,".csv"), sep = ",", row.names = F)
df_HH = gather(df_HH,mc,value,-h,-d,-m,-y,-tt)
## vẽ biểu đồ tự động
df_HH$date = as.Date(paste(df_HH$d,df_HH$m,df_HH$y,sep ="-"),format = "%d-%m-%Y")
lplotr <- function(x, na.rm = TRUE, ...){
  sta_list = unique(x$mc)
  for (i in seq_along(sta_list)) {
    p <-ggplot(subset(x,x$mc == sta_list[i]),aes(date,value,colour = mc))
    plots = p + geom_line()+
      ggtitle(paste("Dien bien mặn tại MC",sta_list[i],lan))+
      scale_y_continuous("unit(g/l)")
    setwd(paste0(here::here(),"/figs/"))
    ggsave(plots,filename= paste("S-",sta_list[i],"_",pa,lan,".png",sep=""),width = 22, height = 11,scale = 0.5)
  }
}
lplotr(df_HH)

# Xuất LƯU LƯƠNG giờ tại Hydro station ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_HH = read.table(paste0(here::here(),"/rawdata/sal_S/",pa,"/HD_SAL.QQQ"),skip = 3,header = F)
## Lay ten tram do
n = na.omit(as.character(df$mc))
name = c("tt","h","d","m","y",paste0("mc",n))
colnames(df_HH) = name
write.table(df_HH, file = paste0(here::here(),"/delta_sim/out_hd","/df_Q_",pa,"_",lan,".csv"), sep = ",", row.names = F)
df_HH = gather(df_HH,mc,value,-h,-d,-m,-y,-tt)
## vẽ biểu đồ tự động
df_HH$date = as.Date(paste(df_HH$d,df_HH$m,df_HH$y,sep ="-"),format = "%d-%m-%Y")
lplotr <- function(x, na.rm = TRUE, ...){
  sta_list = unique(x$mc)
  for (i in seq_along(sta_list)) {
    p <-ggplot(subset(x,x$mc == sta_list[i]),aes(date,value,colour = mc))
    plots = p + geom_line()+
      ggtitle(paste("Dien bien Flow tại MC",sta_list[i],lan))+
      scale_y_continuous("unit(m3/s)")
    setwd(paste0(here::here(),"/figs/"))
    ggsave(plots,filename= paste("Q-",sta_list[i],"_",pa,lan,".png",sep=""),width = 22, height = 11,scale = 0.5)
  }
}
lplotr(df_HH)



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Section: Xuat ket quả cua BOD & DO, ToN; ToP
# Purpose:
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
pa = "PA0_BOD"
lan = ""
# Khai thác DO
# Load danh sach tram ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df = read_excel(paste0(here::here(),"/rawdata/","ds_Export_Mc_RU.xlsx"),sheet = 1)
# Xuất DO tai MC ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
n = na.omit(as.character(df$do))
name = c("tt","h","d","m","y",n)
df_HH = read.table(paste0(here::here(),"/rawdata/sal_S/",pa,"/HD_SAL.OXY"),skip = 3,header = F)
colnames(df_HH) = name
## ghi ra bang full cua DO
write.table(df_HH, file = paste0(path_outwq,"/df_DO_full_",pa,".csv"), sep = ",", row.names = F)
df_HH$tt = NULL
df_HH = gather(df_HH,mc,value,-h,-d,-m,-y)
## vẽ biểu đồ tự động
df_HH$date = as.Date(paste(df_HH$d,df_HH$m,df_HH$y,sep ="-"),format = "%d-%m-%Y")
lplotr <- function(x, na.rm = TRUE, ...){
  sta_list = unique(x$mc)
  for (i in seq_along(sta_list)) {
    p <-ggplot(subset(x,x$mc == sta_list[i]),aes(date,value,colour = mc))
    plots = p + geom_line()+
      ggtitle(paste("Dien bien DO tại MC",sta_list[i],pa))+
      scale_y_continuous("unit(mg/l)")
    setwd(paste0(here::here(),"/figs/"))
    ggsave(plots,filename= paste("DO-MC",sta_list[i],"_",pa,".png",sep=""),width = 22, height = 11,scale = 0.5)
  }
}
lplotr(df_HH)

## lấy bảng lọc 
df = read_excel(paste0(here::here(),"/rawdata/","ds_Export_Mc_RU.xlsx"),sheet = 2)
df[] <- lapply(df, as.character)
df_HH[] <- lapply(df_HH, as.character)
df = left_join(df,df_HH,by = c("d","m","y","mc"))
df[] <- lapply(df, as.numeric)
df$id = paste(df$mc,df$m,df$d,df$h,sep = "_")
df = subset(df,select = c("id","mc","h","d","m","y","value"))
write.table(df, file = paste0(path_outwq,"/df_DO.csv"), sep = ",", row.names = F)


# Khai thác BOD
# Load danh sach tram ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df = read_excel(paste0(here::here(),"/rawdata/","ds_Export_Mc_RU.xlsx"),sheet = 1)
# Xuất DO tai MC ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
n = na.omit(as.character(df$do))
name = c("tt","h","d","m","y",n)
df_HH = read.table(paste0(here::here(),"/rawdata/sal_S/",pa,"/HD_SAL.BOD"),skip = 3,header = F)
colnames(df_HH) = name
## ghi ra bang full cua BOD
write.table(df_HH, file = paste0(path_outwq,"/df_BOD_full_",pa,".csv"), sep = ",", row.names = F)
df_HH$tt = NULL
df_HH = gather(df_HH,mc,value,-h,-d,-m,-y)

## vẽ biểu đồ tự động
df_HH$date = as.Date(paste(df_HH$d,df_HH$m,df_HH$y,sep ="-"),format = "%d-%m-%Y")
lplotr <- function(x, na.rm = TRUE, ...){
  sta_list = unique(x$mc)
  for (i in seq_along(sta_list)) {
    p <-ggplot(subset(x,x$mc == sta_list[i]),aes(date,value,colour = mc))
    plots = p + geom_line()+
      ggtitle(paste("Dien bien BOD tại MC",sta_list[i],pa))+
      scale_y_continuous("unit(mg/l)")
    setwd(paste0(here::here(),"/figs/"))
    ggsave(plots,filename= paste("BOD-MC",sta_list[i],"_",pa,".png",sep=""),width = 22, height = 11,scale = 0.5)
  }
}
lplotr(df_HH)

## lấy bảng lọc 
df = read_excel(paste0(here::here(),"/rawdata/","ds_Export_Mc_RU.xlsx"),sheet = 2)
df[] <- lapply(df, as.character)
df_HH[] <- lapply(df_HH, as.character)
df = left_join(df,df_HH,by = c("d","m","y","mc"))
df[] <- lapply(df, as.numeric)
df$id = paste(df$mc,df$m,df$d,df$h,sep = "_")
df = subset(df,select = c("id","mc","h","d","m","y","value"))
write.table(df, file = paste0(path_outwq,"/df_BOD.csv"), sep = ",", row.names = F)

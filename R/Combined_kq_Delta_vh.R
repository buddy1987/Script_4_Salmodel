#--------------------------------------------------------------------
# Title: Delta result combination
# Purpose: ghép và khai thác kết quả tại điểm chọn extraction
# Author:Nguyen Trung Nam
# Short description:
# Created: Wed Mar 14 04:43:25 2018
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
library(lubridate)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Section: Ghep các file chạy thành phần hour timeseries_HHH;QQQ;SSS
# Purpose:
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## THÔNG SỐ CHUNG
path_hd = paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/kq")
path_out = paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/kq_ghep")
pa = "PA0_dm"

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Section: Ghep các file HHHH
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
files = list.files(paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/kq"),pattern = "HD_SAL.HHH")[-1]
# Load danh sach tram ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df = read_excel(paste0(here::here(),"/rawdata/","ds_Export_Mc_RU.xlsx"),sheet = 1)
df_HH = ""
for (i in 1:length(files))
{
  df_H = read.table(paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/kq","/",files[i]),skip = 3,header = F)
  df_HH = rbind(df_HH,df_H)
}
## Lay ten tram do
n = na.omit(as.character(df$mc))
name = c("tt","h","d","m","y",paste0("mc",n))
colnames(df_HH) = name
df_HH$date = as.Date(paste(df_HH$d,df_HH$m,df_HH$y,sep = "-"), format = "%d-%m-%Y")
df_HH$h = as.numeric(df_HH$h)
df_HH = na.omit(df_HH)
hour(df_HH$date) = df_HH$h
df_HH = df_HH[order(df_HH$date),]
df_HH$date = NULL
df_HH$tt = NULL
df_HH[] <- lapply(df_HH, as.numeric)
write.table(df_HH, file = paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/kq_ghep","/","df_H_",pa,".csv"), sep = ",", row.names = F)
df_HH = gather(df_HH,mc,value,-h,-d,-m,-y)
df_HH = na.omit(df_HH)

# LỌC 10 DIỂM ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Lọc 10 điểm
df = read_excel(paste0(here::here(),"/rawdata/","ds_Export_Mc_RU.xlsx"),sheet = 4)
diem  = paste0("mc",df$MC)
df_HH = subset(df_HH,mc %in% diem)
## tinh max, min, tb daily 
df_d = df_HH %>%
  group_by(y,m,d,mc) %>%
  summarise_each (funs(min, max,mean), value)
colnames(df_d) = c("y","m","d","mc","min","ma","bq")
df_d = gather(df_d,dt,value,-mc,-d,-m,-y)
df_d$name = paste(df_d$mc,df_d$dt,sep = "-")
df_d$mc = NULL
df_d$dt = NULL
df_d = spread(df_d,name,value)
df_d$date = as.Date(paste(df_d$d,df_d$m,df_d$y,sep = "-"), format = "%d-%m-%Y")
df_d = df_d[,-c(1:4)]
write.table(df_d, file = paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/kq_ghep","/","df_H_daily",pa,".csv"), sep = ",", row.names = F)
## Vẽ biểu đồ đặc trưng
df_d = gather(df_d,mc,value,-date)
df_d = separate(df_d,mc,into = c("mc","type"))

lplotr <- function(x, na.rm = TRUE, ...){
  sta_list = unique(x$mc)
  for (i in seq_along(sta_list)) {
    p <-ggplot(subset(x,x$mc == sta_list[i]),aes(date,value,colour = type))
    plots = p + geom_line()+
      ggtitle(paste("Dien bien mực nước tại MC",sta_list[i],pa))+
      scale_y_continuous("unit(m)")
    setwd(paste0(here::here(),"/figs/"))
    ggsave(plots,filename= paste("10D_H",sta_list[i],"_",pa,".png",sep=""),width = 22, height = 11,scale = 0.5)
  }
}
lplotr(df_d)
## tinh max, min, tb thang
df_m = df_HH %>%
  group_by(y,m,mc) %>%
  summarise_each (funs(min,max,mean), value)
colnames(df_m) = c("y","m","mc","min","ma","bq")
df_m = gather(df_m,dt,value,-mc,-m,-y)
df_m$name = paste(df_m$mc,df_m$dt,sep = "-")
df_m$mc = NULL
df_m$dt = NULL
df_m = spread(df_m,name,value)
write.table(df_m, file = paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/kq_ghep","/","df_H_m",pa,".csv"), sep = ",", row.names = F)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Section: Ghep các file QQQ
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
files = list.files(path_hd,pattern = "HD_SAL.QQQ")
# Load danh sach tram ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df = read_excel(paste0(here::here(),"/rawdata/","ds_Export_Mc_RU.xlsx"),sheet = 1)
df_HH = ""
for (i in 1:length(files))
{
  df_H = read.table(paste0(path_hd,"/",files[i]),skip = 3,header = F)
  df_HH = rbind(df_HH,df_H)
}
## Lay ten tram do
n = na.omit(as.character(df$mc))
name = c("tt","h","d","m","y",paste0("mc",n))
colnames(df_HH) = name
df_HH$date = as.Date(paste(df_HH$d,df_HH$m,df_HH$y,sep = "-"), format = "%d-%m-%Y")
df_HH$h = as.numeric(df_HH$h)
df_HH = na.omit(df_HH)
hour(df_HH$date) = df_HH$h
df_HH = df_HH[order(df_HH$date),]
df_HH$date = NULL
df_HH$tt = NULL
df_HH[] <- lapply(df_HH, as.numeric)
write.table(df_HH, file = paste0(path_out,"/","df_Q_",pa,".csv"), sep = ",", row.names = F)
df_HH = gather(df_HH,mc,value,-h,-d,-m,-y)
df_HH = na.omit(df_HH)
# LỌC 10 DIỂM ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df = read_excel(paste0(here::here(),"/rawdata/","ds_Export_Mc_RU.xlsx"),sheet = 4)
diem  = paste0("mc",df$MC)
df_HH = subset(df_HH,mc %in% diem)
## tinh max, min, tb daily 
df_d = df_HH %>%
  group_by(y,m,d,mc) %>%
  summarise_each (funs(min, max,mean), value)
colnames(df_d) = c("y","m","d","mc","min","ma","bq")
df_d = gather(df_d,dt,value,-mc,-d,-m,-y)
df_d$name = paste(df_d$mc,df_d$dt,sep = "-")
df_d$mc = NULL
df_d$dt = NULL
df_d = spread(df_d,name,value)
df_d$date = as.Date(paste(df_d$d,df_d$m,df_d$y,sep = "-"), format = "%d-%m-%Y")
df_d = df_d[,-c(1:4)]
write.table(df_d, file = paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/kq_ghep","/","df_Q_daily",pa,".csv"), sep = ",", row.names = F)
## Vẽ biểu đồ đặc trưng
df_d = gather(df_d,mc,value,-date)
df_d = separate(df_d,mc,into = c("mc","type"))

lplotr <- function(x, na.rm = TRUE, ...){
  sta_list = unique(x$mc)
  for (i in seq_along(sta_list)) {
    p <-ggplot(subset(x,x$mc == sta_list[i]),aes(date,value,colour = type))
    plots = p + geom_line()+
      ggtitle(paste("Dien bien dòng chảy tại MC",sta_list[i],pa))+
      scale_y_continuous("unit(m3/s)")
    setwd(paste0(here::here(),"/figs/"))
    ggsave(plots,filename= paste("10D_Q",sta_list[i],"_",pa,".png",sep=""),width = 22, height = 11,scale = 0.5)
  }
}
lplotr(df_d)

## tinh max, min, tb thang
df_m = df_HH %>%
  group_by(y,m,mc) %>%
  summarise_each (funs(min,max,mean), value)
colnames(df_m) = c("y","m","mc","min","ma","bq")
df_m = gather(df_m,dt,value,-mc,-m,-y)
df_m$name = paste(df_m$mc,df_m$dt,sep = "-")
df_m$mc = NULL
df_m$dt = NULL
df_m = spread(df_m,name,value)
write.table(df_m, file = paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/kq_ghep","/","df_Q_m",pa,".csv"), sep = ",", row.names = F)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Section: Ghep các file HHHH
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
files = list.files(path_hd,pattern = "HD_SAL.SSS")
# Load danh sach tram ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df = read_excel(paste0(here::here(),"/rawdata/","ds_Export_Mc_RU.xlsx"),sheet = 1)
df_HH = ""
for (i in 1:length(files))
{
  df_H = read.table(paste0(path_hd,"/",files[i]),skip = 3,header = F)
  df_HH = rbind(df_HH,df_H)
}
## Lay ten tram do
n = na.omit(as.character(df$man))
name = c("tt","h","d","m","y",paste0("mc",n))
colnames(df_HH) = name
df_HH$date = as.Date(paste(df_HH$d,df_HH$m,df_HH$y,sep = "-"), format = "%d-%m-%Y")
df_HH$h = as.numeric(df_HH$h)
df_HH = na.omit(df_HH)
hour(df_HH$date) = df_HH$h
df_HH = df_HH[order(df_HH$date),]
df_HH$date = NULL
df_HH[] <- lapply(df_HH, as.numeric)
write.table(df_HH, file = paste0(path_out,"/","df_S_",pa,".csv"), sep = ",", row.names = F)
df_HH = gather(df_HH,mc,value,-h,-d,-m,-y)
df_HH = na.omit(df_HH)

# LỌC 10 DIỂM ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df = read_excel(paste0(here::here(),"/rawdata/","ds_Export_Mc_RU.xlsx"),sheet = 4)
diem  = paste0("mc",df$MC)
df_HH = subset(df_HH,mc %in% diem)
## tinh max, min, tb daily 
df_d = df_HH %>%
  group_by(y,m,d,mc) %>%
  summarise_each (funs(min, max,mean), value)
colnames(df_d) = c("y","m","d","mc","min","ma","bq")
df_d = gather(df_d,dt,value,-mc,-d,-m,-y)
df_d$name = paste(df_d$mc,df_d$dt,sep = "-")
df_d$mc = NULL
df_d$dt = NULL
df_d = spread(df_d,name,value)
df_d$date = as.Date(paste(df_d$d,df_d$m,df_d$y,sep = "-"), format = "%d-%m-%Y")
df_d = df_d[,-c(1:4)]
write.table(df_d, file = paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/kq_ghep","/","df_S_daily",pa,".csv"), sep = ",", row.names = F)
## Vẽ biểu đồ đặc trưng
df_d = gather(df_d,mc,value,-date)
df_d = separate(df_d,mc,into = c("mc","type"))

lplotr <- function(x, na.rm = TRUE, ...){
  sta_list = unique(x$mc)
  for (i in seq_along(sta_list)) {
    p <-ggplot(subset(x,x$mc == sta_list[i]),aes(date,value,colour = type))
    plots = p + geom_line()+
      ggtitle(paste("Dien bien Mặn tại MC",sta_list[i],pa))+
      scale_y_continuous("unit(g/l)")
    setwd(paste0(here::here(),"/figs/"))
    ggsave(plots,filename= paste("10D_S",sta_list[i],"_",pa,".png",sep=""),width = 22, height = 11,scale = 0.5)
  }
}
lplotr(df_d)
## tinh max, min, tb thang
df_m = df_HH %>%
  group_by(y,m,mc) %>%
  summarise_each (funs(min,max,mean), value)
colnames(df_m) = c("y","m","mc","min","ma","bq")
df_m = gather(df_m,dt,value,-mc,-m,-y)
df_m$name = paste(df_m$mc,df_m$dt,sep = "-")
df_m$mc = NULL
df_m$dt = NULL
df_m = spread(df_m,name,value)
write.table(df_m, file = paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/kq_ghep","/","df_S_m",pa,".csv"), sep = ",", row.names = F)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Section: Ghep các file chạy thành phần Sum: Hsum;Qsum;Ssum
# Purpose:
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GHÉP FILE HSUM
files = list.files(path_hd,pattern = "HD_SAL.HSUM")
# Load danh sach tram ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_HH = ""
for (i in 1:length(files))
{
  df_H = read.table(paste0(path_hd,"/",files[i]),skip = 2,header = F)
  df_HH = rbind(df_HH,df_H)
}
## Lay ten tram do
name = c("Nhanh","Mcat","Hma","Hbq","Hmi","XX","YY")
colnames(df_HH) = name
df_HH = na.omit(df_HH)
df_HH[] <- lapply(df_HH, as.numeric)
df_HH = df_HH %>% 
  group_by(Nhanh,Mcat) %>%
  summarise(Hbq = mean(Hbq), 
            Hmi = min(Hmi), 
            Hma = max(Hma))
df_HH = na.omit(df_HH)
write.table(df_HH, file = paste0(path_out,"/","df_HSum_",pa,".csv"), sep = ",", row.names = F)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GHÉP FILE SSUM
files = list.files(path_hd,pattern = "HD_SAL.SSUM")
# Load danh sach tram ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_HH = ""
for (i in 1:length(files))
{
  df_H = read.table(paste0(path_hd,"/",files[i]),skip = 2,header = F)
  df_HH = rbind(df_HH,df_H)
}
## Lay ten tram do
name = c("Nhanh","Mcat","Sma","Sbq","Smi","XX","YY")
colnames(df_HH) = name
df_HH = na.omit(df_HH)
df_HH[] <- lapply(df_HH, as.numeric)
df_HH = df_HH %>% 
  group_by(Nhanh,Mcat) %>%
  summarise(Sbq = mean(Sbq), 
            Smi = min(Smi), 
            Sma = max(Sma))
df_HH = na.omit(df_HH)
write.table(df_HH, file = paste0(path_out,"/","df_SSum_",pa,".csv"), sep = ",", row.names = F)



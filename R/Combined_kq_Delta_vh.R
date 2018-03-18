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
# Section: Ghep các file chạy thành phần hour timeseries_HHH;QQQ;SSS;NNN;PPP;OXY;BOD
# Purpose: Tính toán theo ngày, tháng, và vẽ biểu đồ daily
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## THÔNG SỐ CHUNG CẦN THAY ĐỔI KHI CHẠY CÁC pA KHÁC NHAU
tenpa = "Run_HDPA2.CLCBXR.VH2.2050"    # Tên thư mục phương án, chứa kq chạy
pa = "_PA2.CLCBXR.VH2.2050"       # Tên phương án, hay loại kết quả là HD, hay WQ sẽ add vao tên files

## Các path trong script
path_hd = paste0(here::here(),"/delta_sim/Simulation_HD_vh/",tenpa,"/kq")
path_out = paste0(here::here(),"/delta_sim/Simulation_HD_vh/","/kq_ghep")
path_raw = paste0(here::here(),"/rawdata/")
## danh sach cac file
ds = c("HHH","QQQ","OXY","BOD","NNN","PPP","SSS")
unit = c("m","m3/s","mg/l","mg/l","mg/l","mg/l","mg/l")

## Load danh sach tram ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_mc = read_excel(paste0(path_raw,"ds_Export_Mc_RU.xlsx"),sheet = 1)
df_10d = read_excel(paste0(path_raw,"ds_Export_Mc_RU.xlsx"),sheet = 4)

## vòng lặp
for (j in 1:length(ds))
{
files = list.files(path_hd,pattern = paste0("HD_SAL.",ds[j]))[-1]
df_HH = ""
for (i in 1:length(files))
{
  df_H = read.table(paste0(path_hd,"/",files[i]),skip = 3,header = F)
  df_HH = rbind(df_HH,df_H)
}
## Lay ten tram do
n = na.omit(as.character(df_mc$mc))
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
write.table(df_HH, file = paste0(path_out,"/",ds[j],pa,".csv"), sep = ",", row.names = F)
df_HH = gather(df_HH,mc,value,-h,-d,-m,-y)
df_HH = na.omit(df_HH)
## Lọc 10 điểm
df_10d = read_excel(paste0(path_raw,"ds_Export_Mc_RU.xlsx"),sheet = 4)
diem  = paste0("mc",df_10d$MC)
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
write.table(df_d, file = paste0(path_out,"/",paste0(ds[j],"_daily_"),pa,".csv"), sep = ",", row.names = F)
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
write.table(df_m, file = paste0(path_out,"/",paste0(ds[j],"_m_"),pa,".csv"), sep = ",", row.names = F)
}
####














### 
## Vẽ biểu đồ đặc trưng
df_d = gather(df_d,mc,value,-date)
df_d = separate(df_d,mc,into = c("mc","type"))
lplotr <- function(x, na.rm = TRUE, ...){
  sta_list = unique(x$mc)
  for (i in seq_along(sta_list)) {
    p <-ggplot(subset(x,x$mc == sta_list[i]),aes(date,value,colour = type))
    plots = p + geom_line()+
      ggtitle(paste("Diễn biến",ds[j],"tại MC",sta_list[i],"_",pa))+
      scale_y_continuous(paste0("Đơn vị (",unit[j],")"))
    setwd(paste0(here::here(),"/figs/"))
    ggsave(plots,filename= paste("10D_H",sta_list[i],"_",ds[j],"_",pa,".png",sep=""),width = 22, height = 11,scale = 0.5)
  }
}
lplotr(df_d)

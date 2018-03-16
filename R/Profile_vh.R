#--------------------------------------------------------------------
# Title: Extract kết quả dọc sông CL-CB theo từng thời đoạn vận hành
# Purpose:
# Author:Nguyen Trung Nam
# Short description:
# Created: Sun Jan 28 17:19:58 2018
#--------------------------------------------------------------------

# Library:+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(tidyr)
library(here)
library(readxl)
library(XLConnect)
library(zoo)
library(sp)
library(maptools)
library(rgdal)
library(rgeos)
library(sp)
library(geosphere)
library(dplyr)
library(plyr)
library(purrr)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Section: 
# Purpose:
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## THÔNG SỐ CHUNG CẦN THAY ĐỔI KHI CHẠY CÁC pA KHÁC NHAU
tenpa = "RUN_HDPA1"    # Tên thư mục phương án, chứa kq chạy
pa = "_PA1"       # Tên phương án, hay loại kết quả là HD, hay WQ sẽ add vao tên files

## Xác định số lần chạy
path_hd = paste0(here::here(),"/delta_sim/Simulation_HD_vh/",tenpa,"/kq")
lan = list.files(path_hd,pattern = ".SUM")
lan <- regmatches(lan, gregexpr("[[:digit:]]+", lan))
lan = unique(as.numeric(unlist(lan)))

## Các path trong script
path_hd = paste0(here::here(),"/delta_sim/Simulation_HD_vh/",tenpa,"/kq")
path_out = paste0(here::here(),"/delta_sim/Simulation_HD_vh/",tenpa,"/kq_ghep")
path_raw = paste0(here::here(),"/rawdata/")

## load danh sách 
df_ds = read_excel(paste0(path_raw,"ds_Export_Mc_RU.xlsx"),sheet = 3)
df_ts = read_excel(paste0(path_raw,"ds_Export_Mc_RU.xlsx"),sheet = 5)

## Xác định cac loai ket qua có trong thu muc
tso = list.files(path_hd,pattern = ".SUM")
find.list = c("HD_SAL.","_",seq(1:20),".txt")
find.string <- paste(unlist(find.list), collapse = "|")
tso = gsub(find.string, replacement = "", x = tso)
tso = unique(tso)

## Vòng lặp
for (i in 1:length(lan))
{
for (j in 1:length(tso))
{
k = ifelse(tso[j]=="BSUM",3,2)
df_s = read.table(paste0(path_hd,"/","HD_SAL.",tso[j],"_",lan[i],".txt"),skip = k,header = F)
ten = na.omit(subset(df_ts, select = tso[j]))
ten = unlist(ten,use.names = F)
colnames(df_s)  = ten
df1 = df_ds
df1 = left_join(df1,df_s,by = "Mcat")
df1 = df1[order(df1$song,df1$tt),]
write.table(df1, file = paste0(path_out,"/","Profile_",tso[j],"_",pa,"_",lan[i],".csv"), sep = ",", row.names = F)
 }
}
























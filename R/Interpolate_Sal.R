#--------------------------------------------------------------------
# Title: Nội suy ket qua dọc branh SAL model
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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Section: Nội suy theo mặt cắt, theo fixed distance or number of points
# Purpose:
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Nội suy điểm dựa trên mặt cắt ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df = read_excel(paste0(here::here(),"/rawdata/","Mc_Sal_4230.xls"),sheet = 1)
df = df[order(df$Nhanh,df$Chainage),]
dff = numeric()
df_t = numeric()
id_list = unique(df$Nhanh)
for (j in 1: length(id_list))
{
  df1 = subset(df,df$Nhanh == id_list[j])
  dff = numeric()
  nh = numeric()
  df_n = numeric()
  for (i in 1:(length(df1$Nhanh)-1))
  {
    df2 = seq(df1$Chainage[i],df1$Chainage[i+1], by = 1000)
    br  = unlist(rep(df1$Nhanh[i],time = length(df2)))
    dff = c(df2,dff)
    nh  = c(br,nh)
    df_n = data.frame(nh,dff)
  }
  df_t = rbind(df_n,df_t)
}

colnames(df_t) = c("Nhanh","Chainage")
df_n = subset(df,select = c("Nhanh","Chainage"))
df_t = unique(rbind(df_t,df_n))
df_t = df_t[order(df_t$Nhanh,df_t$Chainage),]
## lay id cua mat cat trong sal dua vao danh sach can noi suy
df_n = subset(df,select = c("Nhanh","Chainage","id"))
df_t = left_join(df_t,df_n, by = c("Nhanh","Chainage"))
colnames(df_t) = c("Nhanh","Chainage","id")

# Doc ket qua man sal vao ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_s = read.table(paste0(here::here(),"/rawdata/sal_S/","HD_SAL.SSUM"),skip = 1,header =T)
df_s$id = df$id  ## giả thuyết rằng trong model xuất ra theo đúng thứ tụ của shp mc
df_s  = subset(df_s,select = c("id","Sma"))
df_s$id = as.character(df_s$id)
df_t = left_join(df_t,df_s, by = c("id"))

# Filling na ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## tạo 3 vòng lặp for (i) tách thành từng block theo nhánh; (ii) tách sub-block theo cặp mặt cắt liền kề trong nhánh
## (iii) vòng lặp cho tính nội suy dự trên khoảng cách
df_t$id_n = seq(1,length(df_t$Nhanh))
dfid = na.omit(df_t)
dfff = numeric()
id_list = unique(dfid$Nhanh)
for (j in 1: length(id_list))
{
  df1 = subset(dfid,dfid$Nhanh == id_list[j])
  df_tt = numeric()
  dff = numeric()
  for (i in 1:(length(df1$id)-1))
  {
    df_tt = df_t[df1$id_n[i]:df1$id_n[i+1],]
    for (k in 1:(length(df_tt$id_n)-1))
    {
      df_tt$Sma[k+1] = df_tt$Sma[1] - ((df_tt$Sma[1]- df_tt$Sma[length(df_tt$id_n)])/df_tt$Chainage[length(df_tt$id_n)])* df_tt$Chainage[k+1]
    } 
    dff = rbind(df_tt,dff)
  }
  dfff = rbind(dff,dfff)
}
## do tach khoi nen khi gop vao bi duplicate nen can remove duplicated rows
dfff = unique(dfff)
dfff = dfff[order(dfff$Nhanh,dfff$Chainage),]
dfff$Sma = round(dfff$Sma,2)                                # thay ten cot
dfff$Chainage = round(dfff$Chainage,0)
## Chuyển chainge lẻ về số chẵn để join lấy tọa độ dễ dàng, tính cho gần đúng vì các điểm tọa độ có chainge chẵn
dfff$f = dfff$Chainage%%2==0
dfff$Chainage = ifelse(dfff$f == FALSE,(dfff$Chainage-1),dfff$Chainage)
dfff$f = NULL

# Lấy tọa độ cho các điểm filled gap và tao shp ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
dfxy = readRDS(paste0(here::here(),"/output/dfxy.rds"))
### lây tọa độ cho các điểm
df = left_join(dfff,dfxy, by = c("Nhanh","Chainage"))
df = subset(df,select = c("x","y","Nhanh","Chainage","Sma","id"))
df = subset(df,df$x > 0)
### tao shp cho file
xy <- df[,c(1,2)]
coords = xy
sp = SpatialPoints(coords)
spdf <- SpatialPointsDataFrame(sp,data = df)
Utm48N = " +proj=utm +zone=48 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(spdf) = CRS(Utm48N)
### ghi ra shapefile
writeOGR(spdf, dsn = paste0(here::here(),"/output"), "Kq_S_N_L14_BoRU_kin", "ESRI Shapefile",overwrite_layer= TRUE)
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
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Load danh sach tram ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df = read_excel(paste0("d:/Trung Nam/SIWRP/A Dung/Cailon_caibe/Simulation_delta/input_new","/ds_Export_Mc_RU.xlsx"),
                sheet = 1)

# Xuất mực nước giờ tại Hydro station ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_HH = read.table(paste0("d:/Trung Nam/SIWRP/A Dung/Cailon_caibe/Simulation_delta/test","/HD_SAL.HHH"),
                   skip = 3,header = F)
## Lay ten tram do
n = na.omit(as.character(df$mc))
name = c("tt","h","d","t","y",paste0("mc",n))
colnames(df_HH) = name
write.table(df_HH, file = "d:/Trung Nam/SIWRP/A Dung/Cailon_caibe/Simulation_delta/output/df_HHH.csv", sep = ",", row.names = F)


# Xuất LƯU LƯƠNG giờ tại Hydro station ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_HH = read.table(paste0("d:/Trung Nam/SIWRP/A Dung/Cailon_caibe/Simulation_delta/test","/HD_SAL.QQQ"),
                   skip = 3,header = F)
## Lay ten tram do
n = na.omit(as.character(df$mc))
name = c("tt","h","d","t","y",paste0("mc",n))
colnames(df_HH) = name
#df_HH = df_HH[,1:6]
write.table(df_HH, file = "d:/Trung Nam/SIWRP/A Dung/Cailon_caibe/Simulation_delta/output/df_QQQ.csv", sep = ",", row.names = F)


# Xuất MỰC NƯỚC TGIỜ TAi Ruộng ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_HH = read.table(paste0("d:/Trung Nam/SIWRP/A Dung/Cailon_caibe/Simulation_delta/test","/HD_SAL.HPL"),
                   skip = 2,header = F)
## Lay ten tram do
n = na.omit(as.character(df$ru))
name = c("tt","h","d","t","y",paste0("HPL",n))
colnames(df_HH) = name
write.table(df_HH, file = "d:/Trung Nam/SIWRP/A Dung/Cailon_caibe/Simulation_delta/output/df_HPL.csv", sep = ",", row.names = F)


# Xuất Dòng chảy giờ tại Ruộng ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_HH = read.table(paste0("d:/Trung Nam/SIWRP/A Dung/Cailon_caibe/Simulation_delta/test","/HD_SAL.QPL"),
                   skip = 2,header = F)
## Lay ten tram do
n = na.omit(as.character(df$ru))
name = c("tt","h","d","t","y",paste0("QPL",n))
colnames(df_HH) = name
write.table(df_HH, file = "d:/Trung Nam/SIWRP/A Dung/Cailon_caibe/Simulation_delta/output/df_QPL.csv", sep = ",", row.names = F)







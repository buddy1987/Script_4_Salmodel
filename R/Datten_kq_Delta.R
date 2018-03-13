library(tidyr)
library(here)
library(stringr)
library(readxl)
library(dplyr)
# GHI FILE KET QUA ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Xac định tự động thứ thự file fvao dang chạy
path_hd = "d:/Repository/Script_4_Salmodel/delta_sim/Simulation_HD_vh/Run_HD/kq"
path_d  = "D:/Repository/Script_4_Salmodel"
fvao = list.files(path_hd,pattern = "Fvao")[-1]
fvao <- regmatches(fvao, gregexpr("[[:digit:]]+", fvao))
fvao = as.numeric(unlist(fvao))
name = min(fvao)-1

## Dọc tự động danh sách và ghi theo thư tự của Fvao_lần chạy
df = read_excel(paste0(path_d,"/rawdata/","Vanhanh_delta.xlsx"),sheet = 3)

for (i in 1:length(df$ten))
{
  tx = readLines(paste0(path_hd,"/",df$ten[i]))
  writeLines(tx, con = paste0(path_hd,df$ten[i],"_",name,".txt"))
}
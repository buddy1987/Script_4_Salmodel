library(tidyr)
library(here)
library(stringr)
library(readxl)
library(dplyr)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Section: Tự động load Fvao lần thứ n và chạy
# Purpose:
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
fvao = list.files(paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/kq"),pattern = "HD_SAL.HHH")
fvao <- regmatches(fvao, gregexpr("[[:digit:]]+", fvao))
fvao = as.numeric(unlist(fvao))
name = max(fvao)+1
fvao = readLines(paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/","Fvao_",name,".txt"))
writeLines(fvao, con = paste0(here::here(),"/delta_sim/Simulation_HD_vh/Run_HD/","Fvao.txt"))
## end











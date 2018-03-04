library(here)
## directory contains various files with function definitions 
dir.create(paste0(here(),"/R"))
df = data.frame("Thu muc nay chua script R")
write.table(df, file  = paste0(here(),"/R/readme.txt"), col.names = F, row.names = F)
rm(df)
## directory contains data used in the analysis and read only
dir.create(paste0(here(),"/rawdata"))
df = data.frame("Du lieu tho khong sua du lieu nay")
write.table(df, file  = paste0(here(),"/rawdata/readme.txt"), col.names = F, row.names = F)
rm(df)
## directory contains the paper, word or Latex or Rmarkdown
dir.create(paste0(here(),"/doc"))
df = data.frame("Chua bao cao, Rmarkdown")
write.table(df, file  = paste0(here(),"/doc/readme.txt"), col.names = F, row.names = F)
rm(df)
## directory contains the paper, word or Latex or Rmarkdown
dir.create(paste0(here(),"/figs"))
df = data.frame("Chua hinh anh,chart")
write.table(df, file  = paste0(here(),"/figs/readme.txt"), col.names = F, row.names = F)
rm(df)
## directory contains simuation output, processed datasets, logs
dir.create(paste0(here(),"/output"))
df = data.frame("chua table, va san pham dau ra")
write.table(df, file  = paste0(here(),"/output/readme.txt"), col.names = F, row.names = F)
rm(df)
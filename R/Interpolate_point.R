#--------------------------------------------------------------------
# Title: NỘI SUY DỌC SEGMENT T
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
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Section: Nội suy theo segment
# Purpose:
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df = read_excel(file.choose(),sheet = 1)
## 2 vòng lặp for loop, giải pháp này chưa tối ưu khi kích thước dataframe lớn
dff = numeric()
df1 = numeric()
id_list = unique(df$id)
for (j in 1: length(id_list))
  {
  df1 = subset(df,df$id == id_list[j])
for (i in 1:(length(df1$id)-1))
{
  df1$h[i+1] = df1$h[1] - ((df1$h[1]- df1$h[length(df1$id)])/df1$dis[length(df1$id)])* df1$dis[i+1]
  
}
  dff = rbind(df1,dff)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Section:
# Purpose:
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
proj4string  <- CRS("+proj=utm +zone=48 +datum=WGS84 +units=m +no_defs")
line_seg = maptools::readShapeSpatial(paste0(here::here(),"/rawdata/nhanh_bdcm.shp"),proj4string)


## Function to generate random point with distance around fixed distance
sample.line <- function(x, sdist = 5)
{
  if (!require(sp)) stop("sp PACKAGE MISSING")
  if (!inherits(x, "SpatialLinesDataFrame")) stop("MUST BE SP SpatialLinesDataFrame OBJECT")
  lgth <- SpatialLinesLengths(x) 
  lsub <- x[1,]
  ns <- round( (lgth[1] / sdist), digits=0)
  lsamp <- spsample(lsub, n=ns, type="regular", offset=c(0.5,0.5))
  data = x@data[1,]
  df = as.data.frame(lapply(data, rep, ns))
  results <- SpatialPointsDataFrame(lsamp, df, match.ID = TRUE)
  
  for (i in 2:dim(x)[1] ) 
  {    
    lsub <- x[i,]
    ns <- round( (lgth[i] / sdist), digits=0)
    lsamp <- spsample(lsub, n=ns, type="regular")
    data = x@data[i,]
    df = as.data.frame(lapply(data, rep, ns))
    lsamp <- SpatialPointsDataFrame(lsamp, df, match.ID = TRUE)
    results <- rbind(results, lsamp)     
  }
  ( results )
}

line_point <- sample.line(line_seg, sdist= 100) 

plot(lv_point)   
### get point coordination
line_point@data$x = coordinates(lv_point)[,1]
line_point@data$y = coordinates(lv_point)[,2]


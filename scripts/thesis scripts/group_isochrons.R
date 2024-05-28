library(raster)
library(sf)
library(tidyverse)
library(tmap)
library(beepr)

# identify polygons from the same fire ####
setwd("E:/chapter3/isochrons")
isochrons = st_read("isochrons_snapped_qgis.gpkg")
nrow(isochrons)
## 121,432
isochrons$ID = c(1:nrow(isochrons))
targetcrs = st_crs(isochrons)
# isochrons$area_check = st_area(isochrons)
## measurements are the same, do not appear to be cumulative- each polygon is measured separately; the variable "area" is in km
isochrons$time = as.POSIXct(isochrons$time)
isochrons$lasttim = as.POSIXct(isochrons$lasttim)
isochrons$progtime = difftime(isochrons$time, isochrons$lasttim, units = "hours")
isochrons$progtime = as.numeric(isochrons$progtime)

isochrons = isochrons |> 
  dplyr::select(ID, time, lasttim, area, progtime)

startiso = isochrons |> 
  filter(progtime == 0)
nrow(startiso)
## 4,333

# touching_list = st_touches(isochrons)
# saveRDS(touching_list, "touchinglist.rds")
touching_list = readRDS("touchinglist.rds")
## index = ID

isochrons$fireID = isochrons$ID
for(i in c(1:nrow(startiso))){
  firstorder = isochrons[unlist(touching_list[startiso$ID[i]]),] |>  
    filter(time == lasttim)
  isochrons$fireID[isochrons$ID %in% firstorder$ID] = startiso$ID[i]
}
length(unique(isochrons$fireID))
## 121,325 (removed 107 unique fire ID's)

isofires = isochrons
iso = list()
i = 1
for(i in c(1:nrow(startiso))){
  t.x = startiso[i,]
  fireID.temp = t.x$ID
  
  fires = list()
  fires[[1]] = t.x
  IDlist = list()
  IDlist[[1]] = t.x$ID
  
  # use while() to recursively identify adjoining fire polygons. When new polygons are identified they are codified as "t.x".
  
  check = 2
  while(nrow(t.x) > 0){
    t2.x = list()
    IDcheck = 2
    for(x in 1:nrow(t.x)){
      t2 = isofires[unlist(touching_list[t.x$ID[x]]),]
      t2 = t2[!(t2$ID %in% unlist(IDlist)),]
      t2 = t2[t2$lasttim == t.x$time[x],] 
      t2.x[[x]] = t2
      fires[[check]] = t2
      IDlist[[IDcheck]] = t2$ID
      check = check + 1
      IDcheck = IDcheck + 1
      print(paste0(fireID.temp, " = ", length(unique(IDlist[[1]])), ", iter ", x))
    }
    t.x = bind_rows(t2.x)
    st_geometry(t.x) = NULL
    t.x = unique(t.x)
    IDlist = list(sort(unique(unlist(IDlist))))
    isofires$fireID[isofires$ID %in% t.x$ID] = fireID.temp
    
  }
  iso.temp = bind_rows(fires)
  iso.temp$fireID = fireID.temp
  iso[[i]] = iso.temp
}
iso2 = bind_rows(iso)
st_write(iso2, "isochrons_fireID.gpkg", delete_dsn = T)
nrow(iso2)
## 1,806,579

# # check results ####
# setwd("E:/chapter3/isochrons")
# iso2 = st_read("isochrons_fireID.gpkg")
# nrow(iso2)
# ## 1,806,579
# 
# length(unique(iso2$ID))
# ## 121,432
# length(unique(iso2$fireID))
# ## 4,333
# 
# iso.temp = iso2 |> filter(ID == unique(iso2$ID)[1])
# iso.temp
# ## unique ID's belong to different fireID's
# 
# iso.temp = iso2
# st_geometry(iso.temp) = NULL
# tally.temp = iso.temp |> group_by(ID) |> tally()
# summary(tally.temp)
# ## from 1 to 185 replicates
# 
# iso.temp = iso2 |> 
#   filter(ID == tally.temp$ID[tally.temp$n == max(tally.temp$n)][1])
# iso.temp
# 
# fire.temp = iso2 |> 
#   filter(fireID %in% iso.temp$fireID)
# 
# st_geometry(fire.temp) = NULL
# tally.temp = fire.temp |> group_by(ID) |> tally()
# summary(tally.temp)
# ## still between 1 and 185 replicates
# 
# i = 1
# iso.temp = iso2 |> 
#   filter(ID == i)
# 
# fire.temp = iso2 |> 
#   filter(fireID %in% iso.temp$fireID)
# fire.temp = fire.temp[order(fire.temp$ID),]
# 
# fire.temp[fire.temp$fireID == 24,] == fire.temp[fire.temp$fireID == 26,]
# ## all columns exactly the same except fireID
# 
# i = 2
# iso.temp = iso2 |> 
#   filter(ID == i)
# iso.temp
# 
# fire.temp = iso2 |> 
#   filter(fireID %in% iso.temp$fireID)
# fire.temp = fire.temp[order(fire.temp$ID),]
# 
# fire.temp[fire.temp$fireID == 34,] == fire.temp[fire.temp$fireID == 135,]
# ## ERROR
# 
# tm_shape(fire.temp |> filter(fireID == 34)) + tm_polygons(col = "darkblue") +
#   tm_shape(fire.temp |> filter(fireID == 135)) + tm_polygons(col = "darkred")
# ## looking at adjacent polygons from different fireID's, it appears there was simply an error somewhere. These fires should be the same
# 
# i = 2687
# iso.temp = iso2 |> 
#   filter(ID == i)
# iso.temp
# ## this ID belongs to a whopping 185 fireID's
# 
# fire.temp = iso2 |> 
#   filter(fireID %in% iso.temp$fireID)
# fire.temp = fire.temp[order(fire.temp$ID),]
# fire.temp
# nrow(fire.temp)
# ## 78,809!!
# length(unique(fire.temp$ID))
# ## 3,045!!
# 
# firesub = unique(fire.temp$fireID[1:10])
# fire.temp1 = fire.temp |> 
#   filter(fireID %in% firesub)
# nrow(fire.temp1)
# ## 1,257
# 
# tm_shape(fire.temp1) + tm_polygons(col = "fireID")
# # almost all ID's belong to a single (3,954), except some ID's that belong to 3,212
# 
# fire1 = fire.temp1 |> 
#   filter(fireID == 3954)
# fire2 = fire.temp1 |> 
#   filter(fireID == 3212)
# 
# tm_shape(fire1) + tm_polygons(col = "darkblue") +
#   tm_shape(fire2) + tm_polygons(col = "darkred")
# ## these should also be all one fire

# merge duplicates ####
setwd("E:/chapter3/isochrons")
iso2 = st_read("isochrons_fireID.gpkg")
nrow(iso2)
## 1,806,579

ids = unique(iso2$ID)
length(ids)
## 121,432
fires = list()
while(length(ids) > 0){
  print(ids[1])
  iso.temp = iso2 |>
    filter(ID == ids[1])
  
  fire.temp = iso2 |>
    filter(fireID %in% iso.temp$fireID)
  n1 = nrow(fire.temp)
  ## 30
  fire.temp = iso2 |>
    filter(ID %in% fire.temp$ID)
  n2 = nrow(fire.temp)
  ## 66
  
  while(n1 != n2){
    fire.temp = iso2 |>
      filter(fireID %in% fire.temp$fireID)
    n1 = nrow(fire.temp)
    ## 30
    fire.temp = iso2 |>
      filter(ID %in% fire.temp$ID)
    n1 = n1 + nrow(fire.temp)
    ## 66
    fire.temp = iso2 |>
      filter(fireID %in% fire.temp$fireID)
    n2 = nrow(fire.temp)
    ## 30
    fire.temp = iso2 |>
      filter(ID %in% fire.temp$ID)
    n2 = n2 + nrow(fire.temp)
    ## 66
  }
  
  fire.temp = fire.temp[order(fire.temp$ID),]
  fire.temp$fireID = unique(fire.temp$fireID)[1]
  
  fires[[ids[1]]] = fire.temp[!duplicated(fire.temp$ID),]
  ids = ids[!(ids %in% fire.temp$ID)]
}
beep()
fires.unique = bind_rows(fires)
nrow(fires.unique)
## 877.366
length(unique(fires.unique$ID))
## 121,432
all(unique(iso2$ID) %in% fires.unique$ID)
## TRUE
any(duplicated(fires.unique$ID))
## TRUE
st_write(fires.unique, "isochrons_fireID_grouped.gpkg", delete_dsn = T)

# merge small fires ####
setwd("E:/chapter3/isochrons")
isochrons = st_read("isochrons_fireID_grouped.gpkg")
nrow(isochrons)
## 121,432
length(unique(isochrons$fireID))
## 767
nrow(isochrons |> filter(area < 1))
## 69,099
nrow(isochrons |> filter(area < 0.5))
## 52,899
summary(isochrons$progtime[isochrons$area < 1])
## mean = 82 but there are definitely fires with shorter progression times
backup = isochrons
iso = isochrons
iso$progtime2 = iso$progtime

touching_list = readRDS("touchinglist.rds")

ids = iso$ID[iso$area < 1]
ids = unique(iso$fireID[iso$ID %in% ids])
length(ids)
## 493
i = ids[1]
for(i in ids){
  iso.temp = iso |> 
    filter(fireID == i)
  id.temp = iso.temp$ID[iso.temp$area < 1]
  
  while(length(id.temp) > 0){
    a = id.temp[1]
    print(a)
    iso.temp1 = iso.temp |> 
      filter(ID == a)
    touch.temp = isochrons[touching_list[[a]],]
    if(nrow(touch.temp) > 0){
      pre.temp = touch.temp |> filter(time == iso.temp1$lasttim)
      if(nrow(pre.temp) > 0){
        pre.temp2 = iso.temp1|> 
          dplyr::select(geom) |> 
          st_union(pre.temp)
        pre.temp2$area = pre.temp$area + iso.temp1$area
        pre.temp2$progtime2 = pre.temp$progtime + iso.temp1$progtime
        iso[iso$ID == unique(pre.temp$ID),] = pre.temp2
        iso = iso |> 
          filter(ID != a)
      }
    }
    id.temp = id.temp[id.temp != a]
  }
}
nrow(iso)
## 117,862
length(unique(iso$ID))
## 117,800
length(unique(iso$fireID))
## 765
summary(iso)
iso$progtime2[iso$progtime == 0] = 0

dups = iso$ID[duplicated(iso$ID)]
length(unique(dups))
## 61
iso.list = list()
for(i in dups){
  iso.temp = iso |> 
    filter(ID == i)
  iso.temp$progtime = max(iso.temp$progtime)
  iso.temp$progtime2 = max(iso.temp$progtime2)
  iso.temp$area = max(iso.temp$area)
  iso.list[[i]] = iso.temp[!duplicated(iso.temp$ID),]
}
iso.list = bind_rows(iso.list)
nrow(iso.list)
## 61
length(unique(iso.list$ID))
## 61

iso = iso[!(iso$ID %in% dups),]
nrow(iso)
## 117,739
length(unique(iso$ID))
## 117,739

iso = rbind(iso, iso.list)
nrow(iso)
## 117,800
length(unique(iso$ID))
## 117,800
st_write(iso, "isochrons_fireID_merged.gpkg", delete_dsn = T)


library(raster)
library(sf)
library(tidyverse)
library(tmap)
library(FNN)
library(beepr)

# calculate spread direction for adjacent polygons- ending polygons ####
setwd("E:/chapter3/isochrons")
iso = st_read("isochrons_fireID_grouped.gpkg")
nrow(iso)
## 121,432
targetcrs = st_crs(iso)

adj_list = readRDS("touchinglist.rds")

tmap_mode("view")

i = unique(iso$fireID)[3]
isolist.ind = list()
isolist.merged = list()
counter = 1
for(i in unique(iso$fireID)){
  iso.temp = iso |> 
    filter(fireID == i)
  
  if(nrow(iso.temp) > 0){
    # using the convex hull, create lines extending from the convex hull border to the nearest point on the previous fire progression convex hull
    # identify the longest line from each combination of polygons. This is the direction of fire spread
    iso.start = list()
    iso.a = list()
    iso.b = list()
    ids = unique(iso.temp$ID[iso.temp$progtime != 0])
    while(length(ids) > 0){
      list.index = counter
      a = ids[1]
      
      # for each starting polygon, identify each timestep increase in fire area
      poly2 = iso.temp |> 
        filter(ID == a)
      poly2 = rbind(poly2, iso.temp[iso.temp$ID %in% unlist(adj_list[poly2$ID]),] |> 
                      filter(time == poly2$time & lasttim == poly2$lasttim))
      
      poly1.temp = iso.temp[iso.temp$ID %in% unlist(adj_list[poly2$ID]),] |> 
        filter(time == poly2$lasttim) 
      # time of final poly2 measurement is the same as previous polygon time recorded for poly1
      ## in other words poly2 time leads to poly1 lasttim
      
      if(nrow(poly1.temp) > 0){
        poly2.alt = iso.temp[iso.temp$ID %in% unlist(adj_list[poly1.temp$ID]),] |> 
          filter(lasttim %in% poly1.temp$time)
        
        # tm_shape(iso.temp) + tm_polygons() +
        #   tm_shape(poly1.temp) + tm_polygons(col = "darkblue") +
        #   tm_shape(poly2.alt) + tm_polygons(col = "red") +
        #   tm_shape(poly2) + tm_polygons(col = "darkred")
        
        poly1 = st_union(poly1.temp) |> 
          st_as_sf()
        st_geometry(poly1.temp) = NULL
        poly1 = cbind(poly1, poly1.temp[1,])
        names(poly1)[names(poly1) == "x"] = "geom"
        st_geometry(poly1) = "geom"
        
        poly1$ID = counter
        counter = counter + 1
        poly1$area = as.numeric(st_area(poly1))
        
        # remove any starting polygons that have no shared fireline
        fireline.final = 0
        if(nrow(poly2.alt) > 1){
          for(x in c(1:nrow(poly2.alt))){
            pre.line = st_cast(st_intersection(poly1, poly2.alt[x,]))
            fireline = as.numeric(sum(st_length(pre.line)))
            if(fireline == 0){
              poly2.alt = poly2.alt[-x,]
            }
            fireline.final = fireline.final + fireline
          }
        }
        poly2 = poly2.alt
        poly2 = st_union(poly2)
        
        st_geometry(poly2.alt) = NULL
        poly2 = cbind(poly2, poly2.alt[1,])
        names(poly2)[names(poly2) == "geometry"] = "geom"
        st_geometry(poly2) = "geom"
        
        poly2$ID = counter
        counter = counter + 1
        poly2$area = as.numeric(st_area(poly2))
        
        # union geometries between starting polygon and next polygon
        poly2.mrgd = poly1 |> 
          dplyr::select(geom) |> 
          st_union() |> 
          st_union(poly2) |> 
          st_as_sf()
        
        poly2.mrgd = cbind(poly2.mrgd, poly2.alt[1,])
        names(poly2.mrgd)[names(poly2.mrgd) == "x"] = "geom"
        st_geometry(poly2.mrgd) = "geom"
        
        poly2.mrgd$ID = unique(poly2$ID)
        poly2.mrgd$area = as.numeric(st_area(poly2.mrgd))
        
        ids = ids[!ids %in% poly2.alt$ID]
        iso.alt = do.call("rbind", replicate(length(poly2.alt$ID), poly2.mrgd, simplify = FALSE))
        iso.alt$ID = poly2.alt$ID
        iso.temp[iso.temp$ID %in% poly2.alt$ID,] = iso.alt
        
        # tm_shape(iso.temp) + tm_polygons() +
        #   tm_shape(poly2.mrgd) + tm_polygons(col = "darkred") +
        #   tm_shape(poly2) + tm_polygons(col = "red") +
        #   tm_shape(poly1) + tm_polygons(col = "darkblue")
        
        # for each timestep in fire growth, convert polygons to points and calculate a convex hull for each
        poly1.pt = st_cast(poly1, "POINT")
        poly1.poly = as.matrix(st_coordinates(poly1.pt))
        ch <- chull(poly1.poly)
        ch <- poly1.poly[c(ch, ch[1]), ]
        poly1.poly = st_as_sf(as.data.frame(ch), coords = c("X", "Y"), crs = targetcrs) |> 
          summarise(geom = st_combine(geometry)) %>%
          st_cast("POLYGON")
        
        poly2.pt = st_cast(poly2.mrgd, "POINT")
        poly2.poly = as.matrix(st_coordinates(poly2.pt))
        ch <- chull(poly2.poly)
        ch <- poly2.poly[c(ch, ch[1]), ]
        poly2.poly = st_as_sf(as.data.frame(ch), coords = c("X", "Y"), crs = targetcrs) |> 
          summarise(geom = st_combine(geometry)) %>%
          st_cast("POLYGON")
        
        # tm_shape(iso.temp) + tm_polygons() +
        #   tm_shape(poly2.poly) + tm_polygons(col = "darkred") +
        #   tm_shape(poly1.poly) + tm_polygons(col = "darkblue") +
        #   tm_shape(poly2.mrgd) + tm_polygons(col = "red") +
        #   tm_shape(poly1) + tm_polygons(col = "blue")
        
        if(!st_contains(poly1.poly, poly2.poly, sparse = F)){
          # sample points on  polygon edge 100 m apart at most
          poly1.pt = poly1.poly |> 
            st_segmentize(dfMaxLength = 100) %>% 
            st_coordinates() %>% 
            as.data.frame() %>% 
            select(X, Y) %>% 
            st_as_sf(coords = c("X", "Y"), crs = targetcrs)
          poly2.pt = poly2.poly |> 
            st_segmentize(dfMaxLength = 100) %>% 
            st_coordinates() %>% 
            as.data.frame() %>% 
            select(X, Y) %>% 
            st_as_sf(coords = c("X", "Y"), crs = targetcrs)
          
          tryCatch({
            # calculate minimum distance from 1) points on second polygon back to 2) points on starting polygon
            c<-get.knnx(as.matrix(st_coordinates(poly1.pt)), as.matrix(st_coordinates(poly2.pt)), k = 1)
            c.index = c[[1]]
            c.dist = c[[2]]
            c.df = data.frame(index = c[[1]],
                              distance = c[[2]])
            c.df |> 
              filter(distance == max(c.df$distance))
            which(c.df$distance == max(c.df$distance))
            
            max2 = poly2.pt[which(c.df$distance == max(c.df$distance)),] |> 
              distinct()
            max1 = poly1.pt[c.df$index[c.df$distance == max(c.df$distance)],] |> 
              distinct()
            
            sp.dir = unique(as.numeric(nngeo::st_azimuth(max1, max2)))
            poly2$spread.dir = sp.dir
            poly2$prepolyIDs = list(poly1$ID)
            poly2$fireline = fireline.final
            
            poly2.mrgd$spread.dir = sp.dir
            poly2.mrgd$prepolyIDs = list(poly1$ID)
            poly2.mrgd$fireline = fireline.final
            
            # linecheck1 = poly1.pt[c.df$index,]
            # linecheck = list()
            # for(l in c(1:nrow(c.df))){
            #   linecheck2 = rbind(linecheck1[l,], poly2.pt[l,])
            #   if(linecheck2[1,] != linecheck2[2,]){
            #     linecheck[[l]] = linecheck2 |>
            #       group_by() |>
            #       summarize() |>
            #       st_cast("LINESTRING")
            #   }
            # }
            # linecheck = bind_rows(linecheck)
            # linecheck$ID = c(1:nrow(linecheck))
            # 
            # tm_shape(iso.temp) + tm_polygons() +
            #   tm_shape(poly2.mrgd) + tm_polygons(col = "darkred") +
            #   tm_shape(poly2.pt) + tm_dots(col = "red") +
            #   tm_shape(poly1.pt) + tm_dots(col = "blue") +
            #   tm_shape(poly1) + tm_polygons(col = "darkblue") +
            #   tm_shape(max2) + tm_dots() +
            #   tm_shape(max1) + tm_dots() +
            #   tm_shape(linecheck) + tm_lines(col = "ID")
          }, error = function(e){print(a); cat("ERROR :", conditionMessage(e), "\n")})
          
          iso.a[[list.index]] = poly2
          iso.b[[list.index]] = poly2.mrgd
          
        } else {
          print(paste0(a, ": starting convex hull contains following polygon"))
        }
      } else {
        print(paste0(a, ": no preceding polygon"))
      }
      
      ids = ids[ids != a]
    }
    beep()
    
    # starting polygons
    iso.start = iso.temp |> 
      filter(progtime == 0)
    iso.start$spread.dir = NA
    iso.start$prepolyIDs = NA
    iso.start$fireline = NA
    
    # unmerged progression polygons
    iso.a = bind_rows(iso.a)
    iso.a = rbind(iso.start, iso.a)
    
    # merged progression polygons
    iso.b = bind_rows(iso.b)
    iso.b = rbind(iso.start, iso.b)
    
    # tm_shape(iso.temp) + tm_polygons(col = "black") + 
    #   tm_shape(iso.b) + tm_polygons(col = "darkred") +
    #   tm_shape(iso.a) + tm_polygons(col = "red") +
    #   tm_shape(iso.start) + tm_polygons()
    
    isolist.ind[[i]] = iso.a
    isolist.merged[[i]] = iso.b
  }
}
isolist.ind = bind_rows(isolist.ind)
saveRDS(isolist.ind, "isolist_ind.rds")
isolist.merged = bind_rows(isolist.merged)
saveRDS(isolist.merged, "isolist_merged.rds")

# # calculate spread direction for adjacent polygons- starting polygons ####
# setwd("E:/chapter3/isochrons")
# iso = st_read("isochrons_fireID_grouped.gpkg")
# nrow(iso)
# ## 121,432
# targetcrs = st_crs(iso)
# 
# adj_list = readRDS("touchinglist.rds")
# 
# i = unique(iso$fireID)[1]
# isolist.ind = list()
# isolist.merged = list()
# counter = 1
# for(i in unique(iso$fireID)){
#   ## *** I might not want to start with starting polygons. This is resulting in missing polygons and they may be getting removed somewhere (example: ID = 142). Instead, I should start with the latest polygons and work backwards
#   # for each fire, identify starting polygons
#   iso.temp = iso |> 
#     filter(fireID == i)
#   
#   start.temp = iso.temp |> 
#     filter(progtime == 0)
#   
#   # tm_shape(iso.temp) + tm_polygons() + 
#   #   tm_shape(start.temp) + tm_polygons(col = "darkblue")
#   
#   # using the convex hull, create lines extending perpendicularly from the convex hull border to the next fire progression convex hull. Lines should be cut off at these boundaries
#   # identify the longest line from each combination of polygons. This is the direction of fire spread
#   iso.start = list()
#   iso.a = list()
#   iso.b = list()
#   ids = unique(iso.temp$ID)
#   while(length(ids) > 0){
#     list.index = counter
#     
#     if(length(unique(start.temp$ID)) > 0){
#       a = unique(start.temp$ID)[1]
#     } else {
#       a = ids[1]
#     }
#     
#     # for each starting polygon, identify each timestep increase in fire area
#     start.a = iso.temp |> 
#       filter(ID == a)
#     start.a = rbind(start.a, iso.temp[iso.temp$ID %in% unlist(adj_list[start.a$ID]),] |> 
#       filter(time == start.a$time & lasttim == start.a$lasttim))
#     
#     adj.start.temp = iso.temp[iso.temp$ID %in% unlist(adj_list[start.a$ID]),] |> 
#       filter(lasttim == start.a$time)
#     
#     # figure out if there are any following polygons. Some will be the last in their fire ID
#     if(nrow(adj.start.temp[adj.start.temp$progtime != 0,]) > 0){
#       # for each increase, identify other possible starting polygons that are adjacent to it
#       adj.start.temp = adj.start.temp |> 
#         filter(time == min(adj.start.temp$time[adj.start.temp$progtime != 0]))
#       start.alt = iso.temp[iso.temp$ID %in% unlist(adj_list[adj.start.temp$ID]),] |> 
#         filter(time %in% adj.start.temp$lasttim)
#       
#       adj.start = st_union(adj.start.temp) |> 
#         st_as_sf()
#       st_geometry(adj.start.temp) = NULL
#       adj.start = cbind(adj.start, adj.start.temp[1,])
#       names(adj.start)[names(adj.start) == "x"] = "geom"
#       st_geometry(adj.start) = "geom"
#       
#       adj.start$ID = counter
#       counter = counter + 1
#       adj.start$area = as.numeric(st_area(adj.start))
#       
#       # tm_shape(iso.temp) + tm_polygons() +
#       #   tm_shape(start.a) + tm_polygons(col = "darkblue") +
#       #   tm_shape(adj.start) + tm_polygons(col = "darkred") +
#       #   tm_shape(start.alt) + tm_polygons(col = "blue")
# 
#       # remove any starting polygons that have no shared fireline
#       if(nrow(start.alt) > 1){
#         for(x in c(1:nrow(start.alt))){
#           pre.line = st_cast(st_intersection(adj.start, start.alt[x,]))
#           fireline = as.numeric(sum(st_length(pre.line)))
#           if(fireline == 0){
#             start.alt = start.alt[-x,]
#           }
#         }
#       }
#       start.a = start.alt
#       start.a = st_union(start.a)
#       
#       st_geometry(start.alt) = NULL
#       start.a = cbind(start.a, start.alt[1,])
#       names(start.a)[names(start.a) == "geometry"] = "geom"
#       st_geometry(start.a) = "geom"
#       
#       start.a$ID = counter
#       counter = counter + 1
#       start.a$area = as.numeric(st_area(start.a))
#       
#       # union geometries between starting polygon and next polygon
#       adj.next = start.a |> 
#         dplyr::select(geom) |> 
#         st_union() |> 
#         st_union(adj.start) |> 
#         st_as_sf()
#       
#       adj.next = cbind(adj.next, adj.start.temp[1,])
#       names(adj.next)[names(adj.next) == "x"] = "geom"
#       st_geometry(adj.next) = "geom"
#       
#       adj.next$ID = unique(adj.start$ID)
#       adj.next$area = as.numeric(st_area(adj.next))
#       
#       # tm_shape(iso.temp) + tm_polygons() +
#       #   tm_shape(adj.next) + tm_polygons(col = "darkred") +
#       #   tm_shape(adj.start) + tm_polygons(col = "red") +
#       #   tm_shape(start.a) + tm_polygons(col = "darkblue")
#       
#       # for each timestep in fire growth, convert polygons to points and calculate a convex hull for each
#       start.pt = st_cast(start.a, "POINT")
#       start.poly = as.matrix(st_coordinates(start.pt))
#       ch <- chull(start.poly)
#       ch <- start.poly[c(ch, ch[1]), ]
#       start.poly = st_as_sf(as.data.frame(ch), coords = c("X", "Y"), crs = targetcrs) |> 
#         summarise(geom = st_combine(geometry)) %>%
#         st_cast("POLYGON")
#       
#       adj.pt = st_cast(adj.next, "POINT")
#       adj.poly = as.matrix(st_coordinates(adj.pt))
#       ch <- chull(adj.poly)
#       ch <- adj.poly[c(ch, ch[1]), ]
#       adj.poly = st_as_sf(as.data.frame(ch), coords = c("X", "Y"), crs = targetcrs) |> 
#         summarise(geom = st_combine(geometry)) %>%
#         st_cast("POLYGON")
#       
#       tm_shape(iso.temp) + tm_polygons() +
#         tm_shape(adj.poly) + tm_polygons(col = "darkred") +
#         tm_shape(start.poly) + tm_polygons(col = "darkblue") +
#         tm_shape(adj.next) + tm_polygons(col = "red") +
#         tm_shape(start.a) + tm_polygons(col = "blue")
#       
#       if(!st_contains(start.poly, adj.poly, sparse = F)){
#         # sample points on  polygon edge 100 m apart at most
#         start.pt = start.poly |> 
#           st_segmentize(dfMaxLength = 100) %>% 
#           st_coordinates() %>% 
#           as.data.frame() %>% 
#           select(X, Y) %>% 
#           st_as_sf(coords = c("X", "Y"), crs = targetcrs)
#         adj.pt = adj.poly |> 
#           st_segmentize(dfMaxLength = 100) %>% 
#           st_coordinates() %>% 
#           as.data.frame() %>% 
#           select(X, Y) %>% 
#           st_as_sf(coords = c("X", "Y"), crs = targetcrs)
#         
#         tryCatch({
#           # calculate minimum distance from 1) points on second polygon back to 2) points on starting polygon
#           c<-get.knnx(as.matrix(st_coordinates(start.pt)), as.matrix(st_coordinates(adj.pt)), k = 1)
#           c.index = c[[1]]
#           c.dist = c[[2]]
#           c.df = data.frame(index = c[[1]],
#                             distance = c[[2]])
#           c.df |> 
#             filter(distance == max(c.df$distance))
#           which(c.df$distance == max(c.df$distance))
#           
#           max2 = adj.pt[which(c.df$distance == max(c.df$distance)),] |> 
#             distinct()
#           max1 = start.pt[c.df$index[c.df$distance == max(c.df$distance)],] |> 
#             distinct()
#           
#           sp.dir = unique(as.numeric(nngeo::st_azimuth(max1, max2)))
#           adj.start$spread.dir = sp.dir
#           adj.start$prepolyIDs = list(start.alt$ID)
#           
#           adj.next$spread.dir = sp.dir
#           adj.next$prepolyIDs = list(start.alt$ID)
#           
#           # linecheck1 = start.pt[c.df$index,]
#           # linecheck = list()
#           # for(l in c(1:nrow(c.df))){
#           #   linecheck2 = rbind(linecheck1[l,], adj.pt[l,])
#           #   if(linecheck2[1,] != linecheck2[2,]){
#           #     linecheck[[l]] = linecheck2 |>
#           #       group_by() |>
#           #       summarize() |>
#           #       st_cast("LINESTRING")
#           #   }
#           # }
#           # linecheck = bind_rows(linecheck)
#           # linecheck$ID = c(1:nrow(linecheck))
#           # 
#           # tm_shape(iso.temp) + tm_polygons() +
#           #   tm_shape(adj.pt) + tm_dots(col = "red") +
#           #   tm_shape(start.pt) + tm_dots(col = "blue") +
#           #   tm_shape(start.a) + tm_polygons(col = "darkblue") +
#           #   tm_shape(adj.next) + tm_polygons(col = "darkred") +
#           #   tm_shape(max2) + tm_dots() +
#           #   tm_shape(max1) + tm_dots() +
#           #   tm_shape(linecheck) + tm_lines(col = "ID")
#         }, error = function(e){print(a); cat("ERROR :", conditionMessage(e), "\n")})
#         
#         if(length(unique(start.temp$ID)) > 0){
#           iso.start[[list.index]] = start.a
#         }
#         iso.a[[list.index]] = adj.start
#         iso.b[[list.index]] = adj.next
#         
#       } else {
#         print(paste0(a, ": starting convex hull contains following polygon"))
#       }
#     } else {
#       print(paste0(a, ": polygon is the last in its series"))
#     }
#     
#     if(length(unique(start.temp$ID)) > 0){
#       start.temp = start.temp |> 
#         filter(ID != a)
#     }
#     ids = ids[ids != a]
#   }
#   beep()
#   iso.start = bind_rows(iso.start)
#   iso.start$spread.dir = NA
#   iso.start$prepolyIDs = NA
#   
#   iso.a = bind_rows(iso.a)
#   iso.a = rbind(iso.start, iso.a)
#   
#   iso.b = bind_rows(iso.b)
#   iso.b = rbind(iso.start, iso.b)
#   
#   tm_shape() + tm_polygons() + 
#     tm_shape(iso.b) + tm_polygons(col = "darkred") +
#     tm_shape(iso.a) + tm_polygons(col = "red") + 
#     tm_shape(iso.start) + tm_polygons()
#   
#   isolist.ind[[i]] = iso.a
#   isolist.merged[[i]] = iso.b
# }
# isolist.ind = bind_rows(isolist.ind)
# isolist.merged = bind_rows(isolist.merged)
# 
# # calculate fireline length ####
# startiso = isochrons |> 
#   filter(progtime == 0)
# nrow(startiso)
# ## 4,333
# 
# # touching_list = st_touches(isochrons)
# # saveRDS(touching_list, "touchinglist.rds")
# touching_list = readRDS("touchinglist.rds")
# ## index = ID
# 
# isochrons$fireID = isochrons$ID
# for(i in c(1:nrow(startiso))){
#   firstorder = isochrons[unlist(touching_list[startiso$ID[i]]),] |>  
#     filter(time == lasttim)
#   isochrons$fireID[isochrons$ID %in% firstorder$ID] = startiso$ID[i]
# }
# length(unique(isochrons$fireID))
# ## 121,325 (removed 107 unique fire ID's)
# 
# isofires = isochrons
# iso = list()
# i = 1
# for(i in c(1:nrow(startiso))){
#   t.x = startiso[i,]
#   fireID.temp = t.x$ID
#   
#   fires = list()
#   fires[[1]] = t.x
#   IDlist = list()
#   IDlist[[1]] = t.x$ID
#   
#   # use while() to recursively identify adjoining fire polygons. When new polygons are identified they are codified as "t.x".
#   
#   check = 2
#   while(nrow(t.x) > 0){
#     t2.x = list()
#     IDcheck = 2
#     for(x in 1:nrow(t.x)){
#       t2 = isofires[unlist(touching_list[t.x$ID[x]]),]
#       t2 = t2[!(t2$ID %in% unlist(IDlist)),]
#       t2 = t2[t2$lasttim == t.x$time[x],] 
#       t2.x[[x]] = t2
#       fires[[check]] = t2
#       IDlist[[IDcheck]] = t2$ID
#       check = check + 1
#       IDcheck = IDcheck + 1
#       print(paste0(fireID.temp, " = ", length(unique(IDlist[[1]])), ", iter ", x))
#     }
#     t.x = bind_rows(t2.x)
#     st_geometry(t.x) = NULL
#     t.x = unique(t.x)
#     IDlist = list(sort(unique(unlist(IDlist))))
#     isofires$fireID[isofires$ID %in% t.x$ID] = fireID.temp
#     
#   }
#   iso.temp = bind_rows(fires)
#   iso.temp$fireID = fireID.temp
#   iso[[i]] = iso.temp
# }
# iso2 = bind_rows(iso)
# st_write(iso2, "isochrons_fireID.gpkg", delete_dsn = T)
# 
# tm_shape(iso[iso$fireID == 867,]) + tm_polygons()
# 
# if(nrow(touch.temp) > 0){
#   touch.temp = isochrons[touching_list[[i]],]
# }
# 
# if(iso.temp$progtime != 0){
#   pre.temp = touch.temp |> filter(time == iso.temp$lasttim)
#   if(nrow(pre.temp) == 0){
#     print(paste0(i, " isochron does not overlap with any polygons with matching timestamps"))
#     iso.temp$prepolyID = NA
#     iso.temp$fireline = NA
#     iso.temp$spreaddir = NA
#     iso[[i]] = iso.temp
#   } else {
#     
#     pre.line = st_cast(st_intersection(iso.temp, pre.temp))
#     iso.temp$fireline = sum(st_length(pre.line))
#     pre.line = st_cast(pre.line, "POINT")
#     
#     if(nrow(pre.line) == 1){
#       pre.centroid = pre.line
#     } else if(nrow(pre.line) < 4){
#       pre.poly = rbind(pre.line, pre.line[nrow(pre.line):1,])
#       
#       pre.centroid = pre.poly |> 
#         summarise(geom = st_combine(geom)) %>%
#         st_cast("POLYGON") |> 
#         st_centroid()
#       
#     } else if(nrow(pre.line) >= 4){
#       pre.centroid = pre.line |> 
#         summarise(geom = st_combine(geom)) %>%
#         st_cast("POLYGON") |> 
#         st_centroid()
#     }
#     
#     pre.centroid = st_transform(pre.centroid, crs = 4326)
#     iso.temp$spreaddir = as.numeric(mean((nngeo::st_azimuth(pre.centroid, iso.centroid))))
#     
#     if(nrow(pre.temp) == 1){
#       iso.temp$prepolyID = pre.temp$ID
#     } else if(nrow(pre.temp) > 1){
#       iso.temp$prepolyID = list(pre.temp$ID)
#     }
#     iso[[i]] = iso.temp
#   } 
# }
# # }
# 
# iso2 = list()
# for(i in 1:length(iso)){
#   iso.temp = iso[[i]]
#   iso.temp$prepolyID = paste(unlist(iso.temp$prepolyID), collapse = ', ')
#   iso2[[i]] = iso.temp
# }
# iso2 = bind_rows(iso2)
# nrow(iso2)
# # 121,431
# setwd("E:/chapter3/isochrons")
# st_write(iso2, "isochrons_withfireline.gpkg", delete_dsn = T)
# nrow(iso2)
# ## 1,806,579
# 
# length(unique(iso2$ID))
# ## 121,432
# length(unique(iso2$fireID))
# ## 4,333
# 
# iso3 = list()
# for(i in unique(iso2$fireID)){
#   iso.temp = iso2 |> 
#     filter(fireID == i)
#   iso3[[i]] = distinct(iso.temp)
# }
# iso3 = bind_rows(iso3)
# nrow(iso3)
# ##
# length(unique(iso3$ID))
# ## 
# length(unique(iso3$fireID))
# ## 
# 
# iso2 = st_read("isochrons_withfireline.gpkg")
# all(unique(iso2$ID) %in% unique(isochrons$ID))
# ## FALSE
# length(unique(iso2$ID)) == length(unique(isochrons$ID))
# ## FALSE
# 
# missing = isochrons |> filter(!(ID %in% iso2$ID))
# all(missing$time == missing$lasttim)
# ## TRUE
# ## so all missing ID's from the original dataset correspond to first fire polygons
# 
# st_geometry(iso2) = NULL
# iso2 = iso2 |> 
#   filter(!is.na(fireline) & progtime != 0)
# nrow(iso2)
# ## 117,099
# cor(iso2 |> dplyr::select(area, fireline), method = "spearman")
# ## 0.85
# 
# isochrons = backup
# isochrons$prog = isochrons$area/isochrons$progtime
# isochrons$spread = isochrons$area/isochrons$progtime/isochrons$fireline
# nrow(isochrons |> filter(is.infinite(spread)))
# ## 3,032
# ## may just have to remove these
# summary(isochrons |> filter(is.infinite(spread)))
# any(duplicated(isochrons$ID[is.infinite(isochrons$spread)]))
# ## FALSE
# 
# isochrons$fireline = as.numeric(isochrons$fireline)
# isochrons$fireline[isochrons$fireline == 0] = 1
# isochrons$spread = isochrons$area/isochrons$progtime/isochrons$fireline
# summary(isochrons)
# 
# isochrons = isochrons |>
#   filter(progtime <= 24)
# nrow(isochrons)
# ## 70,370
# # st_write(isochrons, "isochrons_withfireline_f1.gpkg", delete_dsn = T)
# st_write(isochrons, "isochrons_withfireline_withspreaddir.gpkg", delete_dsn = T)
# st_geometry(isochrons) = NULL
# isochrons = isochrons |> 
#   filter(!is.na(fireline))
# cor(isochrons |> dplyr::select(area, fireline, spread), method = "spearman")
# ## 0.89, 0.65, 0.45
# 
# iso3 = st_read("isochrons_gedi_m1.gpkg")
# backup2 = iso3
# st_geometry(iso3) = NULL
# iso3 = iso3 |> 
#   filter(!is.na(fireline))
# nrow(iso3)
# ## 70,370
# cor(iso3 |> dplyr::select(area, fireline, spread), method = "spearman")
# ## 0.89, 0.65, 0.45
# ## looks the same
# 
# nans = isochrons |> filter(is.na(spreaddir) & !is.na(spread))
# nrow(nans)
# ## 2,925
# 
# iso4 = st_read("isochrons_snapped_qgis.gpkg")
# iso4$ID = c(1:nrow(iso4))
# 
# tm_shape(nans) + tm_polygons()
# tm_shape(nans[5,]) + tm_polygons(col = "red") + tm_shape(iso4 |> filter(ID == 135)) + tm_polygons()
# ## all isochrons with NaN spreaddir is because the polygon is surrounded by the previous polygon- so the centerpoint for the fireline is actually inside the fire polygon
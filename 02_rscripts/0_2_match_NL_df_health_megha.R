library(tidyverse)
library(sf)
library(ipumsr)

#0. LOADING FINAL DF, GPS DATA FROM IPUMS AND NL ACTIVITY FROM SHRUG#######

df_final_withouth_NA <- readRDS("01_tidy_data/resid_clean_megha.rds")

#geographical data
sf_dis_point <- st_read("00_raw_data/ipums/dhs/2015/geo_shp/IAGE71FL.shp") %>% filter(LATNUM != 0)%>%
  inner_join(df_final_withouth_NA, by = c("DHSCLUST" = "CLUSTERNO")) 

shapefile_points <- st_transform(sf_dis_point, crs = "+proj=longlat +datum=WGS84")
shapefile_buffer <- st_buffer(shapefile_points, dist = 10000)

c <- st_read("00_raw_data/shrug/geometries_shrug-v1.5.samosa-open-polygons-shp/village.shp")

c_bis <- c %>% filter(pc11_s_id == 17)
shapefile_polygons <- st_transform(c_bis, crs = st_crs(shapefile_points))

intersection <- st_intersection(shapefile_polygons, shapefile_buffer)




shrug_key <- read.csv("00_raw_data/shrug/csv_shrug-v1.5.samosa-nl-csv/shrug-v1.5.samosa-keys-csv/shrug_pc11_subdistrict_key.csv") %>% 
  filter(pc11_state_id == 17)

shrug_night_light_polygon <- read.csv("00_raw_data/shrug/csv_shrug-v1.5.samosa-nl-csv/shrug-v1.5.samosa-nl-csv/shrug_nl_wide.csv") %>%
  select(total_light2013, shrid, num_cells) %>%
  inner_join(shrug_key) %>% 
  rename(pc11_sd_id= pc11_subdistrict_id)%>% select(-shrid)%>%
  mutate(total_light_cell = total_light2013/num_cells)%>%
  group_by(pc11_sd_id)%>% #compute NL by subdistrict 
  mutate(total_light2013_sd = sum(total_light_cell)) %>% ungroup() %>%
  select(-total_light_cell, -total_light2013, -num_cells) %>%
  inner_join(c) %>% unique()%>% 
  st_as_sf() #on a un df avec NL per subdistrict + polygon
  # as_Spatial()# for computing misclassification rate 

#function to compute misclassification rate#####

Sys.setenv("_SP_EVOLUTION_STATUS_"=2)

point_in_polygon_fun<-function(Observed_DHS_Points,Polygon,Polygon_Values,
                               Rural_Code,n_Approximation,NA_Option){
  #Packages
  require(spatstat)
  require(splancs)
  require(fields)
  Observed_DHS_Points<-
    spTransform(Observed_DHS_Points, "+proj=utm +zone=36 +datum=WGS84")
  Polygon<-spTransform(Polygon, "+proj=utm +zone=36 +datum=WGS84")
  #Assigning Offset Distances
  n<-length(Observed_DHS_Points)
  65
  #Typical Case
  if(Rural_Code==0){
    offset.dist<-ifelse(Observed_DHS_Points$URBAN_RURA=="U", 2000, 5000)
  }
  #Worst Case Scenario
  if(Rural_Code==1){
    offset.dist<-ifelse(Observed_DHS_Points$URBAN_RURA=="U", 2000, 10000)
  }
  final<-matrix(0,nrow=n,ncol=3)
  for(i in 1:n){
    #Creating Buffer Around Point with Maximum Offset as Radius
    pdsc<-disc(radius = offset.dist[i], centre = c(coordinates(Observed_DHS_Points)[i,1],
                                                   coordinates(Observed_DHS_Points)[i,2]))
    pdsc<-as(pdsc, "SpatialPolygons")
    proj4string(pdsc) <- CRS("+proj=utm +zone=36 +datum=WGS84")
    #Filling in the Buffer with Points
    rpt<-csr(pdsc@polygons[[1]]@Polygons[[1]]@coords, n_Approximation)
    rpt<-SpatialPoints(rpt)
    proj4string(rpt)<-CRS("+proj=utm +zone=36 +datum=WGS84")
    #Which Region is each Point in?
    ov<-over(rpt, Polygon)
    ov<-ov[is.na(ov[,1])==0,] #Removing Points Outside of the Polygon
    proportions<-matrix(0,nrow=length(unique(ov[,1])),ncol=2)
    for(j in 1:length(unique(ov[,1]))){
      #Proportion of Points in this Region
      proportions[j,1]<-mean(ov[,1]==unique(ov[,1])[j])
      #Polygon Value for the Proportion
      proportions[j,2]<-Polygon_Values[unique(ov[,1])[j]]
    }
    #Determining Polygon of Observed Point
    ov_point<-over(Observed_DHS_Points[i,], Polygon)
    
    #Leave Missing Polygon Values as Missing
    if(NA_Option==0){
      #Continous Polygon Value
      final[i,1]<-proportions[,1]%*%proportions[,2]
      #Discrete Polygon Value
      final[i,2]<-proportions[proportions[,1]==max(proportions[,1]),2]
    }
    #Reweighting the Non-Missing Polygon Values
    if(NA_Option==1){
      #Removing the Missing Observations
      proportions_1<-proportions[is.na(proportions[,2])==0,1]
      proportions_2<-proportions[is.na(proportions[,2])==0,2]
      if(length(proportions_1)>0){
        
        #Reweighting the Proportions
        proportions_1<-proportions_1/sum(proportions_1)
        #Continous Polygon Value
        final[i,1]<-proportions_1%*%proportions_2
        #Discrete Polygon Value
        final[i,2]<-proportions_2[proportions_1==max(proportions_1)]
      }
      if(length(proportions_1)==0){
        #Continous Polygon Value
        final[i,1]<-NA
        #Discrete Polygon Value
        final[i,2]<-NA
      }
    }
    #Probability of Missclassification
    final[i,3]<-1-proportions[(unique(ov[,1])==ov_point[,1]),1]
    #Completion Percentage
    print(c("Percent Complete", 100*round(i/n,2)))
  }
  return(final)
}

#test 

# point_in_polygon_fun(sf_dis_point, shrug_night_light_polygon)

#map 
# 
# shrug_night_light %>% ggplot() +  geom_sf(aes(fill = total_light2013)) + 
#   geom_point(data=df_fin, aes(LONGNUM , LATNUM, color = as_factor(OWNHOUSEWHO)), size = 1) +  
#   theme_void() +  theme(legend.position = "right") + labs(colour= "Gender of the house owner \nin the household", 
#                                                            fill = "Night-light luminosity \n(2013)") 
# 
# ggsave("03_plots/map_india_gender_house_owning.png")
# 
# 


#on veut faire correspondre les points avec les polygones 

resid_clean_megha_NL <- st_join(sf_dis_point, shrug_night_light_polygon) 

saveRDS(object = resid_clean_megha_NL, file = "01_tidy_data/resid_clean_megha_NL.rds")


library(tidyverse)
library(sf)
library(scales)
library(haven)
library(ipumsr)
library(ggrepel)

#0. LOADING DATA IPUMS 2015 HOUSEHOLD MEMBERS
region_polygon <- st_read("00_raw_data/ipums/dhs_ipumsi_ia/dhs_ipumsi_ia.shp") 
  filter(LATNUM != 0) 
  # select(ADM1DHS, ADM1NAME) %>% unique() %>%
  # mutate(ADM1NAME = str_replace(ADM1NAME, "NCT of ", ""), 
  #        ADM1NAME = str_replace(ADM1NAME, "&", "and"))

df_final_withouth_NA <- readRDS("01_tidy_data/resid_clean_india.rds") %>%
  select(IDHSHID, OWNHOUSEWHO, GEO_IA2015) %>% 
  filter(OWNHOUSEWHO %in% c(1, 2, 3)) %>%
  mutate(GEO_IA2015 = as_factor(GEO_IA2015), 
         GEO_IA2015 = as.character(GEO_IA2015), 
         GEO_IA20152 = case_when(GEO_IA2015 %in% c("Goa", "Daman", "Diu") ~ "Goa, Daman and Diu", 
                                 GEO_IA2015 %in% c("Uttar Pradesh", "Uttarakhand") ~ "Uttar Pradesh, Uttaranchal", 
                                 GEO_IA2015 %in% c("Odisha") ~ "Orissa", 
                                 GEO_IA2015 %in% c("Madhya Pradesh", "Chhattisgarh") ~ "Madhya Pradesh, Chhattisgarh", 
                                 GEO_IA2015 %in% c("Puducherry") ~ "Pondicherry", 
                                 GEO_IA2015 %in% c("Bihard", "Jharkhand") ~ "Bihar, Jharkhand", 
                                 TRUE ~ GEO_IA2015))%>%
  group_by(GEO_IA20152) %>%
  mutate(count = n_distinct(IDHSHID), 
         OWNHOUSEWHO = as_factor(OWNHOUSEWHO)) %>% 
  group_by(GEO_IA20152, OWNHOUSEWHO) %>%
  summarise(n = n_distinct(IDHSHID), count = count) %>% ungroup() %>% unique() %>%
  mutate(percent = 100*n/ count) %>%
  inner_join(region_polygon, by = c("GEO_IA20152" = "ADMIN_NAME")) %>%
  st_as_sf()
            
df_final_withouth_NA %>% 
  filter(OWNHOUSEWHO== "Female member") %>% 
    ggplot() +  
  geom_sf(aes(fill = percent)) + 
  theme_void() +  
  theme(legend.position = "bottom") + 
  geom_label_repel(data = df_final_withouth_NA %>% filter(GEO_IA20152 == "Meghalaya", 
                                                          OWNHOUSEWHO== "Female member"),
                   aes(label = GEO_IA20152, geometry = geometry), nudge_y = -1.5,  
                   stat = "sf_coordinates",
        min.segment.length = 0)+ 
  labs(fill = "Percentage of households \nwith female ownership")

#1. 

#combien de foyers néolocaux en ville vs. campagne? 

count_rural_residence_indiv_level <- df_final_withouth_NA %>% select(IDHSHID, residence_indiv_level, URBANHH, OWNHOUSEWHO, AGLANDWHO, URBANHH) %>% 
  unique()%>%
  group_by(residence_indiv_level, AGLANDWHO) %>% summarise(n = n())

count_district_residence_indiv_level <- df_final_withouth_NA %>% select(IDHSHID, residence_indiv_level, URBANHH, OWNHOUSEWHO, AGLANDWHO, URBANHH, 
                                                                        GEOALT_IA2015) %>% unique()%>%
  group_by(residence_indiv_level, GEOALT_IA2015) %>% summarise(n = n()) %>% group_by(GEOALT_IA2015) %>%
  summarise(percent = 100*n/sum(n), residence_indiv_level = residence_indiv_level)

#where are located households where property is owned by male members? 
#3. MAP
# vec_DHSID <- df_final_withouth_NA$DHSID %>% unique()

df_fin <- inner_join(sf_dis, df_final_withouth_NA, by = c("DHSCLUST" = "CLUSTERNO"))%>%
  mutate(khasi = case_when(DHSREGCO %in% c(296, 298) ~ "Khasi", 
                           T ~ "other"))

sf_state %>% ggplot() +  geom_sf(aes(fill = total_light2013)) + 
  geom_point(data=df_fin, aes(LONGNUM , LATNUM, color = as_factor(OWNHOUSEWHO)), size = 1) +  
  theme_void() +  theme(legend.position = "right") + labs(colour= "Gender of the house owner \nin the household", 
                                                          fill = "Night-light luminosity \n(2013)") 

ggsave("03_plots/map_india_gender_house_owning.png")






df_fin <- inner_join(sf_dis, df_final_withouth_NA, by = c("DHSCLUST" = "CLUSTERNO"))%>%
  mutate(khasi = case_when(DHSREGCO %in% c(296, 298) ~ "Khasi", 
                           T ~ "other"))

sf_state %>% ggplot() +  geom_sf(aes(fill = total_light2013)) + 
  geom_point(data=df_fin, aes(LONGNUM , LATNUM, color = as_factor(OWNHOUSEWHO)), size = 1) +  
  theme_void() +  theme(legend.position = "right") + labs(colour= "Gender of the house owner \nin the household", 
                                                          fill = "Night-light luminosity \n(2013)") 

ggsave("03_plots/map_india_gender_house_owning.png")





library(tidyverse)
library(sf)

###SOCIO ECONOMIC DATA MEGHALAYA

#1_LOADINGG_DATA#########
key <- read.csv("/Users/loialamarque/Dropbox/Thèse/data/shrug/csv_shrug-v1.5.samosa-pop-econ-census-csv/shrug-v1.5.samosa-keys-csv/shrug_pc11_district_key.csv")
nl <- read.csv("/Users/loialamarque/Dropbox/Thèse/data/shrug/csv_shrug-v1.5.samosa-nl-csv/shrug-v1.5.samosa-nl-csv/shrug_nl_wide.csv")%>%
  select(shrid, total_light2011, num_cells) %>% mutate(avg_light = total_light2011/num_cells)
pc_11_all_india <- read.csv("/Users/loialamarque/Dropbox/Thèse/data/shrug/csv_shrug-v1.5.samosa-pop-econ-census-csv/shrug-v1.5.samosa-pop-econ-census-csv/shrug_pc11.csv")%>%
  select(pc11_pca_tot_p_r, pc11_pca_tot_p_u, shrid, pc11_pca_tot_p)

secc <- read.csv("/Users/loialamarque/Dropbox/Thèse/data/shrug/csv_shrug-v1.5.samosa-secc-csv/shrug-v1.5.samosa-secc-csv/shrug_secc.csv")

sf_state <- st_read("/Users/loialamarque/Dropbox/Thèse/data/shrug/geometries_shrug-v1.5.samosa-open-polygons-shp/state.shp")

sf_dis <- st_read("/Users/loialamarque/Dropbox/Thèse/data/shrug/geometries_shrug-v1.5.samosa-open-polygons-shp/district.shp")
  
#2_STAT_ALL INDIA
big_df_meghalaya <- nl %>% inner_join(pc_11_all_india) %>% inner_join(secc) %>% inner_join(key) %>%
  replace_na(list(pc11_pca_tot_p_r = 0, pc11_pca_tot_p_u = 0, pc11_pca_tot_p =0))%>%
  mutate(pc11_state_id = as.factor(pc11_state_id),  
         pc11_district_id = as.factor(pc11_district_id))%>%
  group_by(pc11_state_name)%>%
  summarise(rur = round(100*(sum(pc11_pca_tot_p_r)/sum(pc11_pca_tot_p)), 2), 
            poverty_rural = mean(secc_pov_rate_rural, na.rm = T), 
            poverty_urban = mean(secc_pov_rate_urban, na.rm = T), 
            household_rural = mean(num_members_mean_rural, na.rm = T), 
            household_urban = mean(num_members_mean_urban, na.rm = T), 
            total_nightlight = round(mean(avg_light, na.rm = T), 2)) %>%
  drop_na()%>%
  summarise(household = round((household_rural + household_urban)/2, 2),
            poverty = round((poverty_rural+poverty_urban)/2, 2), 
            pc11_state_name = pc11_state_name,  rur =rur, 
            total_nightlight = total_nightlight)%>%
  filter(pc11_state_name != "") 

write.csv(big_df_meghalaya, "/Users/loialamarque/Dropbox/Thèse/outputs/stat_state.csv" )

#3_MAP
big_df_meghalaya_map <- key %>% select(pc11_state_name, pc11_state_id)%>% unique() %>%
  inner_join(big_df_meghalaya)%>% mutate(pc11_state_id = as.factor(pc11_state_id))

sf <- inner_join(big_df_meghalaya_map, sf_state, by =c("pc11_state_id" = "pc11_s_id"))%>%
  st_as_sf()

sf %>%
  ggplot() + geom_sf(aes(fill = total_nightlight)) + theme_void() + 
  theme(legend.position = "bottom") + labs(fill = "Average nightlights") 
ggsave("/Users/loialamarque/Dropbox/Thèse/outputs/map_house_hold.png")


#3_

big_df_meghalaya_bis <- nl %>% inner_join(pc_11_all_india) %>% inner_join(secc) %>% inner_join(key) %>%
  replace_na(list(pc11_pca_tot_p_r = 0, pc11_pca_tot_p_u = 0, pc11_pca_tot_p =0))%>%
  mutate(pc11_state_id = as.factor(pc11_state_id),  
         pc11_district_id = as.factor(pc11_district_id))%>%
  filter(pc11_state_name == "meghalaya")%>%
  group_by(pc11_district_name)%>%
  summarise(rur = round(100*(sum(pc11_pca_tot_p_r)/sum(pc11_pca_tot_p)), 2), 
            poverty_rural = mean(secc_pov_rate_rural, na.rm = T), 
            poverty_urban = mean(secc_pov_rate_urban, na.rm = T), 
            household_rural = mean(num_members_mean_rural, na.rm = T), 
            household_urban = mean(num_members_mean_urban, na.rm = T), 
            total_nightlight = round(mean(avg_light, na.rm = T), 2)) %>%
  drop_na()%>%
  summarise(household = round((household_rural + household_urban)/2, 2),
            poverty = round((poverty_rural+poverty_urban)/2, 2), 
            pc11_district_name = pc11_district_name,  rur =rur, 
            total_nightlight = total_nightlight)%>%
  filter(pc11_district_name != "") 




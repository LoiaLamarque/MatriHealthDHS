library(tidyverse)
library(sf)
library(scales)
library(haven)
library(data.table)

#BUILDING MATRILINEAL VARIABLES (LAND OWNERSHIP AND KIN CORESIDENCE)

#0. LOADING DATA
df_brut <- fread("00_raw_data/ipums/idhs_00005.csv")%>%
  filter(GEO_IA1992_2015 == 22)
varname1 <- read.csv2("00_raw_data/ipums/varname_landholding.csv")%>%
  rename("AGLANDWHO" = "Value")%>% select(AGLANDWHO, Label) 
varname2 <- read.csv2("00_raw_data/ipums/varname_disctrict.csv")%>%
  select(GEOALT_IA2015, Label) %>%rename("District" = "Label")
df_brut$AGLANDWHO <- as.integer(df_brut$AGLANDWHO)

truc <- df_brut %>%
  inner_join(varname1)%>% inner_join(varname2)

sf_state <- st_read("00_raw_data/shrug/geometries_shrug-v1.5.samosa-open-polygons-shp/state.shp")%>%
  filter(s_name == "Meghalaya")
sf_dis <- st_read("00_raw_data/shrug/geometries_shrug-v1.5.samosa-open-polygons-shp/district.shp")%>%
  filter(pc11_d_id %in% c(293:299))

#1. LAND OWNER who own the land/ household 
stat_land_prop_district <- truc %>%
  group_by(District, Label) %>%
  summarise(count = n()) %>%
  group_by(District) %>%
  mutate(total = sum(count), land_owner = percent(count/total))
  #who is the owner of the land? mostly women

write.csv(stat_land_prop_district, "04_stat_desc/ipums_land_owner.csv")

#map 

stat_land_prop_district_map <- truc %>%
  group_by(Label, GEOALT_IA2015, URBANHH) %>%
  summarise(count = n()) %>%
  mutate(GEOALT_IA2015 = as.factor(GEOALT_IA2015))%>%
  group_by(GEOALT_IA2015, URBANHH) %>%
  mutate(total = sum(count), land_owner = 100*(count/total))

sf <- inner_join(stat_land_prop_district_map, sf_dis, by =c("GEOALT_IA2015" = "pc11_d_id"))%>%
  st_as_sf()

sf %>%
  filter(Label == "Female member") %>%
  ggplot() + geom_sf(aes(fill = land_owner)) + theme_void() + 
  # facet_grid(~URBANHH, labeller = labeller("1" == "Urban", "2" == "Rural")) + 
  theme(legend.position = "bottom") + labs(fill = "% female land owners") 

ggsave("03_plots/map_house_hold.png")


# #2.1 CREATION VARIABLES RESIDENCE ET TYPE D APPARENTES AT THE HOUSEHOLD LEVEL
df_resid <- truc %>%
  filter(GEO_IA1992_2015 == 22, HHAGE != 0) %>%
  select(HHRELATE, HHEADSEXHH, IDHSHID, SEX,  URBANHH, HHLINENO, HHMEMBERS,  GEOALT_IA2015)%>%
  group_by(IDHSHID, URBANHH) %>%
  mutate(relate = max(HHRELATE),
         residence = case_when(relate <= 3 ~ "neolocal", #neolocal = nuclear family, parents and children
                               T ~ "kin_coresid")) %>% ungroup()

male_kin1 <- df_resid %>%filter(residence == "kin_coresid", HHEADSEXHH == 2, HHRELATE %in% c(4, 7, 11, 22))%>% filter(HHRELATE != 1) %>%
  mutate(type_kin = "male_kin")#meuf qui vivent avec leur in laws

male_kin2 <- df_resid %>%filter(residence == "kin_coresid", HHEADSEXHH == 1, HHRELATE %in% c(6, 8, 12, 13, 20, 30)) %>%
filter(HHRELATE != 1) %>% mutate(type_kin = "male_kin")#mec qui vivent avec leurs kins

joint_male <- rbind(male_kin1, male_kin2)

female_kin1 <- df_resid %>%filter(residence == "kin_coresid", HHEADSEXHH == 2, HHRELATE %in% c(6, 8, 12, 13, 20, 30))%>%
  filter(HHRELATE != 1) %>%
  mutate(type_kin = "female_kin")

female_kin2 <- df_resid %>%filter(residence == "kin_coresid", HHEADSEXHH == 1, HHRELATE %in% c(4, 7, 11, 22))%>% filter(HHRELATE != 1) %>%
  mutate(type_kin = "female_kin")

joint_female <- rbind(female_kin1, female_kin2)
total_join <- rbind(joint_male, joint_female)%>%
  group_by(IDHSHID, type_kin)%>%
  filter(!is.na(type_kin))%>%
  summarise(kin_count = n())%>%
  pivot_wider(names_from = type_kin, values_from = kin_count)%>%
  mutate_at(c("female_kin", "male_kin"), ~ replace_na(., 0))

df_final <- left_join(df_resid, total_join)%>% inner_join(truc)%>%
  select(URBANHH, HHMEMBERS, male_kin, female_kin, residence,  IDHSHID, District, Label) %>% unique()
# 
# write.csv(df_final, "01_tidy_data/ipums_df_kin_type_resid.csv")

#######2.2 CREATION VARIABLES RESIDENCE ET TYPE D APPARENTES AT THE INDIVIDUAL LEVEL###########
ind <- read.csv("00_raw_data/ipums/idhs_00006.csv") %>%
  filter(GEO_IA1992_2015 == 22)%>% select(contains("IDH"), )

df_resid <- truc %>%
  filter(GEO_IA1992_2015 == 22, HHAGE != 0) %>%
  select(HHRELATE, HHEADSEXHH, IDHSHID, SEX,  URBANHH, HHLINENO, HHMEMBERS,  GEOALT_IA2015)%>%
  group_by(IDHSHID, URBANHH) %>%
  mutate(relate = max(HHRELATE), 
         residence = case_when(relate <= 3 ~ "neolocal", #neolocal = nuclear family, parents and children
                               T ~ "kin_coresid")) %>% ungroup()

male_kin1 <- df_resid %>%filter(residence == "kin_coresid", HHEADSEXHH == 2, HHRELATE %in% c(4, 7, 11, 22))%>% filter(HHRELATE != 1) %>%
  mutate(type_kin = "male_kin")#meuf qui vivent avec leur in laws

male_kin2 <- df_resid %>%filter(residence == "kin_coresid", HHEADSEXHH == 1, HHRELATE %in% c(6, 8, 12, 13, 20, 30)) %>%
  filter(HHRELATE != 1) %>% mutate(type_kin = "male_kin")#mec qui vivent avec leurs kins

joint_male <- rbind(male_kin1, male_kin2)

female_kin1 <- df_resid %>%filter(residence == "kin_coresid", HHEADSEXHH == 2, HHRELATE %in% c(6, 8, 12, 13, 20, 30))%>% 
  filter(HHRELATE != 1) %>%
  mutate(type_kin = "female_kin")

female_kin2 <- df_resid %>%filter(residence == "kin_coresid", HHEADSEXHH == 1, HHRELATE %in% c(4, 7, 11, 22))%>% filter(HHRELATE != 1) %>%
  mutate(type_kin = "female_kin")

joint_female <- rbind(female_kin1, female_kin2)
total_join <- rbind(joint_male, joint_female)%>%
  group_by(IDHSHID, type_kin)%>%
  filter(!is.na(type_kin))%>%
  summarise(kin_count = n())%>%
  pivot_wider(names_from = type_kin, values_from = kin_count)%>%
  mutate_at(c("female_kin", "male_kin"), ~ replace_na(., 0))

df_final <- left_join(df_resid, total_join)%>% inner_join(truc)%>%
  select(URBANHH, HHMEMBERS, male_kin, female_kin, residence,  IDHSHID, District, Label) %>% unique()

write.csv(df_final, "01_tidy_data/ipums_df_kin_type_resid.csv")


#3. STAT DESC

#stat sur la residence en fonction de la propriété: est-ce qu'ils sont suffisamment décorrélés pour isoler l'effet de chacune 
#des variables sur la santé? 

stat_resid_ownership <- df_final %>%
  group_by(residence, Label) %>%
  summarise(count = n()) %>%
  group_by(residence) %>% mutate(total = sum(count),
                                 percent = percent(count/total))
write.csv2(stat_resid_ownership, "/Users/loialamarque/Dropbox/Thèse/outputs/ipums_stat_resid_ownership.csv")


stat_resid_kin %>% group_by(residence, type_kin) %>% summarise(n = n())
urban_rur <- stat_matri_neo %>% group_by(residence, URBANHH) %>% summarise(n = n())

matri_subsample <- stat_matri_neo %>%
  filter(residence ==  "matrilocal")%>% select(HHRELATE, residence, IDHSHID,  HHEADSEXHH)

check <- stat_matri_neo %>% select(HHRELATE, residence, IDHSHID)


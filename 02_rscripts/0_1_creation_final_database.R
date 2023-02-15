library(tidyverse)
library(scales)
library(haven)
library(ipumsr)
library(janitor)
library(naniar)

##CREATION FINAL DATABSE INDIA AND MEGHALAYA 

#BUILDING MATRILINEAL VARIABLES (LAND OWNERSHIP AND KIN CORESIDENCE)

#0. LOADING DATA IPUMS 2015 HOUSEHOLD MEMBERS
ddi <- read_ipums_ddi("00_raw_data/ipums/idhs_00008.xml")
data <- read_ipums_micro(ddi, verbose = F) %>% filter(YEAR == 2015)

ddi_indiv <- read_ipums_ddi("00_raw_data/ipums/idhs_00011.xml")
data_indiv <- read_ipums_micro(ddi_indiv, verbose = F) %>% filter(YEAR == 2015) %>%
  select(- c(SAMPLE, SAMPLESTR, COUNTRY, YEAR, DHSID, IDHSPSU, IDHSSTRATA, HHID,
         POPWT, GEO_IA1992_2015, GEO_IA2015,
         GEOALT_IA2015)) %>% mutate(HHLINENO = as.integer(paste0("0", LINENO)))

df_indiv_hh <- inner_join(data_indiv, data, by = c("IDHSHID","AGE" = "HHAGE", "HHLINENO"))

geo_cov <- read.csv("00_raw_data/ipums/dhs/2015/geo_cov/IAGC72FL.csv")

#1. CODE MATRI VARIABLES

#1.1. RESIDENCE 
df_resid <- df_indiv_hh %>%
  select(HHRELATE, HHEADSEXHH, IDHSHID, SEX,  URBANHH, HHLINENO, 
         HHMEMBERS,  GEOALT_IA2015, MARSTAT, AGLANDWHO, OWNHOUSEWHO, AGE, 
         HWFANEMIALVL, IDHSPID, PERWEIGHT, HWFBMI, CHEB, HWFWEIGHT, HWFHEIGHT, HHWEIGHT, 
         HWFHEMOLEVELALT, HHMEMBERS, contains("BIRTHWT"), RESIDEINTYR, PREGNANT, EDYRTOTAL:HUSFERTPREF, 
         contains("IRON"), CLUSTERNO, DHSID, GEO_IA2015, contains("KIDDO"), contains("DEC"))%>%
  group_by(IDHSHID) %>%
  mutate(relate = max(HHRELATE), 
         resid = case_when((relate <= 3 & 2 %in% HHRELATE) ~ "neolocal",
                               ((any(HHRELATE == 7)) & (!HHRELATE %in% c(6, 8, 12, 13, 20, 21))) ~ "HHH_in_laws", 
                               (any(HHRELATE %in% c(6, 8, 12, 13, 20, 21)) & all(HHRELATE != 7)) ~ "HHH_relatives", #neolocal = nuclear family, parents and children
                               T ~ "extended_cognatic")) %>% ungroup()%>%
  mutate(residence_indiv_level = case_when((resid == "HHH_in_laws" & HHRELATE == 1) ~ 'patrilocal', 
                               (resid == "HHH_in_laws" & HHRELATE == 2) ~ 'matrilocal', 
                               (resid == "HHH_relatives" & HHRELATE == 1) ~ "matrilocal", 
                               (resid == "HHH_relatives" & HHRELATE == 2) ~ 'patrilocal', 
                               (resid == "neolocal") ~ "neolocal", 
                               (resid == "extended_cognatic") ~ "extended_cognatic", 
                               T ~ "NA"))%>%
  filter(SEX == 2 & MARSTAT == 21 & HHRELATE %in% c(1, 2)) #on ne garde que les individus mariés 

# 
# View(df_resid %>% select(IDHSHID, SEX, MARSTAT, residence_indiv_level, resid, HHRELATE, RESIDEINTYR))
# View(df_indiv_hh %>% select(IDHSHID, SEX, MARSTAT, HHRELATE, AGE))

stat_resid_property <- df_resid %>% filter(!OWNHOUSEWHO %in% c(8,9)) %>%group_by(residence_indiv_level, OWNHOUSEWHO) %>% count() %>%
  mutate(OWNHOUSEWHO = as_factor(OWNHOUSEWHO)) %>%
  spread(residence_indiv_level, n) %>% adorn_totals(c("row", "col"))

write.csv(stat_resid_property, "04_stat_desc/stat_resid_house.csv")


#1.2. AUTRE CODE RESIDENCE: NOMBRE D ANNEES PASSEES DANS LE FOYER DE RESIDENCE 

df_resid_alt <- df_indiv_hh %>%
  filter(SEX == 2, GEO_IA2015 == 22, !RESIDEINTYR %in% c(97, 98)) %>%
  select(HHRELATE, HHEADSEXHH, IDHSHID, SEX,  URBANHH, HHLINENO, 
         HHMEMBERS,  GEOALT_IA2015, MARSTAT, AGLANDWHO, OWNHOUSEWHO, AGE, 
         HWFANEMIALVL, IDHSPID, PERWEIGHT, HWFBMI, CHEB, HWFWEIGHT, HWFHEIGHT, HHWEIGHT, 
         HWFHEMOLEVELALT, HHMEMBERS, contains("BIRTHWT"), RESIDEINTYR, PREGNANT, EDYRTOTAL:HUSFERTPREF, 
         contains("IRON"), CLUSTERNO, DHSID, GEO_IA2015, contains("KIDDO"), contains("DEC"), 
         EDUCLVL, WEALTHQ, CASEID)%>%
  mutate(resid_year = case_when(RESIDEINTYR == 95 ~ "Always", 
                                T ~ "Visitor")) %>%
  left_join(df_resid)
  

# 
# View(df_resid %>% select(IDHSHID, SEX, MARSTAT, residence_indiv_level, resid, HHRELATE, RESIDEINTYR))
# View(df_indiv_hh %>% select(IDHSHID, SEX, MARSTAT, HHRELATE, AGE))

stat_resid_property <- df_resid %>% filter(!OWNHOUSEWHO %in% c(8,9)) %>%group_by(residence_indiv_level, OWNHOUSEWHO) %>% count() %>%
  mutate(OWNHOUSEWHO = as_factor(OWNHOUSEWHO)) %>%
  spread(residence_indiv_level, n) %>% adorn_totals(c("row", "col"))

write.csv(stat_resid_property, "04_stat_desc/stat_resid_house.csv")

#2. HEALTH VARIABLES 

#2.1. CORESIDENCE WITH KIN
df_health_matri_without_na_india <- df_resid %>%
  mutate(HWFBMI = HWFBMI/100, 
         HWFHEMOLEVELALT= HWFHEMOLEVELALT/10) %>%
  replace_with_na(replace = list(CHEB = c(98),
                                 HWFWEIGHT = c(9994: 9999), 
                                 HWFHEIGHT = c(9994: 9999), 
                                 HWFHEMOLEVELALT = c(992:999), 
                                 BIRTHWT_01 = c(9995:9999), 
                                 BIRTHWT_02 = c(9995:9999), 
                                 BIRTHWT_03 = c(9995:9999), 
                                 BIRTHWT_04 = c(9995:9999), 
                                 BIRTHWT_05 = c(9995:9999), 
                                 BIRTHWT_06 = c(9995:9999), 
                                 HUSFERTPREF = c(7, 8, 9), 
                                 FPLCHDESIRE = c(7, 8, 9), 
                                 OWNHOUSEWHO = c(8, 9), 
                                 HWFBMI = c(9997:9999), 
                                 AGE = c(95:98))) %>%
  filter(!is.na(OWNHOUSEWHO)) %>%
  inner_join(geo_cov, by = c("CLUSTERNO" = "DHSCLUST")) #variables geographiques pour les contrôles

# df_health_matri_without_na_megha <- df_health_matri_without_na_india %>% filter(GEO_IA2015 == 22)

saveRDS(object = df_health_matri_without_na_india, file = "01_tidy_data/resid_clean_india.rds")

saveRDS(object = df_health_matri_without_na_megha, file = "01_tidy_data/resid_clean_megha.rds")

#2.2. RESIDENCE IN NATAL HOUSE
df_health_alt_matri_without_na <- df_resid_alt %>%
  mutate(HWFBMI = HWFBMI/100, 
         HWFHEMOLEVELALT= HWFHEMOLEVELALT/10) %>%
  replace_with_na(replace = list(CHEB = c(98),
                                 HWFWEIGHT = c(9994: 9999), 
                                 HWFHEIGHT = c(9994: 9999), 
                                 HWFHEMOLEVELALT = c(992:999), 
                                 BIRTHWT_01 = c(9995:9999), 
                                 BIRTHWT_02 = c(9995:9999), 
                                 BIRTHWT_03 = c(9995:9999), 
                                 BIRTHWT_04 = c(9995:9999), 
                                 BIRTHWT_05 = c(9995:9999), 
                                 BIRTHWT_06 = c(9995:9999), 
                                 HUSFERTPREF = c(7, 8, 9), 
                                 FPLCHDESIRE = c(7, 8, 9), 
                                 OWNHOUSEWHO = c(8, 9), 
                                 HWFBMI = c(9997:9999), 
                                 DECBIGHH = c(60, 98, 99), 
                                 DECFAMVISIT = c(8, 9), 
                                 DECFEMEARN = c(60, 98, 99), 
                                 DECFEMHCARE = c(60, 98, 99),
                                 DECHLCENTERGO = c(98, 99), 
                                 AGE = c(95:98))) %>%
  left_join(geo_cov, by = c("CLUSTERNO" = "DHSCLUST")) #variables geographiques pour les contrôles

saveRDS(object = df_health_alt_matri_without_na, file = "MatriHealthDHS/01_tidy_data/resid_philopatry_clean_megha.rds")

#other way for residence #####

# df_resid_mar <- df_resid %>% filter(HHRELATE == 2)
# vec_id <- c(unique(df_resid_mar$IDHSHID)) 
# df_resid_mar2 <- df_final %>% filter(IDHSHID %in% vec_id) #on ne garde que les HH avec un couple
#   
# 
# male_kin1 <- df_resid_mar2 %>%
#   filter(residence == "kin_coresid", HHEADSEXHH == 2, HHRELATE %in% c(11, 7, 22))%>% 
#   filter(HHRELATE != 1) %>%
#   mutate(type_kin = "male_kin")#meuf qui vivent avec leur in laws
# 
# male_kin2 <- df_resid_mar2 %>%filter(residence == "kin_coresid", HHEADSEXHH == 1, 
#                                      HHRELATE %in% c(6, 8, 12, 13, 20, 30, 21)) %>%
#   filter(HHRELATE != 1) %>% mutate(type_kin = "male_kin")#mec qui vivent avec leurs kins
# 
# joint_male <- rbind(male_kin1, male_kin2)
# 
# female_kin1 <- df_resid_mar2 %>%filter(residence == "kin_coresid", HHEADSEXHH == 2, 
#                                        HHRELATE %in% c(6, 8, 12, 13, 20, 30, 21))%>%
#   filter(HHRELATE != 1) %>%
#   mutate(type_kin = "female_kin")
# 
# female_kin2 <- df_resid_mar2 %>%filter(residence == "kin_coresid", HHEADSEXHH == 1, HHRELATE %in% c(11, 7, 22))%>% filter(HHRELATE != 1) %>%
#   mutate(type_kin = "female_kin")
# 
# joint_female <- rbind(female_kin1, female_kin2)
# 
# total_join <- rbind(joint_male, joint_female)%>%
#   group_by(IDHSHID, type_kin)%>%
#   filter(!is.na(type_kin))%>%
#   summarise(kin_count = n())%>%
#   pivot_wider(names_from = type_kin, values_from = kin_count)%>%
#   mutate_at(c("female_kin", "male_kin"), ~ replace_na(., 0))
# 
# df_final_mar <- left_join(df_resid_mar2, total_join)%>% 
#   mutate(AGLANDWHO = as_factor(AGLANDWHO), 
#          OWNHOUSEWHO = as_factor(OWNHOUSEWHO), 
#          GEOALT_IA2015 = as.integer(GEOALT_IA2015), 
#          residence_2 = case_when((residence == "kin_coresid" & is.na(female_kin) & 
#                                     is.na(male_kin))~"extended_cognatic", 
#                                  (residence == "neolocal" ~ "nuclear_family"), 
#                                  (residence == "kin_coresid" & female_kin == 0 & male_kin != 0 & HHRELATE !=20)~
#                                    "patrilocal", 
#                                  (residence == "kin_coresid" & male_kin == 0  & female_kin !=0)~"matrilocal",
#                                  (HHRELATE == 20) ~ "matrilocal",  
#                                  T ~ "extended_cognatic")) 
######


df_intermed <- df_resid %>% 
         filter(residence_indiv_level == "patrilocal" & OWNHOUSEWHO == 2 | 
                  residence_indiv_level == "matrilocal" & OWNHOUSEWHO == 1) %>% left_join(df_indiv_hh)

vec_id_mar <- unique(df_intermed$IDHSHID) 

df_check <- df_indiv_hh %>% filter(IDHSHID %in% vec_id_mar) %>%
  select(HHRELATE, IDHSHID, SEX,AGE, MARSTAT, OWNHOUSEWHO, residence_indiv_level)



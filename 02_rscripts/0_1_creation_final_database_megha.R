library(tidyverse)
library(scales)
library(haven)
library(ipumsr)
library(janitor)
library(naniar)

##CREATION FINAL DATABSE MEGHALAYA 

#BUILDING MATRILINEAL VARIABLES (LAND OWNERSHIP AND KIN CORESIDENCE)

#0. LOADING DATA IPUMS 2015 HOUSEHOLD MEMBERS
ddi <- read_ipums_ddi("00_raw_data/ipums/idhs_00008.xml")
data <- read_ipums_micro(ddi, verbose = F) %>% filter(YEAR == 2015, GEO_IA2015 == 22)

ddi_indiv <- read_ipums_ddi("00_raw_data/ipums/idhs_00011.xml")
data_indiv <- read_ipums_micro(ddi_indiv, verbose = F) %>% filter(YEAR == 2015, GEO_IA2015 == 22) %>%
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
         contains("IRON"), CLUSTERNO, DHSID, GEO_IA2015, contains("KIDDO"), contains("DEC"), PSU, STRATA)%>%
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


#1.2. AUTRE CODE RESIDENCE: NOMBRE D ANNEES PASSEES DANS LE FOYER DE RESIDENCE 

df_resid_alt <- df_indiv_hh %>%
  filter(SEX == 2, !RESIDEINTYR %in% c(97, 98)) %>%
  select(HHRELATE, HHEADSEXHH, IDHSHID, SEX,  URBANHH, HHLINENO, 
         HHMEMBERS,  GEOALT_IA2015, MARSTAT, AGLANDWHO, OWNHOUSEWHO, AGE, 
         HWFANEMIALVL, IDHSPID, PERWEIGHT, HWFBMI, CHEB, HWFWEIGHT, HWFHEIGHT, HHWEIGHT, 
         HWFHEMOLEVELALT, HHMEMBERS, contains("BIRTHWT"), RESIDEINTYR, PREGNANT, EDYRTOTAL:HUSFERTPREF, 
         contains("IRON"), CLUSTERNO, DHSID, GEO_IA2015, contains("KIDDO"), contains("DEC"), 
         EDUCLVL, WEALTHQ, CASEID, PSU, STRATA)%>%
  mutate(resid_year = case_when(RESIDEINTYR == 95 ~ "Always", 
                                T ~ "Visitor")) %>%
  left_join(df_resid)

#2. HEALTH VARIABLES 

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



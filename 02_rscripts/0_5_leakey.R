library(scales)

#preliminary results - Leakey

##1. ANEMIA EN FONCTION DE RESIDENCE ET OWNERSHIP

df_final_clean <- df_final %>%
  filter(AGLANDWHO != "Missing", AGLANDWHO != "NIU (not in universe)", 
         OWNHOUSEWHO != "Missing", OWNHOUSEWHO != "NIU (not in universe)",
         !HWFANEMIALVL %in% c(8, 9))%>%
  mutate(ANEM = case_when(HWFANEMIALVL %in% c(1, 2, 3)~ "Anaemic", 
                          T ~ "Non-anaemic"))

df_stat <- df_final_clean %>%
  group_by(AGLANDWHO, ANEM) %>%
  summarise(n = n()) %>% group_by(AGLANDWHO) %>% summarise(percent = percent(n/sum(n)), ANEM = ANEM, n = n)
write.csv(df_stat, "04_stat_desc/leakey/stat_health_agland.csv")

# df_stat2 <- df_final_clean %>%
#   group_by(AGLANDWHO)%>%
#   summarise(mean = mean(HWFHEMOLEVELALT), median = median(HWFHEMOLEVELALT))


df_stat2 <- df_final_clean %>%
  group_by(OWNHOUSEWHO, ANEM) %>%
  summarise(n = n()) %>% group_by(OWNHOUSEWHO) %>% summarise(percent = percent(n/sum(n)), ANEM = ANEM, n = n)
write.csv(df_stat2, "04_stat_desc/leakey/stat_health_house.csv")

##2. ECONOMETRIE#####
#anemia en fonction de toutes les autres variables


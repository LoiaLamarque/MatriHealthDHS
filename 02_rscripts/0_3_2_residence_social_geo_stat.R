library(tidyr)
library(ipumsr)

#stat residence
df_final_withouth_NA <- readRDS("01_tidy_data/df_final_without_na.rds")%>%
  mutate(URBAN = as_factor(URBANHH), 
         OWNHOUSEWHO = as_factor(OWNHOUSEWHO)) %>%
  filter(!RESIDEINTYR %in% c(96:98)) %>%
  mutate(resid_year = case_when(RESIDEINTYR == 95 ~ "Always", 
                                RESIDEINTYR <20 ~ "<20", 
                                RESIDEINTYR >= 20 ~ ">20"))

stat_desc_resid <- df_final_withouth_NA %>%
  group_by(resid_year, residence_indiv_level, OWNHOUSEWHO) %>%
  count()


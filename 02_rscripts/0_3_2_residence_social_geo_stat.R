library(tidyr)
library(ipumsr)

#stat residence
df_final_withouth_NA <- readRDS("MatriHealthDHS/01_tidy_data/resid_philopatry_clean_megha.rds")%>%
  mutate(URBAN = as_factor(URBANHH), 
         OWNHOUSEWHO = as_factor(OWNHOUSEWHO)) %>%
  filter(!RESIDEINTYR %in% c(96:98)) %>%
  mutate(resid_year = as.character(case_when(RESIDEINTYR == 95 ~ "Matri", 
                                T ~ "Non-matri")))

stat_desc_resid <- df_final_withouth_NA %>%
  group_by(resid_year, OWNHOUSEWHO) %>% count()%>%
  spread(resid_year, n) %>% adorn_totals(c("row", "col"))

writexl::write_xlsx(stat_desc_resid, 'MatriHealthDHS/04_stat_desc/stat_resid_house.xlsx')

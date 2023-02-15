library(tidyr)
library(psych)
library(survey)
library(gtsummary)

#on regarde les NA

df_final <- readRDS("MatriHealthDHS/01_tidy_data/resid_philopatry_clean_megha.rds") %>%
  select(URBANHH, MARSTAT, OWNHOUSEWHO, AGE, HWFHEMOLEVELALT, HWFBMI, CHEB, resid_year, HHWEIGHT, PREGNANT, 
         PERWEIGHT, EDUCLVL, WEALTHQ, PERWEIGHT, IDHSPID, IDHSHID, contains("BIRTH"), contains("2015"), CLUSTERNO, 
         Nightlights_Composite,
         DECBIGHH,
         DECFAMVISIT,
         DECFEMEARN, 
         DECFEMHCARE, 
         DECHLCENTERGO, 
         KIDDOBCMC_01) %>%
  mutate(across(-c(resid_year, PERWEIGHT, HHWEIGHT, HWFBMI, HWFHEMOLEVELALT, contains("BIRTH"), contains("2015"), CLUSTERNO, 
                   Nightlights_Composite), ~as_factor(.x))) %>%
  mutate(across(c(AGE:CHEB), ~as.numeric(.x))) %>%
  mutate(ANAEMIC = case_when(HWFHEMOLEVELALT < 12 ~ "Not anaemic", 
                             T ~ "Anaemic")) 
#computing birthweight

df_birth <- df_final %>% select(contains("BIRTH")) %>% select(-BIRTHWT_ALL) %>%
  rowMeans(., na.rm = T) %>% tibble() 

df_final_every_variables <- bind_cols(df_birth, df_final) %>% rename(birthweight = ".") %>%
  mutate(birthweight = birthweight/1000)
write_rds(df_final_every_variables, "MatriHealthDHS/01_tidy_data/resid_philopatry_clean_megha_2.rds")

df_final <- bind_cols(df_birth, df_final) %>% rename(birthweight = ".") %>%
  mutate(birthweight = birthweight/1000) %>%select(-contains("2015"), -CLUSTERNO, -contains("BIRTHWT"))

#getvarname#####
# 
# ddi <- read_ipums_ddi("00_raw_data/ipums/idhs_00008.xml")
# data <- read_ipums_micro(ddi, verbose = F) %>% filter(YEAR == 2015)
# 
# ddi_indiv <- read_ipums_ddi("00_raw_data/ipums/idhs_00010.xml")
# data_indiv <- read_ipums_micro(ddi_indiv, verbose = F) %>% filter(YEAR == 2015) %>%
#   select(- c(SAMPLE, SAMPLESTR, COUNTRY, YEAR, DHSID, IDHSPSU, IDHSSTRATA, HHID,
#              POPWT, GEO_IA1992_2015, GEO_IA2015,
#              GEOALT_IA2015)) %>% mutate(HHLINENO = as.integer(paste0("0", LINENO)))
# 
# df_indiv_hh <- right_join(data_indiv, data, by = c("IDHSHID","AGE" = "HHAGE", "HHLINENO"))%>% #on merge data indiv et HH pour avoir tous les membres du foyer
#   filter(IDHSHID %in% c(unique(data_indiv$IDHSHID))) 
# varname <- ipums_var_info(df_indiv_hh, vars = everything()) %>% select(var_name, var_label)
# c <- data.frame(var_name = colnames(df_final)) %>% left_join(varname) %>%
#   mutate(var_label = case_when(var_name == "resid_year" ~ "Years lived in place of residence", 
#                                 T ~ var_label))
# 
# setnames(df_final, old = c$var_name, 
#          new = c$var_label)

#df_final : final database with right varnames

#1. STAT DESC#####

#transforme df en survey design pour les stats desc avec poids
df_final_indiv <- df_final %>% select(-c(URBANHH, OWNHOUSEWHO, WEALTHQ, HHWEIGHT, IDHSHID))
df_final_hh <- df_final %>% select(URBANHH, OWNHOUSEWHO, WEALTHQ, HHWEIGHT, IDHSHID) %>% unique()

#missing values
gg_miss_var(df_final_indiv) #presque pas de variable manquante mashallah
gg_miss_var(df_final_hh)

df_survey_indiv <- svydesign(weights=~PERWEIGHT, nest=F, data=df_final_indiv, id = ~IDHSPID)
stat_desc_indiv <- df_survey_indiv %>% 
  tbl_svysummary(by = resid_year, 
    # Use include to select variables
    label = list(resid_year~"Number of years spent in the house", 
             CHEB ~ "Number of children", 
             AGE ~ "Age", 
             HWFHEMOLEVELALT ~ "Hemoglobin level", 
             HWFBMI ~ "BMI", 
             ANAEMIC ~ "Anaemic",
             birthweight ~ "Mean birthweight", 
             PREGNANT ~ "Currently pregnant", 
             DECFEMHCARE ~ "Final say on woman's health care", 
             DECHLCENTERGO ~ "Can visit health center/hospital alone"), 
    include = c(birthweight:PREGNANT, EDUCLVL, ANAEMIC,DECBIGHH: DECHLCENTERGO),
    statistic = list(all_continuous()  ~ "{mean} ({sd})",
                     all_categorical() ~ "{n}    ({p}%)"),
    digits = list(all_continuous()  ~ c(1, 1),
                  all_categorical() ~ c(0, 1))
  ) %>%
  # modify_header(label = "**Variable**") %>%
  # modify_caption("Weighted descriptive statistics for individual variables") %>%
  bold_labels() %>% 
  modify_spanning_header(labl = "Variable", c("stat_1", "stat_2") ~ "**Number of years spent \nin the area**")%>%
  as_flex_table()

flextable::save_as_docx(stat_desc_indiv, path = "MatriHealthDHS/04_stat_desc/stat_desc_indiv_weighted.docx")

###survey household level
df_survey_hh <- svydesign(weights=~HHWEIGHT, nest=F, data=df_final_hh, id = ~IDHSHID)
stat_desc_hh <- df_survey_hh %>% 
  tbl_svysummary(by = OWNHOUSEWHO, 
    # Use include to select variables
    include = c(URBANHH:WEALTHQ),
    statistic = list(all_categorical() ~ "{n}    ({p}%)"),
    digits = list(all_categorical() ~ c(0, 1))
  ) %>%
  # modify_header(label = "**Variable**") %>%
  # modify_caption("Weighted descriptive statistics for household variables") %>%
  modify_spanning_header(labl = "Variable", c("stat_1", "stat_2") ~ "**Gender of the house owner**")%>%
  bold_labels() %>% 
  as_flex_table()

flextable::save_as_docx(stat_desc_hh, path = "MatriHealthDHS/04_stat_desc/stat_desc_hh_weighted.docx")

#other way (weigh more complicated)

stat_desc_cat <- ExpCustomStat(df_final,Cvar=c("Gender of HH member who owns house","Urban-rural status","Years lived in place of residence",
                              "Highest educational level"),gpby=FALSE)
  
writexl::write_xlsx(stat_desc_cat, "MatriHealthDHS/04_stat_desc/counting_cat_variables.xlsx")

stat_desc_num <- df_final %>% select_if(~is.numeric(.))%>%
  describe()

#2. COUNTING MISSING VALUE FOR THE EPIDEMIOLOGICAL FLOWCHART 
# 
# df_na <- df_health_matri_without_na %>%
#   filter(is.na(HWFHEMOLEVELALT))
# 
# df_na <- df_health_matri_without_na %>%
#   filter(is.na(HWFBMI))
# 
# df_na <- df_health_matri_without_na %>%
#   filter(is.na(OWNHOUSEWHO))
# 
# df_na <- df_health_matri_without_na %>%
#   filter(is.na(CHEB))
# 
# df_final_without_na <- df_health_matri_without_na %>%
#   filter(!is.na(OWNHOUSEWHO))
# #only missing values
# 
# df_final_without_na %>% group_by(OWNHOUSEWHO) %>% count()
# 
# saveRDS(object = df_final_without_na, file = "01_tidy_data/df_final_without_na.rds")

#3. HISTO FOR VARIABLES OF INTEREST 
#quelques stat desc
df_health <- df_health_matri_without_na %>% 
  select(HWFHEMOLEVELALT,HWFBMI, HWFWEIGHT, HWFHEIGHT, CHEB, 
         IDHSPID, PERWEIGHT)%>% na.omit()
 # # sum(df_health$PERWEIGHT) to get weighter number of obs
 #  # mutate(across(HWFHEMOLEVELALT:HWFHEIGHT, ~as_factor(lbl_clean(.x)))) %>%
 #  pivot_longer(cols = c("HWFHEMOLEVELALT":"HWFHEIGHT"), names_to = "var", values_to = "value") %>%
 #  group_by(var, value) %>% count(wt = PERWEIGHT) 

df_health %>% ggplot(aes(x = HWFHEMOLEVELALT)) + geom_histogram() + theme_bw() 
ggsave("03_plots/histo_hemoglob.png")

df_health %>% ggplot(aes(x = CHEB)) + geom_histogram() + theme_bw()
ggsave("03_plots/histo_nb_children.png")
# 
# ggplot(df_health, aes(x = value, y = n)) +
#   geom_bar(stat = "identity") + facet_wrap(~var, scales = "free") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), 
#         axis.title.x=element_blank()) + theme_bw() + 
#   theme(axis.title.x=element_blank()) + scale_x_continuous(n.breaks = 15)
# ggsave("03_plots/histo_health_var.png")

#NUTRITION
df_nutri <- df_health_matri_without_na %>% filter(SEX == 2) %>%
  select(HWFBMI, IDHSPID, PERWEIGHT) %>% na.omit()%>%
  group_by(HWFBMI)%>%
  count(wt = PERWEIGHT)

sum(df_nutri$PERWEIGHT)

ggplot(df_nutri, aes(x = HWFBMI, y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x=element_blank()) + theme_bw() + xlab("Body mass index (BMI)")
ggsave("03_plots/histo_bmi_var.png")

# #DOMESTIC VIOLENCE
# df_dom_violence <- df_final %>%
#   select(DVPSLAP:DVPSEX, IDHSPID, DVWEIGHT,-DVPKNFTHUSEF, 
#          -DVPKNFTHUSE, -DVPCHOKEFQ, -DVPCHOKE, 
#          -DVPKICKFQ, -DVPPUNCHFQ) %>%
#   mutate(across(DVPSLAP:DVPSEX, ~as_factor(.x))) %>% na.omit()%>%
#   pivot_longer(cols = c("DVPSLAP":"DVPSEX"), names_to = "var", values_to = "value")%>%
#   inner_join(varname) %>% filter(!value %in% c("NIU (not in universe)", "Missing" )) %>%
#   group_by(name, value, var) %>% count(wt = DVWEIGHT) %>% ungroup()
# 
# df_dom_violence %>% filter(var!= "DVPTWISTFQ", 
#                            var != "DVPSLAPFQ") %>%
#   ggplot(aes(x = value, y = n), ylab = '') +
#   geom_bar(stat = "identity") + facet_wrap(~name, scales = "free", 
#                                            labeller = labeller(name = label_wrap_gen(width = 25))) + 
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
#         axis.title.x=element_blank())
# 
# ggsave("03_plots/histo_dom_viol_var1.png")
# 
# df_dom_violence %>% filter(var %in% c("DVPTWISTFQ", "DVPSLAPFQ")) %>%
#   ggplot(aes(x = value, y = n), ylab = '') +
#   geom_bar(stat = "identity") + facet_grid(~name, scales = "free", 
#                                            labeller = labeller(name = label_wrap_gen(width = 25))) + 
#   theme_bw() + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
#         axis.title.x=element_blank())
# 
# ggsave("03_plots/histo_dom_viol_var2.png")

#FERTILITY

# #2. VARIABLES MATRI 
# 
# df_matri <- df_final %>% select(residence_2, IDHSHID, HHWEIGHT) %>% unique() %>%
#   group_by(residence_2)  %>% count(wt = HHWEIGHT)


#######

# df_matri2 <- df_final %>% select(AGLANDWHO, OWNHOUSEWHO, IDHSHID, HHWEIGHT) %>% unique() %>% na.omit() %>%
#   mutate(across(AGLANDWHO:OWNHOUSEWHO, ~as_factor(lbl_clean(.x)))) %>% 
#   filter(OWNHOUSEWHO != "NIU (not in universe)", AGLANDWHO != "NIU (not in universe)", OWNHOUSEWHO != "Missing", AGLANDWHO != "Missing") %>%
#   pivot_longer(cols = c("AGLANDWHO":"OWNHOUSEWHO"), names_to = "var", values_to = "value") %>%
#   group_by(var, value) %>% count(wt = HHWEIGHT)
# 
# ggplot(df_matri2, aes(x = value, y = n)) +
#   geom_bar(stat = "identity") + facet_wrap(~var, scales = "free") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), 
#         axis.title.x=element_blank()) + theme_bw() + 
#   theme(axis.title.x=element_blank())
# ggsave("03_plots/histo_ownerhsip_var.png")







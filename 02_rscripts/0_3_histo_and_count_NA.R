library(tidyr)
library(pacman)
library(naniar)


#STAT DESC IPUMS VAR. FOR ALL VARIABLES, TABLE WITH WEIGHT COUNT AND HISTO

df_final <- readRDS("01_tidy_data/resid_clean.rds")


# varname <- cbind(var = ddi$var_info$var_name, name = ddi$var_info$var_label) %>% as.tibble()

#1. ADDING VARIABLES HEALTH FOR WOMEN
#HEALTH  
df_health_matri_without_na <- df_final %>%
  mutate(HWFBMI = HWFBMI/100, 
         HWFHEMOLEVELALT= HWFHEMOLEVELALT/10) %>%
  replace_with_na(replace = list(CHEB = c(99,98),
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
                                 AGE = c(95:98))) 
#BASE DE DONNEES CLEAN!

gg_miss_var(df_health_matri_without_na) #presque pas de variable manquante mashallah

#2. COUNTING MISSING VALUE FOR THE EPIDEMIOLOGICAL FLOWCHART 

df_na <- df_health_matri_without_na %>%
  filter(is.na(HWFHEMOLEVELALT))

df_na <- df_health_matri_without_na %>%
  filter(is.na(HWFBMI))

df_na <- df_health_matri_without_na %>%
  filter(is.na(OWNHOUSEWHO))

df_na <- df_health_matri_without_na %>%
  filter(is.na(CHEB))

df_final_without_na <- df_health_matri_without_na %>%
  filter(!is.na(OWNHOUSEWHO))
#only missing values

df_final_without_na %>% group_by(OWNHOUSEWHO) %>% count()

saveRDS(object = df_final_without_na, file = "01_tidy_data/df_final_without_na.rds")

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







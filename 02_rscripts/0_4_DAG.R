library(tidyr)
library(ggdag)
library(ipumsr)
library(scales)
theme_set(theme_dag())

df_final_without_na <- readRDS("01_tidy_data/df_final_without_na.rds")

#1. est-ce qu'il y a plus de matri en ville ou en campagne? 

df_check <- df_final_without_na %>% mutate(URBAN = as_factor(URBANHH), 
                                           OWNHOUSEWHO = as_factor(OWNHOUSEWHO)) %>%
  filter(!RESIDEINTYR %in% c(96:98))

#matrilinéarité 
df_test <- df_check %>% group_by(URBAN, OWNHOUSEWHO) %>% 
  count(wt = HHWEIGHT) %>% group_by(URBAN) %>% summarise(count = percent(n/sum(n)), 
                                            OWNHOUSEWHO = OWNHOUSEWHO) %>%
  spread(URBAN, count) 

#matrilocalité 

df_test_2 <- df_check %>% mutate(resid_year = case_when(RESIDEINTYR == 95 ~ "Always", 
                                                        RESIDEINTYR <20 ~ "<20", 
                                                        RESIDEINTYR >= 20 ~ ">20"))%>%
  group_by(URBAN, resid_year) %>% 
  count(wt = HHWEIGHT) %>% group_by(URBAN) %>% summarise(count = percent(n/sum(n)), 
                                            resid_year = resid_year) %>%
  spread(URBAN, count) 

#même taux de matrilinéarité en ville et en campagne

#2. DAG: la relation entre matrilinéarité et santé des femmes est-elle médiée par des variables? 

matri_health_dag <- dagify(fitness ~ matriliny,
                           fitness ~ hemo_level + nb_child, 
                         econ_devel ~ proximity_kin + inheritance,
                         matriliny ~ econ_dev, 
                         matriliny ~ inheritance + proximity_kin, 
                         fitness ~ econ_dev, 
                         exposure = "econ_dev",
                         outcome = "fitness", 
                         latent = c("matriliny", "fitness"), 
                         labels = c(
                           "fitness" = "Fitness", 
                           "hemo_level" = "Hemoglobin \n level", 
                           "nb_child" = "Number of children", 
                           "econ_dev" = "Economic \n development", 
                           "hemoglobin" = "Hemoglobin",
                           "matriliny" = "Matriliny",
                           "proximity_kin" = "Co-residence with\n kin",
                           "inheritance" = "Female property"))

ggdag(matri_health_dag, text = FALSE, use_labels = "label") 

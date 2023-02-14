library(tidyr)
library(ipumsr)
library(dagitty)
library(ggdag)

df_final <- readRDS("MatriHealthDHS/01_tidy_data/resid_philopatry_clean_megha_2.rds") %>%
  dplyr::select(-c(BIRTHWT_ALL:BIRTHWT_06)) %>%
  dplyr::mutate(anm = as.numeric(ifelse(ANAEMIC == "Anaemic", 1, 0)), 
         resid_year = as.numeric(ifelse(resid_year == "Always", 1, 0)))

c <- df_final %>% select(-PERWEIGHT, -HHWEIGHT, -contains("2015"))
create_report(c)

dag_coords <-
  tibble(name = c("Anaemia", "EducLevel", "Matri_locality", "Matri_property", "Area_type", 
                  "Number_children", "Wealth_quintile", "Pregnant", "Age"),
         x    = c(1, 2, 2.5, 3, 4, 9, 6, 7.9,  3),
         y    = c(2, 2, 1, 2, 2, 0.3, 3, 4, 1))

dag_df <- as.data.frame(t(df_final)) 
dag_df$name <- colnames(df_final) 
dag_df <- tibble(dag_df)
#1. model specifying all the potential relationships

dag <-
  dagify(EducLevel ~ Area_type, 
         Anaemia ~ EducLevel + Matri_locality + Matri_property, 
         Matri_property ~ EducLevel, 
         Matri_locality ~ EducLevel, 
         outcome = "Anaemia", 
         exposure = 'Matri_property', 
         coords = dag_coords)

dag %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_point(aes(color = name == "Anaemia"),
                 alpha = 1/2, size = 6.5, show.legend = F) +
  geom_point(x = 2.5, y = 1, 
             size = 6.5, shape = 1, stroke = 1, color = "orange") +
  geom_dag_text(color = "black") +
  geom_dag_edges() + 
  scale_color_manual(values = c("steelblue", "orange")) +
  theme_dag()

#### model 1 ####

base_model <- "
  # regressions
  anm ~ EDUCLVL  + AGE + PREGNANT + WEALTHQ  + CHEB
  HWFBMI ~ resid_year + OWNHOUSEWHO + EDUCLVL +  anm + AGE + WEALTHQ + URBANHH
  CHEB ~ resid_year + OWNHOUSEWHO + EDUCLVL + AGE  + WEALTHQ
  OWNHOUSEWHO ~ EDUCLVL
  resid_year ~ EDUCLVL
  
  # correlations
  OWNHOUSEWHO ~~ URBANHH
  resid_year ~~ URBANHH"

model_1 = paste(base_model, "\n \n #output \n anm ~ resid_year + OWNHOUSEWHO", sep = ' ')

fit_1 = sem(data = df_final, model = model_1, ordered = c("EDUCLVL", 
                                                          "PREGNANT", 
                                                          "WEALTHQ", 
                                                          "OWNHOUSEWHO", 
                                                          "URBANHH"))


fit_1_test = sem(data = df_final, model = model_1, std.lv =TRUE)

summary(fit_1, standardized=TRUE, fit.measure=TRUE)
fit_measure_1a = fitMeasures(fit_1)
std_parameters_1a = standardizedSolution(fit_1) 
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

library(tidyverse)
library(scales)

#endow  gender differences 

data_endow <- read.csv2("/Users/loialamarque/Dropbox/Thèse/data/endow/EK_Indiv.csv")%>%
  mutate(RelationshipToHH2 = case_when(RelationshipToHH == "son" ~ "Son", 
                                       RelationshipToHH == "daughter" ~ "Daughter",
                                       T ~ paste0(RelationshipToHH)))%>%
  filter(RelationshipToHH2 != "NA") %>%
  mutate(Edu2 = case_when(Edu == "High" ~ "University", 
                          Edu == "Middle" ~ "Secondary",  
                          Edu == "None" ~ "Primary", 
                          T ~ paste0(Edu)))

#sous-ensemble  khaddhuh
khadduh = data_endow %>%  select(contains("ID"), RelationshipToHH2, Gender, Age)%>%
  filter(Gender ==  "Female", RelationshipToHH2 %in% c("Daughter", "Grand-daughter"))%>%
  group_by(RelationshipToHH2, SharingUnitID)%>%
  filter(Age == min(Age))%>% mutate(khadduh = "yes")%>% select(khadduh, IndivID) %>%
  full_join(data_endow)%>% mutate(khadduh = str_replace_na(khadduh, "no"))
         
#age moyen par niveau d'éducation
khadduh %>% group_by(Edu) %>% summarise(mean_age = mean(Age, na.rm = T))

#coding khadduh 

khadduh %>%
  group_by(khadduh, Edu) %>%
  filter(Age >  25) %>% 
  summarise(educ = n())

data_endow %>%
  group_by(Gender, Edu) %>%
  filter(Age >  25) %>% 
  summarise(educ = n())%>%
  group_by(Gender) %>% summarise(percent = percent(educ/sum(educ)), Edu =  Edu, educ = educ)

data_endow %>%
  group_by(Gender, Edu2) %>%
  filter(Age >  25) %>% 
  summarise(Count = n())%>%
  ggplot(aes(fill = Edu2,  y = Count, x  = Gender))  +  
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() + 
  ggtitle("Education level (>25 yo), N = 98")
ggsave("/Users/loialamarque/Dropbox/Thèse/outputs/barplot_educ.png")

data_endow %>%
  group_by(Gender, Edu2) %>%
  filter(Age >  25) %>% 
  summarise(Count = n())%>%
  ggplot(aes(fill = Edu2,  y = Count, x  = Gender))  +  
  geom_bar(position = "stack", stat = "identity") + 
  theme_minimal() +
  ggtitle("Education level (>25 yo, N = 98)")
ggsave("/Users/loialamarque/Dropbox/Thèse/outputs/barplot_educ2.png")

data_endow %>%
  group_by(Gender, Occupation) %>%
  summarise(Count = n())%>%  drop_na(Occupation) %>%
  ggplot(aes(x = reorder(Occupation, -Count), y = Count, fill = Gender))  +  
  geom_bar(position = "stack", stat = "identity") + facet_grid(~Gender, scales = "free_x")+ 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90)) + 
  xlab("Occupation")
ggsave("/Users/loialamarque/Dropbox/Thèse/outputs/barplot_occupation.png")

#exploring differences in norm-preference

norm_pref  <- readxl::read_xlsx("/Users/loialamarque/Dropbox/Thèse/data/endow/norm_pref.xlsx")%>%
  filter(SharingUnitID != 'Na')%>%
  inner_join(khadduh)


gender_diff_MF <- norm_pref %>%
  filter(Rangbah_khaduh != "NA",  
         norm_pref  != "NA") %>%
  mutate(norm_pref2 = case_when(norm_pref  == 1~ "Equal_Inheritance", 
                                norm_pref == 2 ~ "Khaddhuh_Inheritance", 
                                norm_pref == 3 ~ "Other" ))

# gender_diff_MF %>% group_by(norm_pref2, Rangbah_khaduh)%>%
#   summarise(Count = n()) %>%
#   ggplot(aes(x = norm_pref2, y = Count, fill = Rangbah_khaduh))  +  
#   geom_bar(position = "fill", stat = "identity")  + 
#   theme_minimal() + scale_y_continuous(labels=scales::percent) + 
#   xlab("") + ylab("")

gender_diff_MF %>% group_by(norm_pref2, Rangbah_khaduh, Gender)%>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Rangbah_khaduh, y = Count, fill = norm_pref2))  +  
  geom_bar(position = "fill", stat = "identity")  + 
  theme_minimal() + scale_y_continuous(labels=scales::percent) + facet_grid(~Gender)+ 
  xlab("Youngest daughter/eldest uncle ") + ylab("") + guides(fill=guide_legend(title="Matrilineal norms"))
ggsave("/Users/loialamarque/Dropbox/Thèse/outputs/barplotmatrilineal_norms.png")



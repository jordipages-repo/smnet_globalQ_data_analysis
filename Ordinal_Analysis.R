##
# R Script to analyse effect of moderators (ORDINAL ANALYSIS) on SaltmarshNET questionnaire
# Collaboration with Emma McKinley, Meghan Alexander, Daryl Burdon, Simone Martino and Jordi Pagès.
# Unversitat de Barcelona - Barcelona, 25th March 2019
##

# # # # # # #
# LIBRARIES #
# # # # # # # 

library(tidyverse)
library(tidylog)
library(RColorBrewer)
library(forcats)
# library(gridExtra)
library(ordinal)
library(formula.tools)
library(modelr)
# devtools::install_github("tidymodels/broom")
# library(broom)

# All colour brewer palettes
# display.brewer.all()


# # # # # # # # # # 
# Loading data ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # # # # # # # # # 
smnet <- read_csv('Questionnaire_processed_JFP.csv')
# glimpse(smnet)

# Parsing data (using forcats for factors) and recoding when necessary
smnet <- smnet %>% 
  mutate(Q1 = factor(recode(Q1, 
                            `1` = "Government",
                            `2` = "Government",
                            `3` = "Private",
                            `4` = "Research",
                            `5` = "Research",
                            `6` = "Nonprofit",
                            `7` = "Nonprofit",
                            `8` = "Other",
                            `9` = "Other",
                            `10` = "Other",
                            `11` = "Other",
                            `12`= "Other")),
         Q2 = factor(recode(Q2,
                            `1` = "Researcher",
                            `2` = "Researcher",
                            `3` = "Policy/Planning",
                            `4` = "Policy/Planning",
                            `5` = "Consultant",
                            `6` = "HighManagerialRole",
                            `7` = "FieldRanger",
                            `8` = "Other",
                            `9` = "Researcher",
                            `10` = "Other",
                            `11` = "Other")),
         Q3 = factor(recode(Q3,
                            `1` = "Conservation",
                            `2` = "Agriculture",
                            `3` = "LandOwner",
                            `4` = "NatResManager",
                            `5` = "SustainableDev",
                            `6` = "FloodRiskMgment",
                            `7` = "Hunting",
                            `8` = "Fishing",
                            `9` = "Education",
                            `10` = "HabitatRestoration",
                            `11` = "Research",
                            `12` = "SiteManagement",
                            `13` = "Other")),
         Q5 = factor(recode(Q5,
                            `1` = "<1year",
                            `2` = "1-5years",
                            `3` = "5-10years", 
                            `4` = ">10years")),
         Q6 = factor(recode(Q6,
                            `1` = "<10%",
                            `2` = "10-25%",
                            `3` = "25-50%",
                            `4` = "50-75%",
                            `5` = ">75%")),
         Language = factor(recode(Language,
                                  `1` = "English",
                                  `2` = "French",
                                  `3` = "German",
                                  `4` = "Spanish",
                                  `5` = "Italian", 
                                  `6` = "Portuguese",
                                  `7` = "Welsh",
                                  `8` = "Chinese")),
         Q7_country = factor(recode(Q7_country, 
                                    france = "France",
                                    ITALY = "Italy")),
         Q7_continent = factor(recode(Q7_continent,
                               Australia = "Australia",
                               Africa = "Africa",
                               Asia = "Asia",
                               Europe = "Europe",
                               `North America` = "America",
                               `South America` = "America")),
         Q8 = factor(recode(Q8, 
                            `1` = "DiplomaSecondaryOrLess",
                            `2` = "DiplomaSecondaryOrLess",
                            `3` = "DiplomaSecondaryOrLess",
                            `4` = "Degree",
                            `5` = "Masters",
                            `6` = "PhD",
                            `8` = "Other"))
         )

# glimpse(smnet)


# # # # # # # # # # # # # # # # # # # # # # # # # # 
# ORDINAL ANALYSIS WITH Q9, Q11, Q14, Q15, Q18, Q19 ---------------------------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # 

smnet_long <- smnet %>% 
  select(c(Q1, Q2, Q5, Q6, Q7_country, Q7_continent, Q8, starts_with("Q9"), starts_with("Q11"),
           starts_with("Q14"), Q15, starts_with("Q18"), starts_with("Q19"))) %>% 
  gather(key = items, value = answer, Q9ProtectSLR:Q19Wildfood) %>% 
  filter(answer %in% c(NA, 1:6)) %>% # Because for Q9SuppSensePlace there's an answer == 0 (weird)
  filter(answer != 6) %>%  # To delete unsures, which is difficult to put as being part of an ordered factor.
  mutate(answer = factor(answer, ordered = T),
         items = factor(items)) %>% 
  na.omit()

# Nested data frame
by_items_ordinal <-  smnet_long %>%
  group_by(items) %>% 
  nest()

# by_items_ordinal$data[[1]]

# Now that we have our nested data frame we're in good position to fit some models. 
# We have a model fitting function:
ordinal_items <- function(df){
  fullmodel <- clm(answer ~ Q1 + Q2 + Q5 + Q6 + Q7_continent + Q8, data = df)
  step_selection <- step(fullmodel, trace = 0)
  formula_selected <- step_selection$formula
  clm(formula_selected, data = df)
}

# model <- map(by_items_ordinal$data, ordinal_items)

# And we want to apply it to every data frame. Since the data frames are inside a nested data frame, 
# we use purrr::map() to apply ordinal_items to each eleemnt
by_items_ordinal <- by_items_ordinal %>%
  mutate(model = map(data, ordinal_items))


# car::Anova(by_items_ordinal$model[[1]])
# nominal_test(by_items_ordinal$model[[1]])
# scale_test(by_items_ordinal$model[[1]])

by_items_ordinal
# to extract model quality measures from a model we can use broom::glance()
# For a lot of models, we will use mutate() and unnest()
# by_items_ordinal %>% 
#   mutate(glance = map(model, broom::glance)) %>% 
#   unnest(glance)

# To get the model fit values, we unnest
# If we just want the variables and model outputs without the nested columns, do .drop = TRUE in unnest():
model_all_nested <- by_items_ordinal %>% 
  mutate(hessian = map_dbl(model, "cond.H"),
         formula = map(model, "formula")) 

model_fit_unnested <- model_all_nested %>% 
  mutate(tidy = map(model, broom::tidy)) %>% 
  unnest(tidy) %>% 
  filter(coefficient_type != "alpha")

# model_hessian_formula <- by_items_ordinal %>% 
#   mutate(hessian = map_dbl(model, "cond.H"),
#          formula = map(model, "formula")) %>% 
#   select(items, hessian, formula)

model_Anova_unnested <- model_all_nested %>%
  filter(formula != "answer ~ 1") %>% 
  filter(hessian < 10000) %>%     # This is one of the assumptions of ordinal analysis. 
                                  # It is a measure of how identifiable the model is
  # filter(str_detect(items, "Q9|Q11|Q14|Q15|Q18")) %>% 
  mutate(anova = map(model, car::Anova),
         anova2 = map(anova, broom::tidy)) %>% 
  unnest(anova2)

# write_excel_csv(model_Anova_unnested, "Tables/Ordinal_Main_Effects_Results_14May.csv")
# write_excel_csv(model_fit_unnested, "Tables/Ordinal_Summary_Table_Results_14May.csv")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# WE ARE JUST LACKING NOMINAL AND SCALE TESTS (ASSUMPTIONS OF ORDINAL ANALYSIS)------ 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
a <- smnet_long %>% 
  filter(items == "Q9MinEnvHazards")
ord_a <- clm(answer ~ Q1 + Q2 + Q5 + Q6 + Q7_continent + Q8, data = a)
step(ord_a)
ord_a <- clm(answer ~ Q6 + Q7_continent + Q8, data = a)
nominal_test(ord_a)
scale_test(ord_a)




llista <- as.character(by_items_ordinal$items)
for(i in 1:length(items)){
  a <- smnet_long %>% 
    filter(items == llista[i])
  ord_a <- clm(answer ~ Q1 + Q2 + Q5 + Q6 + Q7_continent + Q8, data = a)
  formula <- step(ord_a, trace = F)$formula
  ord_a <- clm(formula, data = a)
  print(items[i])
  print(formula)
  print(nominal_test(ord_a))
  print(scale_test(ord_a))
}


# Not sure why tests don't work.... and even work worst with the ones below...
nominal_test(by_items_ordinal$model[[5]])
scale_test(by_items_ordinal$model[[5]])



# # # # # # # # # # # # # # # # # # # # # # # # # # 
# NOMINAL ANALYSIS WITH Q12 ---------------------------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # 

# See Bryman 2012 (social research methods). See also https://www.tutorialspoint.com/r/r_chi_square_tests.htm
# Whe both the response and predictor variables are nominal (categorical) we study them with a chi square test +
# Phi or Cramér's V statistics
chisq_Q12 <- smnet %>% 
  select(c(starts_with("Q12"), Q7_continent)) %>% 
  gather(key = items, value = answer, Q12RAMSAR:Q12NotProtect) %>% 
  mutate(answerOK = recode(answer, 
                           `0` = "No",
                           `1` = "Yes")) %>% 
  select(-answer) %>% 
  filter(!is.na(answerOK)) %>%
  filter(!is.na(Q7_continent)) %>% 
  group_by(items) %>% 
  summarise(p_value = chisq.test(Q7_continent, answerOK)$p.value)

#	Chisq.test
# items                 p_value
#	Q12LocalProtect	      7.108766e-01
# Q12NationalProtect	  5.719477e-03 **
# Q12NotProtect	        6.608182e-01
# Q12PrivateProtect	    2.662003e-03 **
# Q12RAMSAR	            1.082846e-07 ***
# Q12SubNationalProtec	4.207584e-06 ***
# Q12SupranatProtect	  1.255424e-24 ***

smnet %>% 
  select(c(starts_with("Q12"), Q7_continent)) %>% 
  gather(key = items, value = answer, Q12RAMSAR:Q12NotProtect) %>% 
  mutate(answerOK = factor(recode(answer, 
                                  `0` = "No",
                                  `1` = "Yes"),
                           levels = c("Yes", "No")),
         items = factor(items,
                        levels = c("Q12RAMSAR",
                                   "Q12SupranatProtect",
                                   "Q12NationalProtect",
                                   "Q12SubNationalProtec",
                                   "Q12LocalProtect",
                                   "Q12PrivateProtect",
                                   "Q12NotProtect"))) %>%
  select(-answer) %>% 
  filter(!is.na(answerOK)) %>%
  filter(!is.na(Q7_continent)) %>% 
  group_by(Q7_continent, items, answerOK) %>% 
  summarise(n = n()) %>%
  mutate(percent = (n/sum(n))) %>% 
  ggplot() +
  geom_bar(aes(y = percent, x = Q7_continent, fill = answerOK), colour = "black", size = 0.2, stat = "identity") +
  scale_fill_manual(values = rev(c("#395B8B", "#4EC173"))) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(fill = "", x = NULL, y = NULL) +
  facet_wrap(~items) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.position = "none", 
        legend.title = element_blank())
# ggsave(filename = "Figs/NEW_Q12_by_continent_chisqOK.pdf")

  
# # # # # # # # # # # # # # # # # # # # # # # # # # 
# NOMINAL ANALYSIS WITH Q13 ---------------------------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # 

chisq_Q13 <- smnet %>% 
  select(c(starts_with("Q13"), Q7_continent)) %>% 
  gather(key = items, value = answer, Q13) %>% 
  mutate(answerOK = recode(answer, 
                           `2` = "No",
                           `1` = "Yes")) %>% 
  select(-answer) %>% 
  filter(!is.na(answerOK)) %>%
  filter(!is.na(Q7_continent)) %>% 
  group_by(items) %>% 
  summarise(p_value = chisq.test(Q7_continent, answerOK)$p.value)

#	Chisq.test
# items     p_value
#	Q13       0.1382057

smnet %>% 
  select(c(starts_with("Q13"), Q7_continent)) %>% 
  gather(key = items, value = answer, Q13) %>% 
  mutate(answerOK = recode(answer, 
                           `2` = "No",
                           `1` = "Yes")) %>% 
  select(-answer) %>% 
  filter(!is.na(answerOK)) %>%
  filter(!is.na(Q7_continent)) %>% 
  group_by(Q7_continent, items, answerOK) %>% 
  summarise(n = n()) %>%
  mutate(percent = 100*(n/sum(n))) %>% 
  ggplot() +
  geom_bar(aes(y = percent, x = Q7_continent, fill = answerOK), stat = "identity") +
  scale_fill_manual(values = c("#395B8B", "#4EC173"))
# ggsave(filename = "Figs/Q13_by_continent_chisq.pdf")



# # # # # # # # # # # # # # # # # # # # # # # # # # 
# NOMINAL ANALYSIS WITH Q17 ---------------------------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # 

chisq_Q17 <- smnet %>% 
  select(c(starts_with("Q17"), Q7_continent)) %>% 
  gather(key = items, value = answer, Q17ClimRegul:Q17NutCycl) %>% 
  mutate(answerOK = recode(answer, 
                           `0` = "No",
                           `1` = "Yes")) %>% 
  select(-answer) %>% 
  filter(!is.na(answerOK)) %>%
  filter(!is.na(Q7_continent)) %>% 
  group_by(items) %>% 
  summarise(p_value = chisq.test(Q7_continent, answerOK)$p.value)

#	Chisq.test
# items                  p_value
# Q17Agric              9.16e- 8
# Q17Arts               3.51e- 5
# Q17ClimRegul          8.25e- 2
# Q17Coast_protect      1.06e- 3
# Q17ConservProtectSpec 7.40e- 4
# Q17Fish               8.48e- 2
# Q17Habitat            4.86e- 4
# Q17Health             1.62e- 4
# Q17Landscape          4.02e- 3
# Q17NutCycl            8.48e-10
# Q17PestRegul          1.84e- 1
# Q17Pollinat           3.96e- 3
# Q17Recre              1.18e- 2
# Q17SensePlace         2.01e- 8
# Q17WaterQualRegul     1.87e- 3
# Q17WildFood           7.89e- 3

smnet %>% 
  select(c(starts_with("Q17"), Q7_continent)) %>% 
  gather(key = items, value = answer, Q17ClimRegul:Q17NutCycl) %>% 
  mutate(answerOK = factor(recode(answer, 
                           `0` = "No",
                           `1` = "Yes"),
                           levels = c("Yes", "No")),
         items = factor(items)) %>%
  select(-answer) %>% 
  filter(!is.na(answerOK)) %>%
  filter(!is.na(Q7_continent)) %>% 
  group_by(Q7_continent, items, answerOK) %>% 
  summarise(n = n()) %>%
  mutate(percent = (n/sum(n))) %>% 
  ggplot() +
  geom_bar(aes(y = percent, x = Q7_continent, fill = answerOK), colour = "black", size = 0.2, stat = "identity") +
  scale_fill_manual(values = rev(c("#395B8B", "#4EC173"))) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(fill = "", x = NULL, y = NULL) +
  facet_wrap(~items) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        legend.position = "none", 
        legend.title = element_blank())
# ggsave(filename = "Figs/NEW_Q17_by_continent_chisqOK.pdf")




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 
# library(car)
# library(RVAideMemoire) # from here https://stats.stackexchange.com/questions/305020/why-does-type-ii-iii-anova-on-cumulative-link-model-r-package-ordinal-give-dif/305982
# car::Anova(by_items$model[[1]])



# Old trials
# group_by(items) %>%
  # do(model = as.character(step(clm(answer ~ Q1 + Q2 + Q5 + Q6 + Q7_continent + Q8, data = .), trace = 0)$formula))
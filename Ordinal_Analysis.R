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
                            `1` = "TechnicalSecondaryOrLess",
                            `2` = "TechnicalSecondaryOrLess",
                            `3` = "Diploma",
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

# model <- map(by_items$data, ordinal_items)

# And we want to apply it to every data frame. Since the data frames are inside a nested data frame, 
# we use purrr::map() to apply ordinal_items to each eleemnt
by_itemsQ9 <- by_itemsQ9 %>%
  mutate(model = map(data, ordinal_items))


# car::Anova(by_items$model[[1]])
# nominal_test(by_items$model[[1]])
# scale_test(by_items$model[[1]])

by_itemsQ9
# to extract model quality measures from a model we can use broom::glance()
# For a lot of models, we will use mutate() and unnest()
# by_itemsQ9 %>% 
#   mutate(glance = map(model, broom::glance)) %>% 
#   unnest(glance)

# To get the model fit values, we unnest
# If we just want the variables and model outputs without the nested columns, do .drop = TRUE in unnest():
model_all_nested <- by_itemsQ9 %>% 
  mutate(hessian = map_dbl(model, "cond.H"),
         formula = map(model, "formula")) 

model_fit_unnested <- model_all_nested %>% 
  mutate(tidy = map(model, broom::tidy)) %>% 
  unnest(tidy) %>% 
  filter(coefficient_type != "alpha")

# model_hessian_formula <- by_itemsQ9 %>% 
#   mutate(hessian = map_dbl(model, "cond.H"),
#          formula = map(model, "formula")) %>% 
#   select(items, hessian, formula)

model_Anova_unnested <- model_all_nested %>%
  filter(formula != "answer ~ 1") %>% 
  mutate(anova = map(model, car::Anova),
         anova2 = map(anova, broom::tidy)) %>% 
  unnest(anova2)







# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 
# library(car)
# library(RVAideMemoire) # from here https://stats.stackexchange.com/questions/305020/why-does-type-ii-iii-anova-on-cumulative-link-model-r-package-ordinal-give-dif/305982
# car::Anova(by_items$model[[1]])



# Old trials
# group_by(items) %>%
  # do(model = as.character(step(clm(answer ~ Q1 + Q2 + Q5 + Q6 + Q7_continent + Q8, data = .), trace = 0)$formula))

  
  

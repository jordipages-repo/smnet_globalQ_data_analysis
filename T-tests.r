##
# R Script to analyse (t-tests) the SaltmarshNET questionnaire
# Collaboration with Emma McKinley, Meghan Alexander, Daryl Burdon, Simone Martino and Jordi Pagès.
# CEAB - Blanes, 5th March 2019
##

# Libraries
library(tidyverse)
library(tidylog)
library(RColorBrewer)
library(forcats)
library(gridExtra)

# All colour brewer palettes
# display.brewer.all()


# # # # # # # # # # 
# Loading data ----
# # # # # # # # # # 
smnet <- read_csv('Questionnaire_processed_JFP.csv')
glimpse(smnet)

# Parsing data (using forcats for factors) and recoding when necessary
smnet <- smnet %>% 
  mutate(Language = factor(recode(Language, `1` = "English", `2` = "French", `3` = "German", `4` = "Spanish", `5` = "Italian", 
                                  `6` = "Portuguese", `7` = "Welsh", `8` = "Chinese")),
         Q7_country = factor(recode(Q7_country, france = "France", ITALY = "Italy")))

glimpse(smnet)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q9.	Main priorities influencing saltmarsh management in your region ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


q9lev <- rev(c("Q9ProtectSLR",
               "Q9ProtectErosion",
               "Q9ProtectSurge",
               "Q9ProtectRiver",
               "Q9MinEnvHazards",
               "Q9PreventBiodivLoss",
               "Q9ReduceImpacts NatResExploit",
               "Q9MinEffectPollut",
               "Q9ReduceExcessNut",
               "Q9SupportClimRegul",
               "Q9ReduceLossLand",
               "Q9ManageInvasives",
               "Q9SuppSensePlace"))

# T-tests
# dplyrTtest <- smnet %>% 
#   select(starts_with("Q9")) %>% 
#   filter(Q9SuppSensePlace %in% c(NA, 1:6)) %>% 
#   gather(key = items, value = answer) %>% 
#   filter(answer != 6) %>% 
#   mutate(answer = answer,
#          items = factor(items, levels = q9lev)) %>% 
#   print() %>% 
#   group_by(items) %>% 
#   summarise(t_test_estimate = t.test(as.data.frame(answer), mu = 3)$estimate,
#             t_confint_low = t.test(as.data.frame(answer), mu = 3)$conf.int[1],
#             t_confint_high = t.test(as.data.frame(answer), mu = 3)$conf.int[2],
#             shapiro_test = shapiro.test(as.numeric(answer))$p.value)

# Agh... shapiro.test is highly significant for all response variables
# I will have to run a wilcoxon test. Instead. 

# Wilcoxon tests
dplyrWilcoxtest <- smnet %>% 
  select(starts_with("Q9")) %>% 
  filter(Q9SuppSensePlace %in% c(NA, 1:6)) %>% 
  gather(key = items, value = answer) %>% 
  filter(answer != 6) %>% 
  mutate(answer = answer,
         items = factor(items, levels = q9lev)) %>% 
  print() %>% 
  group_by(items) %>% 
  summarise(wilcox_test_estimate = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$estimate,
            wilcox_confint_low = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$conf.int[1],
            wilcox_confint_high = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$conf.int[2],
            wilcox_test_pvalue = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$p.value)

count_unsure <- smnet %>% 
  select(starts_with("Q9")) %>% 
  filter(Q9SuppSensePlace %in% c(NA, 1:6)) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = answer,
         items = factor(items, levels = q9lev)) %>% 
  group_by(items) %>% 
  summarise(n = sum(!is.na(answer)),
            unsure = sum(answer == 6, na.rm = T),
            percent_unsure = 100*(unsure/n))

dplyrWilcoxtest$unsures <- count_unsure$percent_unsure

p1 <- ggplot(data = dplyrWilcoxtest, aes(x = items, y = wilcox_test_estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = wilcox_confint_low, ymax = wilcox_confint_high), width=.2, position = position_dodge(.9)) +
  scale_y_continuous(breaks = 1:5, 
                     limits = c(1,5),
                     labels = c("Not\nimportant",
                                "Slightly\nimportant",
                                "Important",
                                "Very\nimportant",
                                "Essential")) +
  scale_x_discrete(labels = c("Support and improve sense of\nidentity and place ",
                              "Managing invasive/\nnon-native species",
                              "Reduce loss of land\nand land reclamation",
                              "Support climate regulation",
                              "Reduce the risks of\nexcess nutrient loading",
                              "Minimise the threat\nand impact of pollution",
                              "Reduce impacts of natural\nresource exploitation",
                              "Prevent or mitigate\nbiodiversity loss",
                              "Minimise other\nenvironmental hazards",
                              "Protect against flooding\nfrom land",
                              "Protect against flooding\nfrom the sea",
                              "Protect against loss of land\nand property due to erosion",
                              "Protect against inundation\ncaused by sea level rise")) +
  labs(fill = '', x = NULL, y = NULL, title = '') +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data = dplyrWilcoxtest, aes(x = items, y = unsures)) +
  # geom_point(size = 2) +
  geom_bar(stat = "identity", fill = "#440154FF") +
  ylim(c(0,20)) +
  scale_x_discrete(labels = c("",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "")) +
  labs(fill = '', x = NULL, y = '% unsure', title = "") +
  coord_flip() +
  theme(plot.margin = unit(c(5.5, 6, 23.3, 0), "pt")) #top, right, bottom, left

ml <- grid.arrange(p1, p2, widths = c(3,1), nrow = 1, top = "Q9. What are the main priorities influencing saltmarsh management in your region?")
# ggsave("Figs/Q9_WilcoxTest.pdf", ml)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q11. Main drivers influencing saltmarsh management in your region ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Clear R memory, because some variables have the same name as in Q9's section.

q11lev <- rev(c("Q11InternatPolicy",
                "Q11NationPolicy",
                "Q11RegionPolicy",
                "Q11LocalPolicy",
                "Q11CulturalMyths",
                "Q11LocalChampions",
                "Q11AvailableMoney",
                "Q11AvailableEquipment-Staff"))

# T-tests
# dplyrTtest <- smnet %>% 
#   select(starts_with("Q11")) %>% 
#   gather(key = items, value = answer) %>% 
#   filter(answer != 6) %>% 
#   mutate(answer = answer,
#          items = factor(items, levels = q11lev)) %>% 
#   print() %>% 
#   group_by(items) %>% 
#   summarise(t_test_estimate = t.test(as.data.frame(answer), mu = 3)$estimate,
#             t_confint_low = t.test(as.data.frame(answer), mu = 3)$conf.int[1],
#             t_confint_high = t.test(as.data.frame(answer), mu = 3)$conf.int[2],
#             shapiro_test = shapiro.test(as.numeric(answer))$p.value)

# Agh... shapiro.test is highly significant for all response variables
# I will have to run a wilcoxon test. Instead. 

# Wilcoxon tests
dplyrWilcoxtest <- smnet %>% 
  select(starts_with("Q11")) %>% 
  gather(key = items, value = answer) %>% 
  filter(answer != 6) %>% 
  mutate(answer = answer,
         items = factor(items, levels = q11lev)) %>% 
  print() %>% 
  group_by(items) %>% 
  summarise(wilcox_test_estimate = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$estimate,
            wilcox_confint_low = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$conf.int[1],
            wilcox_confint_high = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$conf.int[2],
            wilcox_p.value = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$p.value)

count_unsure <- smnet %>% 
  select(starts_with("Q11")) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = answer,
         items = factor(items, levels = q11lev)) %>% 
  group_by(items) %>% 
  summarise(n = sum(!is.na(answer)),
            unsure = sum(answer == 6, na.rm = T),
            percent_unsure = 100*(unsure/n))

dplyrWilcoxtest$unsures <- count_unsure$percent_unsure

p1 <- ggplot(data = dplyrWilcoxtest, aes(x = items, y = wilcox_test_estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = wilcox_confint_low, ymax = wilcox_confint_high), width=.2, position = position_dodge(.9)) +
  scale_y_continuous(breaks = 1:5, 
                     limits = c(1,5),
                     labels = c("Not\nimportant",
                                "Slightly\nimportant",
                                "Important",
                                "Very\nimportant",
                                "Essential")) +
  scale_x_discrete(labels = c("Availability of resources\nto carry out monitoring",
                              "Availability of financial resources",
                              "Local champions\nand/or community action groups",
                              "Cultural myths and traditions",
                              "Local policies",
                              "Policies of your state/region",
                              "National/federal policy\nand legislation",
                              "International policy")) +
  labs(fill = '', x = NULL, y = NULL, title = '') +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data = dplyrWilcoxtest, aes(x = items, y = unsures)) +
  # geom_point(size = 2) +
  geom_bar(stat = "identity", fill = "#440154FF") +
  ylim(c(0,20)) +
  scale_x_discrete(labels = c("",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "")) +
  labs(fill = '', x = NULL, y = '% unsure', title = "") +
  coord_flip() +
  theme(plot.margin = unit(c(5.5, 6, 23.3, 0), "pt")) #top, right, bottom, left

ml <- grid.arrange(p1, p2, widths = c(3,1), nrow = 1, top = "Q11. What are the main drivers influencing saltmarsh management in your region?")
# ggsave("Figs/Q11_WilcoxTest.pdf", ml)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Q14.Level of agreement with statements about saltmarshes, threats and management ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Clear R memory, because some variables have the same name as in Q9's section.

q14lev <- rev(c("Q14A_SM_EffectivelyProtected", 
                      "Q14A_SM_ImpactedSLR",
                      "Q14A_SM_UndervalResource",
                      "Q14A_SM_CCmeansNeed4Mngmnt",
                      "Q14A_CCPositiveCoast",
                      "Q14A_SM_ImpactedUrbanDev",
                      "Q14A_Need2ConserveSMinPolicy",
                      "Q14A_LackResources4SMconserv",
                      "Q14A_Need2ImproveMonitorSM",
                      "Q14A_SMrestorationSuccessful",
                      "Q14A_need4holisticApp",
                      "Q14A_restoringSedInputsCrucial"))

# T-tests
# dplyrTtest <- smnet %>% 
#   select(starts_with("Q14")) %>% 
#   gather(key = items, value = answer) %>% 
#   filter(answer != 6) %>% 
#   mutate(answer = answer,
#          items = factor(items, levels = q14lev)) %>% 
#   print() %>% 
#   group_by(items) %>% 
#   summarise(t_test_estimate = t.test(as.data.frame(answer), mu = 3)$estimate,
#             t_confint_low = t.test(as.data.frame(answer), mu = 3)$conf.int[1],
#             t_confint_high = t.test(as.data.frame(answer), mu = 3)$conf.int[2],
#             shapiro_test = shapiro.test(as.numeric(answer))$p.value)

# Agh... shapiro.test is highly significant for all response variables
# I will have to run a wilcoxon test. Instead. 

# Wilcoxon tests
dplyrWilcoxtest <- smnet %>% 
  select(starts_with("Q14")) %>% 
  gather(key = items, value = answer) %>% 
  filter(answer != 6) %>% 
  mutate(answer = answer,
         items = factor(items, levels = q14lev)) %>% 
  print() %>% 
  group_by(items) %>% 
  summarise(wilcox_test_estimate = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$estimate,
            wilcox_confint_low = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$conf.int[1],
            wilcox_confint_high = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$conf.int[2],
            wilcox_test_pvalue = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$p.value)

count_unsure <- smnet %>% 
  select(starts_with("Q14")) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = answer,
         items = factor(items, levels = q14lev)) %>% 
  group_by(items) %>% 
  summarise(n = sum(!is.na(answer)),
            unsure = sum(answer == 6, na.rm = T),
            percent_unsure = 100*(unsure/n))

dplyrWilcoxtest$unsures <- count_unsure$percent_unsure

p1 <- ggplot(data = dplyrWilcoxtest, aes(x = items, y = wilcox_test_estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = wilcox_confint_low, ymax = wilcox_confint_high), width=.2, position = position_dodge(.9)) +
  scale_y_continuous(breaks = 1:5, 
                     limits = c(1,5),
                     labels = c("Strongly\ndisagree",
                                "Disagree",
                                "Neither agree\nor disagree",
                                "Agree",
                                "Strongly\nagree")) +
  scale_x_discrete(labels = c("Restoring sediment inputs is crucial for the\nmaintenance of marshes under increasing sea levels",
                              "There is a need for a more holistic approach\nto management, taking the entire catchment\ninto consideration",
                              "Saltmarsh restoration activities have been\nsuccessful in re-establishing degraded wetlands",
                              "There is a need to improve\nthe monitoring of saltmarshes",
                              "There are a lack of resources\nfor conserving saltmarshes",
                              "The need to conserve and create\nsaltmarshes is recognised in policy",
                              "Saltmarshes are impacted\nby urban development",
                              "Changing climates can be\npositive for coastal areas",
                              "Climate change means there is a\nneed to ensure saltmarshes\nare well managed",
                              "Saltmarshes are an\nunder-valued resource",
                              "Saltmarshes are impacted\nby sea level rise",
                              "Saltmarshes are effectively\nprotected by existing legislation")) +
  labs(fill = '', x = NULL, y = NULL, title = '') +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data = dplyrWilcoxtest, aes(x = items, y = unsures)) +
  # geom_point(size = 2) +
  geom_bar(stat = "identity", fill = "#440154FF") +
  ylim(c(0,20)) +
  scale_x_discrete(labels = c("",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "")) +
  labs(fill = '', x = NULL, y = '% unsure', title = "") +
  coord_flip() +
  theme(plot.margin = unit(c(5.5, 6, 35, 0), "pt")) #top, right, bottom, left

ml <- grid.arrange(p1, p2, widths = c(3,1), nrow = 1, top = 'Q14. Please indicate your level of agreement with the following statements\nabout saltmarshes, the threats they face and their management')
# ggsave("Figs/Q14_WilcoxTest.pdf", ml)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Q18. How much you agree with the following statements about saltmarshes in your region and their contribution to society ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Clear R memory, because some variables have the same name as in Q9's section.

q18lev <- rev(c("Q18SMimportFoodProvision",
                      "Q18SMprotectFlood",
                      "Q18SMplantsValResource",
                      "Q18SMprovideFoodFish",
                      "Q18SMpreventErosion.",
                      "Q18SMimportantHabitats",
                      "Q18SMimproveWQual",
                      "Q18SMimportantHWellB",
                      "Q18SMvaluableRecreat",
                      "Q18SMimportantCulture",
                      "Q18SMimportantArt",
                      "Q18SMimportantSensePlace",
                      "Q18SMimportantReligion"))

# T-tests
# dplyrTtest <- smnet %>% 
#   select(starts_with("Q18")) %>% 
#   gather(key = items, value = answer) %>% 
#   filter(answer != 6) %>% 
#   mutate(answer = answer,
#          items = factor(items, levels = q18lev)) %>% 
#   print() %>% 
#   group_by(items) %>% 
#   summarise(t_test_estimate = t.test(as.data.frame(answer), mu = 3)$estimate,
#             t_confint_low = t.test(as.data.frame(answer), mu = 3)$conf.int[1],
#             t_confint_high = t.test(as.data.frame(answer), mu = 3)$conf.int[2], 
#             shapiro_test = shapiro.test(as.numeric(answer))$p.value)

# Agh... shapiro.test is highly significant for all response variables
# I will have to run a wilcoxon test. Instead. 

# Wilcoxon tests
dplyrWilcoxtest <- smnet %>% 
  select(starts_with("Q18")) %>% 
  gather(key = items, value = answer) %>% 
  filter(answer != 6) %>% 
  mutate(answer = answer,
         items = factor(items, levels = q18lev)) %>% 
  print() %>% 
  group_by(items) %>% 
  summarise(wilcox_test_estimate = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$estimate,
            wilcox_confint_low = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$conf.int[1],
            wilcox_confint_high = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$conf.int[2])

count_unsure <- smnet %>% 
  select(starts_with("Q18")) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = answer,
         items = factor(items, levels = q18lev)) %>% 
  group_by(items) %>% 
  summarise(n = sum(!is.na(answer)),
            unsure = sum(answer == 6, na.rm = T),
            percent_unsure = 100*(unsure/n))

dplyrWilcoxtest$unsures <- count_unsure$percent_unsure

p1 <- ggplot(data = dplyrWilcoxtest, aes(x = items, y = wilcox_test_estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = wilcox_confint_low, ymax = wilcox_confint_high), width=.2, position = position_dodge(.9)) +
  scale_y_continuous(breaks = 1:5, 
                     limits = c(1,5),
                     labels = c("Strongly\ndisagree",
                                "Disagree",
                                "Neither agree\nor disagree",
                                "Agree",
                                "Strongly\nagree")) +
  scale_x_discrete(labels = c("Saltmarshes are important for spiritual,\nsacred and religious values",
                              "Saltmarshes are important for\na sense of place",
                              "Saltmarshes are important to\ninspire culture art and design",
                              "Saltmarshes are important to\nprotect cultural heritage",
                              "Saltmarshes are a valuable environment\nfor recreation and tourism",
                              "Saltmarshes are important for\nsocietal health and wellbeing",
                              "Saltmarshes improve\nwater quality",
                              "Saltmarshes are important\nhabitats for wildlife",
                              "Saltmarshes can help to prevent\ncoastal erosion",
                              "Saltmarshes provide food\nand shelter for young fish",
                              "Saltmarsh plants are a\nvaluable resource",
                              "Saltmarshes offer protection\nfrom flooding",
                              "Saltmarshes are important\nfor food provision")) +
  labs(fill = '', x = NULL, y = NULL, title = '') +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data = dplyrWilcoxtest, aes(x = items, y = unsures)) +
  # geom_point(size = 2) +
  geom_bar(stat = "identity", fill = "#440154FF") +
  ylim(c(0,20)) +
  scale_x_discrete(labels = c("",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "")) +
  labs(fill = '', x = NULL, y = '% unsure', title = "") +
  coord_flip() +
  theme(plot.margin = unit(c(5.5, 6, 35, 0), "pt")) #top, right, bottom, left

ml <- grid.arrange(p1, p2, widths = c(3,1), nrow = 1, top = 'Q18. Please indicate how much you agree with the following statements\nabout saltmarshes in your region and their contribution to society')
# ggsave("Figs/Q18_WilcoxTest.pdf", ml)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q19.	Please indicate the level of importance of the benefits and services provided by saltmarshes to society ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Clear R memory, because some variables have the same name as in Q9's section.

# T-tests
# dplyrTtest <- smnet %>% 
#   select(starts_with("Q19")) %>% 
#   gather(key = items, value = answer) %>% 
#   filter(answer != 6) %>% 
#   mutate(answer = answer,
#          items = factor(items)) %>% 
#   print() %>% 
#   group_by(items) %>% 
#   summarise(t_test_estimate = t.test(as.data.frame(answer), mu = 3)$estimate,
#             t_confint_low = t.test(as.data.frame(answer), mu = 3)$conf.int[1],
#             t_confint_high = t.test(as.data.frame(answer), mu = 3)$conf.int[2],
#             shapiro_test = shapiro.test(as.numeric(answer))$p.value)
            
# Agh... shapiro.test is highly significant for all response variables
# I will have to run a wilcoxon test. Instead. 

# Wilcoxon tests
dplyrWilcoxtest <- smnet %>% 
  select(starts_with("Q19")) %>% 
  gather(key = items, value = answer) %>% 
  filter(answer != 6) %>% 
  mutate(answer = answer,
         items = factor(items)) %>% 
  print() %>% 
  group_by(items) %>% 
  summarise(wilcox_test_estimate = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$estimate,
            wilcox_confint_low = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$conf.int[1],
            wilcox_confint_high = wilcox.test(as.numeric(answer), mu = 3, conf.int = T)$conf.int[2])


count_unsure <- smnet %>% 
  select(starts_with("Q19")) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = answer,
         items = factor(items)) %>% 
  group_by(items) %>% 
  summarise(n = sum(!is.na(answer)),
            unsure = sum(answer == 6, na.rm = T),
            percent_unsure = 100*(unsure/n))

dplyrWilcoxtest$unsures <- count_unsure$percent_unsure

p1 <- ggplot(data = dplyrWilcoxtest, aes(x = items, y = wilcox_test_estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = wilcox_confint_low, ymax = wilcox_confint_high), width=.2, position = position_dodge(.9)) +
  scale_y_continuous(breaks = 1:5, 
                     limits = c(1,5),
                     labels = c("No benefit", 
                                "Slightly\nbeneficial",
                                "Somewhat\nbeneficial",
                                "Moderately\nbeneficial",
                                "Very\nbeneficial")) +
  scale_x_discrete(labels = c("Agricultural land",
                              "Coastal protection\nfrom flooding",
                              "Carbon storage",
                              "Environmental education",
                              "Habitats for biodiversity",
                              "Health and wellbeing",
                              "Natural landscape",
                              "Nursery habitats for fisheries",
                              "Pollination",
                              "Prevention of coastal erosion",
                              "Recreation e.g. birdwatching",
                              "Reducing climate change\nimpacts",
                              "Reducing impacts of waste\nand pollution",
                              "Tourism",
                              "Wild food and foraging")) +
  labs(fill = '', x = NULL, y = NULL, title = '') +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


p2 <- ggplot(data = dplyrWilcoxtest, aes(x = items, y = unsures)) +
  # geom_point(size = 2) +
  geom_bar(stat = "identity", fill = "#440154FF") +
  ylim(c(0,20)) +
  scale_x_discrete(labels = c("",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "")) +
  labs(fill = '', x = NULL, y = '% unsure', title = "") +
  coord_flip() +
  theme(plot.margin = unit(c(5.5, 6, 28, 0), "pt")) #top, right, bottom, left

ml <- grid.arrange(p1, p2, widths = c(3,1), nrow = 1, top = 'Q19. Please indicate the level of importance of the benefits\nand services provided by saltmarshes to society')
# ggsave("Figs/Q19_WilcoxTest.pdf", ml)






# RETALLS ----
######

# # # # # # # 
# Filtering out NA rows
# # # # # # # 

# This deletes all rows that contain at least 1 NA (BOTH FILTERS DO THE SAME)
smnet %>% 
  filter(!complete.cases(.))

smnet %>% 
  filter_all(any_vars(is.na(.))) # any_vars() uses the union (|)

# This deletes rows with NA in the last column, to filter out those respondents that didn't get to the end of the questionnaire
smnet %>% 
  filter(!is.na(Q19Wildfood))

# This deletes rows with more than 86 NA's, which are all of those respondents 
# that did not answer any of the questions after the ones related to demographics
smnet2 <- smnet %>% 
  filter(rowSums(is.na(.)) < 86)




# Old way of doing t.tests in a FOR LOOP


q9 <- smnet %>% 
  select(starts_with("Q9")) %>% 
  # filter(complete.cases(.)) %>%
  # filter(is.na(Q9SuppSensePlace))
  filter(Q9SuppSensePlace %in% c(NA, 1:6)) %>% # Because there's a 0, and the rest of options also drop NAs
  # filter(Q9SuppSensePlace != 0) %>%
  print()

testimates <- NULL
tconfint1 <- NULL
tconfint2 <- NULL
unsuretot <- NULL
for(i in 1:length(unique(q9))){
  data_now_drop_na <- q9 %>% 
    select(i) %>% 
    filter(complete.cases(.))
  data_now <- data_now_drop_na %>% 
    filter(. != 6) # it filters out 6, but also NAs
  unsure <- 1-(nrow(data_now)/nrow(data_now_drop_na))
  tstudent <- t.test(as.data.frame(data_now), mu = 3)
  testimates <- c(testimates, tstudent$estimate)
  tconfint1 <- c(tconfint1, tstudent$conf.int[1])
  tconfint2 <- c(tconfint2, tstudent$conf.int[2])
  unsuretot <- c(unsuretot, unsure)
}
names(testimates) <- colnames(q9)
names(tconfint1) <- colnames(tconfint1)
names(tconfint2) <- colnames(tconfint2)
names(unsuretot) <- colnames(unsuretot)

tests_q9 <- data.frame(testimates, tconfint1, tconfint2, unsuretot)
tests_q9$x <- rownames(tests_q9)
tests_q9$unsuretot <- round(tests_q9$unsuretot*100)
tests_q9

p1 <- ggplot(data = tests_q9, aes(x = x, y = testimates)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = tconfint1, ymax = tconfint2), width=.2, position = position_dodge(.9)) +
  scale_y_continuous(breaks = 1:5, 
                     limits = c(1,5),
                     labels = c("Not\nimportant",
                                "Slightly\nimportant",
                                "Important",
                                "Very\nimportant",
                                "Essential")) +
  scale_x_discrete(labels = rev(c("Support and improve sense of\nidentity and place ",
                                  "Support climate regulation",
                                  "Reduce loss of land\nand land reclamation",
                                  "Reduce impacts of natural\nresource exploitation",
                                  "Reduce the risks of\nexcess nutrient loading",
                                  "Protect against flooding\nfrom the sea",
                                  "Protect against inundation\ncaused by sea level rise",
                                  "Protect against flooding\nfrom land",
                                  "Protect against loss of land\nand property due to erosion",
                                  "Prevent or mitigate\nbiodiversity loss",
                                  "Minimise other\nenvironmental hazards",
                                  "Minimise the threat\nand impact of pollution",
                                  "Managing invasive/\nnon-native species"
  ))) +
  labs(fill = '', x = NULL, y = NULL, title = '') +
  coord_flip() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data = tests_q9, aes(x = x, y = unsuretot)) +
  # geom_point(size = 2) +
  geom_bar(stat = "identity", fill = "#440154FF") +
  ylim(c(0,20)) +
  scale_x_discrete(labels = c("",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "",
                              "")) +
  labs(fill = '', x = NULL, y = '% unsure', title = "") +
  coord_flip()

ml <- grid.arrange(p1, p2, widths = c(3,1), nrow = 1, top = "Q9. What are the main priorities influencing saltmarsh management in your region?")
ggsave("Q9_Ttest.pdf", ml)



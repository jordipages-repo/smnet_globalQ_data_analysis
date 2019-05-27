##
# R Script to visualise (plot) the SaltmarshNET questionnaire
# Collaboration with Emma McKinley, Meghan Alexander, Daryl Burdon, Simone Martino and Jordi Pagès.
# CEAB - Blanes, 20th February 2019
##


# Libraries
library(tidyverse)
library(tidylog)
library(RColorBrewer)
library(forcats)

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



# # # # # # # # # # # # 
# Data exploration ----
# # # # # # # # # # # # 

# Respondents' language ----
# Barplot count
smnet %>% 
  group_by(Language) %>% 
  summarise(n = n()) %>% 
  print(n = Inf) %>% 
  ggplot(aes(x = fct_reorder(Language, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#3d84d2") +
  ylab("Number of respondents") +
  xlab("Language")
# ggsave("Q0_Barplot_Language_count.pdf")

# Barplot percentage
smnet %>% 
  group_by(Language) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  print(n = Inf) %>% 
  ggplot(aes(x = fct_reorder(Language, -n), y = freq*100, group = 1)) +
  geom_bar(stat = "identity", fill = "#3d84d2") +
  ylab("% of respondents") +
  xlab("Language")
# ggsave("Q0_Barplot_Language_percentage.pdf")

# Barplot by Language by country
smnet %>% 
  group_by(Language, Q7_country) %>% 
  summarise(n = n()) %>%
  print(n = Inf) %>%
  ggplot(aes(x = Q7_country, y = n,  fill = Language)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = rev(c("#9FDA3AAA", "#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", 
                               "#46337EFF", "#440154FF"))) +
  ylab("") +
  xlab("Country") +
  coord_flip()
# ggsave("Q0_Barplot_Language_by_Country_count.pdf")
  

# % respondents by continent
smnet %>% 
  group_by(Q7_continent) %>% 
  filter(!is.na(Q7_continent)) %>% 
  summarise(n = n()) %>% 
  mutate(percent = 100*(n/sum(n)))


# Mapping the number of respondents, by country of origin ----
smnetcountry <- smnet %>% 
  group_by(Q7_country) %>% 
  filter(!is.na(Q7_country)) %>% 
  summarise(n = n())

unique(smnetcountry$Q7_country)

# Get map
map.world <- map_data('world')
ggplot(map.world, aes(x = long, y = lat, group = group)) +
  geom_polygon()

# Checking potential join mismatches
smnetcountry %>% 
  anti_join(map.world, by = c('Q7_country' = 'region')) %>% 
  print(n = Inf)
# Ok, no mismatches. Useful to see what will not be joined. In this case, nothing. Everything will be joined.

# So we can proceed with the left_join
map.smnet <- left_join(map.world, smnetcountry, by = c('region' = 'Q7_country'))

# Map of number of respondents per country
ggplot(map.smnet, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = n)) + 
  # scale_fill_gradientn(colours = brewer.pal(5, "YlOrRd"), trans = "log10", na.value = "#d0d0d0") +
  scale_fill_gradientn(colours = rev(c("#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", 
                                       "#46337EFF")), trans = "log10", na.value = "#d0d0d0") +
  # scale_fill_gradientn(colours = brewer.pal(5, "Blues"), trans = "log10") +
  # theme_minimal()
  labs(fill = '', x = NULL, y = NULL) +
  theme(text = element_text(color = '#EEEEEE'), axis.ticks = element_blank(), axis.text = element_blank(), 
        panel.grid = element_blank(), panel.background = element_rect(fill = '#787878'), 
        plot.background = element_rect(fill = '#787878'), legend.position = c(.18,.36) ,
        legend.background = element_blank(), legend.key = element_blank())
# ggsave(filename = "Q7_RespondentsCountryMapBlues.pdf")



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

q9answerlev <- rev(c("Unsure", "Not important", "Slightly important", "Important", 
                            "Very important", "Essential"))

smnet %>% 
  select(starts_with("Q9")) %>% 
  filter(Q9SuppSensePlace != 0) %>% # Because there's a 0 
  print() %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(recode(answer,
                                `1` = "Not important",
                                `2` = "Slightly important",
                                `3` = "Important",
                                `4` = "Very important",
                                `5` = "Essential",
                                `6` = "Unsure"), 
                         levels = q9answerlev),
         items = factor(items, levels = q9lev)) %>% 
  filter(complete.cases(.)) %>%
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill") +
  scale_fill_manual(values = c("#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", 
                               "#46337EFF", "#440154FF")) +
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
  labs(fill = '', x = NULL, y = NULL, title = "Q9. What are the main priorities influencing saltmarsh management in your region?") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.6))
# ggsave(filename = "Q9_Barplot_Stacked.pdf")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q11. Main drivers influencing saltmarsh management in your region ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

q11lev <- rev(c("Q11InternatPolicy",
                "Q11NationPolicy",
                "Q11RegionPolicy",
                "Q11LocalPolicy",
                "Q11CulturalMyths",
                "Q11LocalChampions",
                "Q11AvailableMoney",
                "Q11AvailableEquipment-Staff"))

q11answerlev <- rev(c("Unsure", "Not important", "Slightly important", "Important", 
                     "Very important", "Essential"))

smnet %>% 
  select(starts_with("Q11")) %>% 
  print() %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(recode(answer,
                                `1` = "Not important",
                                `2` = "Slightly important",
                                `3` = "Important",
                                `4` = "Very important",
                                `5` = "Essential",
                                `6` = "Unsure"), 
                         levels = q11answerlev),
         items = factor(items, levels = q11lev)) %>% 
  filter(complete.cases(.)) %>%
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill") +
  scale_fill_manual(values = c("#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", 
                               "#46337EFF", "#440154FF")) +
  scale_x_discrete(labels = c("Availability of resources\nto carry out monitoring",
                              "Availability of financial resources",
                              "Local champions\nand/or community action groups",
                              "Cultural myths and traditions",
                              "Local policies",
                              "Policies of your state/region",
                              "National/federal policy\nand legislation",
                              "International policy")) +
  labs(fill = '', x = NULL, y = NULL, title = 'Q11. What are the main drivers influencing saltmarsh management in your region?') +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.6))
# ggsave(filename = "Q11_Barplot_Stacked.pdf")

  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Q12. Indicate if there are any specific forms of protection for the saltmarsh you currently work in ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

q12lev <- rev(c("Q12RAMSAR",
            "Q12SupranatProtect",
            "Q12NationalProtect",
            "Q12SubNationalProtec",
            "Q12LocalProtect",
            "Q12PrivateProtect",
            "Q12NotProtect"))


smnet %>% 
  select(starts_with("Q12")) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(recode(answer, `0` = "No", `1` = "Yes"), levels = c("Yes", "No")),
         items = factor(items, levels = q12lev)) %>% 
  filter(complete.cases(.)) %>%
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill") +
  scale_fill_manual(values = c("#4EC173", "#395B8B")) +
  scale_x_discrete(labels = c("Not protected",
                              "Privately owned protected area",
                              "Area designated due to local importance",
                              "Area designated due to sub-national importance",
                              "Area designated due to national importance",
                              "Area listed in other supranational legislation",
                              "Area listed in the RAMSAR convention")) +
  labs(fill = '', x = NULL, y = NULL, title = 'Q12. Please indicate if there are any specific forms of land\nprotection for the area(s) of saltmarsh you currently work in') +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.6))
# ggsave(filename = "Q12_Barplot_Stacked.pdf")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Q13. Are there any uses that are limited/ prohibited through the land protection mentioned above?  ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

smnet %>% 
  select(starts_with("Q13")) %>% 
  filter(complete.cases(.)) %>%
  mutate(Q13 = fct_infreq(factor(recode(Q13, `1` = "Yes", `2` = "No")))) %>% 
  group_by(Q13) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = Q13, y = freq)) +
  geom_bar(aes(fill = Q13), stat = "identity") +
  scale_fill_manual(values = c("#395B8B", "#4EC173")) +
  labs(fill = '', x = NULL, y = "Number of responses", title = 'Q13. Are there any uses that are limited/prohibited\nthrough the land protection mentioned above?') +
  theme(plot.title = element_text(hjust = 0.6))
# ggsave(filename = "Q13_Barplot_Stacked.pdf")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Q14.Level of agreement with statements about saltmarshes, threats and management ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

q14lev.items <- rev(c("Q14A_SM_EffectivelyProtected", 
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

q14answerlev <- rev(c("Unsure",
                  "Strongly disagree",
                  "Disagree",
                  "Neither agree or disagree",
                  "Agree",
                  "Strongly agree"))

smnet %>% 
  select(starts_with("Q14")) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(recode(answer,
                                `1` = "Strongly disagree",
                                `2` = "Disagree",
                                `3` = "Neither agree or disagree",
                                `4` = "Agree",
                                `5` = "Strongly agree",
                                `6` = "Unsure"),
                         levels = q14answerlev),
         items = factor(items, levels = q14lev.items)) %>% 
  filter(complete.cases(.)) %>%
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill") +
  scale_fill_manual(values = c("#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", 
                               "#46337EFF", "#440154FF"), labels = rev(c("Unsure",
                                                                         "Strongly disagree",
                                                                         "Disagree",
                                                                         "Neither agree\nor disagree",
                                                                         "Agree",
                                                                         "Strongly agree"))) +
  scale_x_discrete(labels = c("Restoring sediment inputs is crucial\nfor the maintenance of salt marshes\nunder increasing sea levels",
                              "There is a need for a more holistic approach\nto management, taking the entire catchment\ninto consideration",
                              "Saltmarsh restoration activities have been\nsuccessful in re-establishing\ndegraded wetlands",
                              "There is a need to improve\nthe monitoring of saltmarshes",
                              "There are a lack of resources\nfor conserving saltmarshes",
                              "The need to conserve and create\nsaltmarshes is recognised in policy",
                              "Saltmarshes are impacted\nby urban development",
                              "Changing climates can be\npositive for coastal areas",
                              "Climate change means there is a\nneed to ensure saltmarshes\nare well managed",
                              "Saltmarshes are an\nunder-valued resource",
                              "Saltmarshes are impacted\nby sea level rise",
                              "Saltmarshes are effectively\nprotected by existing legislation")) +
  labs(fill = '', x = NULL, y = NULL, title = 'Q14. Please indicate your level of agreement with the following statements\nabout saltmarshes, the threats they face and their management') +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.6))
# ggsave(filename = "Q14_Barplot_Stacked.pdf")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Q15. Overall, how would you rate the effectiveness of saltmarsh management in your region? ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

smnet %>% 
  select(starts_with("Q15")) %>% 
  filter(complete.cases(.)) %>%
  mutate(Q15 = factor(Q15)) %>% 
  group_by(Q15) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n)) %>% 
  ggplot(aes(x = Q15, y = freq)) +
  geom_bar(aes(fill = Q15), stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", "#46337EFF")) +
  scale_x_discrete(labels = c("Not effective", 
                              "Somewhat effective",
                              "Neither effective or ineffective",
                              "Effective",
                              "Very effective")) +
  labs(x = NULL, y = "Relative frequency of responses", title = 'Q15.Overall, how would you rate the effectiveness\nof saltmarsh management in your region?') +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.6))
# ggsave(filename = "Q15_Barplot.pdf")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Q17. What services and benefits are provided by saltmarsh in your region? Please tick all that apply ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


smnet %>% 
  select(starts_with("Q17")) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(recode(answer, 
                                `0` = "No",
                                `1` = "Yes"), 
                         levels = c("Yes", "No")),
         items = factor(items)) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill") +
  scale_fill_manual(values = rev(c("#395B8B", "#4EC173"))) +
  scale_x_discrete(labels = c("Agriculture", 
                              "Inspiration for the arts ",
                              "Climate regulation ",
                              "Coastal protection/defence\nagainst flood and erosion risks",
                              "Management/conservation of\nprotected species",
                              "Fisheries/fish nursery habitats",
                              "Provision of habitat for natural\nbiodiversity",
                              "Physical and mental health",
                              "Natural landscape",
                              "Nutrient cycling",
                              "Disease/pest regulation",
                              "Pollination",
                              "Recreation",
                              "Sense of place and\ncultural connections",
                              "Water quality regulation",
                              "Wild food foraging and/or\nwild fowling")) +
  labs(fill = "", x = NULL, y = "Relative frequency of responses", title = 'Q17. What services and benefits are provided by saltmarsh in your region?') +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.6))
# ggsave(filename = "Q17_Barplot_Stacked.pdf")
  
  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Q18. How much you agree with the following statements about saltmarshes in your region and their contribution to society ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


q18lev.items <- rev(c("Q18SMimportFoodProvision",
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

q18answerlev <- rev(c("Unsure",
                  "Strongly disagree", 
                  "Disagree", 
                  "Neither agree or disagree", 
                  "Agree", 
                  "Strongly agree"))

smnet %>% 
  select(starts_with("Q18")) %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(recode(answer,
                                `1` = "Strongly disagree",
                                `2` = "Disagree",
                                `3` = "Neither agree or disagree",
                                `4` = "Agree",
                                `5` = "Strongly agree",
                                `6` = "Unsure"),
                         levels = q18answerlev),
         items = factor(items, levels = q18lev.items)) %>% 
  filter(complete.cases(.)) %>%
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill") +
  scale_fill_manual(values = c("#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", 
                               "#46337EFF", "#440154FF"), labels = rev(c("Unsure",
                                                                         "Strongly disagree", 
                                                                         "Disagree", 
                                                                         "Neither agree\nor disagree", 
                                                                         "Agree", 
                                                                         "Strongly agree"))) +
  scale_x_discrete(labels = c("Saltmarshes are important for spiritual,\nsacred and religious values",
                              "Saltmarshes are important for\na sense of place",
                              "Saltmarshes are important to\ninspire culture art and design",
                              "Saltmarshes are important to\nprotect cultural heritage",
                              "Saltmarshes are a valuable environment\nfor recreation and tourism",
                              "Saltmarshes are important for\nsocietal health and wellbeing",
                              "Saltmarshes improve water\nquality",
                              "Saltmarshes are important\nhabitats for wildlife",
                              "Saltmarshes can help to prevent\ncoastal erosion",
                              "Saltmarshes provide food\nand shelter for young fish",
                              "Saltmarsh plants are a\nvaluable resource",
                              "Saltmarshes offer protection\nfrom flooding",
                              "Saltmarshes are important\nfor food provision")) +
  labs(fill = '', x = NULL, y = NULL, title = 'Q18. Please indicate how much you agree with the following statements\nabout saltmarshes in your region and their contribution to society') +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.6))
# ggsave(filename = "Q18_Barplot_Stacked.pdf")




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Q19.	Please indicate the level of importance of the benefits and services provided by saltmarshes to society ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


smnet %>% 
  select(starts_with("Q19")) %>% 
  print() %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(recode(answer, `1` = "No benefit", 
                                `2` = "Slightly beneficial", 
                                `3`= "Somewhat beneficial", 
                                `4` = "Moderately beneficial",
                                `5` = "Very beneficial", 
                                `6` = "Unsure"),
                         levels = rev(c("Unsure",
                                    "No benefit", 
                                    "Slightly beneficial",
                                    "Somewhat beneficial",
                                    "Moderately beneficial",
                                    "Very beneficial"))),
         items = factor(items)) %>% 
  filter(complete.cases(.)) %>%
  ggplot(aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill", na.rm = T) +
  scale_fill_manual(values = c("#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", 
                               "#46337EFF", "#440154FF")) + 
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
  labs(fill = '', x = NULL, y = NULL, title = 'Q19. Please indicate the level of importance of the benefits\nand services provided by saltmarshes to society') +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.6))
# ggsave(filename = "Q19_Barplot_Stacked.pdf")






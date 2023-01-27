### Victor BRUN
### Created 08-03-2021
### Analysis of synthesized themes from "perception MPAs" data

## Packages and data ----

# Packages
rm(list=ls()) ; library(tidyverse) ; library(ggplot2) ; library(plyr)

# Data
df <-  read.csv("Data/general_matrix_synthesis.csv")


## Descriptive statistics ----

# Descriptive stats
table(df$Gender)
table(df$Residence)
table(df$Job_1)
table(df$Involved_project) # No = 36, Yes = 30
table(df$Knowledge_NGO) # No = 38, Yes = 28
table(df$Knowledge_MPA) # No = 5, Yes = 61

# Summary of all perceptions
df_summary <-  df %>% 
  select(13:36) %>%
  summarise_all(sum)

df_summary <-  t(df_summary) %>% data.frame
colnames(df_summary) <- "Total"
df_summary$Perception <- rownames(df_summary)

ggplot(df_summary) +
  aes(x = Perception, y = Total) +
  geom_col(position = "dodge") +
  labs(x = "", y = "N respondents") +
  theme_minimal() +
  coord_flip()

## Perception per group ----

# Creating "df2" summing all the answers per group
df2 = df %>% 
  group_by(Knowledge_NGO) %>%
  select(13:36) %>%
  summarise_all(sum) %>%
  t() %>%
  data.frame 
colnames(df2) = c("No previous knowledge of the NGO", "Previous knowledge of the NGO")
df2 <-  df2[-1,] %>%
  mutate_all(as.numeric)
df2[,1] <- df2[,1]/38
df2[,2] <- df2[,2]/28

# Relevel factors
df2$Perception = rownames(df2)
df2$Perception <- factor(df2$Perception, levels = c("value_livelihood",
                                    "value_food",
                                    "value_cultural",
                                    "value_services",
                                    "value_intrinsic",
                                    "value_no_value",
                                    "issue_destructive_practices",
                                    "issue_depletion_resources",
                                    "issue_deforestation",
                                    "issue_social_causes",
                                    "issue_pollution",
                                    "issue_climate_disasters",
                                    "issue_no",
                                    "issue_landbased_stressors",
                                    "issue_agriculture_water",
                                    "issue_conservation",
                                    "solution_sociocentric_coercive",
                                    "solution_capacity_alternative",
                                    "solution_ecocentric",
                                    "solution_no",
                                    "mpas_ecological_sustainability",
                                    "mpas_fishery",
                                    "mpas_coercive",
                                    "mpas_for_locals"))
df2$Perception <- fct_rev(df2$Perception)

# Representing all proportions using a barplot
df3 = pivot_longer(df2, c(`No previous knowledge of the NGO`, `Previous knowledge of the NGO`))
barplot = ggplot(df3) +
  aes(x = Perception, y = value) +
  geom_col(aes(fill = name), position = "dodge") +
  labs(x = "", y = "% of respondents that identified this theme", fill = "Group of respondents") +
  scale_fill_manual(values = c("black", "light grey")) +
  theme_minimal()
barplot + coord_flip()







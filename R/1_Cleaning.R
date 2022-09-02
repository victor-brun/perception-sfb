### Victor BRUN
### Started 07-03-2021
### Gathering individual perceptions into larger themes on perception SFB study

rm(list=ls()) ; library(tidyr) ; library(dplyr)

df = read.csv("data/df_quanti.csv", sep=";")

# Adding a "Fisherman" variable for people that declared it as a main or secondary occupation
df = df %>%
  mutate(Fisherman = case_when(Job_1 == "Fisherman" ~ "Yes",
                               Job_2 == "Fisherman" ~ "Yes", 
                               TRUE ~ "No"))

# Adding a "Group" variable for which 1 == villages where Sulubaai has been active 
# and 2 == villages for which Sulubaai has not
df = df %>%
  mutate(Involved_project = case_when(df$Residence == "Batas" ~ "No",
                                      df$Residence == "Mabini" ~ "No",
                                      df$Residence == "Silangga" ~ "No",
                                      TRUE ~ "Yes"))

# Creating df2 that will serve as the dataframe to summarize themes
df2 = df %>%
  select(ID, Session, Sex, Residence, Age, Job_1, Job_2, Fisherman, Involved_project)

### ADDING VARIABLES BY GATHERING DIFFERENT INDIVIDUAL PERCEPTIONS

## 1) ENVIRONMENTAL STRESSORS  ----

# Destructive and illegal fishing practices
df2 = df2 %>%
  mutate(issue_destructive_practices = case_when(df$X1_illegal==1 ~ 1,
                                           df$X1_dynamite==1 ~ 1,
                                           df$X1_cyanide==1 ~ 1,
                                           df$X1_compressor==1 ~ 1,
                                           df$X1_comfishing==1 ~ 1,
                                           df$X1_smallnet==1 ~ 1,
                                           df$X3_illegal==1 ~ 1,
                                           df$X3_dynamite==1 ~ 1,
                                           df$X3_cyanide==1 ~ 1,
                                           df$X3_compressor==1 ~ 1,
                                           df$X3_trawl==1 ~ 1,
                                           df$X3_smallnet==1 ~ 1,
                                           TRUE ~ 0))

# Noticed a depletion of marine resources
df2 = df2 %>%
  mutate(issue_depletion_resources = case_when(df$X1_fewfish==1 ~ 1,
                                                 df$X3_overfishing==1 ~ 1,
                                                 df$X3_smallfish==1 ~ 1,
                                                 df$X3_fewfish==1 ~ 1,
                                                 df$X3_farfish==1 ~ 1,
                                                 df$X3_brokencorals==1 ~ 1,
                                                 TRUE ~ 0))

# Terrestrial issues (mining, deforestation, unsustainable agricultural practices)
df2 = df2 %>%
  mutate(issue_terrestrial = case_when(df$X1_logging==1 ~ 1,
                                                 df$X1_kaingin==1 ~ 1,
                                                 df$X1_mangroves==1 ~ 1,
                                                 df$X1_forest==1 ~ 1,
                                                 df$X1_fauna==1 ~ 1,
                                                 df$X1_pesticides==1 ~ 1,
                                                 df$X1_plantations==1 ~ 1,
                                                 df$X1_pests==1 ~ 1,
                                                 df$X1_fertilizers==1 ~ 1,
                                                 TRUE ~ 0))

# Other direct pressures (tourism, pollutions, etc.)
df2 = df2 %>%
  mutate(issue_other_pressures = case_when(df$X1_plastic==1 ~ 1,
                                                 df$X1_wastew==1 ~ 1,
                                                 df$X1_trash==1 ~ 1,
                                                 df$X1_infrastructure==1 ~ 1,
                                                 df$X1_tourism==1 ~ 1,
                                                 df$X1_climatechange==1 ~ 1,
                                                 df$X3_tourism==1 ~ 1,
                                                 df$X3_pollution==1 ~ 1,
                                                 df$X3_pearlfarms==1 ~ 1,
                                                 TRUE ~ 0))

# Root social causes (poverty, demography, lack of collaboration, of respect, thieves, etc.)
df2 = df2 %>%
  mutate(issue_social_causes = case_when(df$X1_demography==1 ~ 1,
                                           df$X1_dependance==1 ~ 1,
                                           df$X1_poverty==1 ~ 1,
                                           df$X1_education==1 ~ 1,
                                           df$X1_policy==1 ~ 1,
                                           df$X3_lowprice==1 ~ 1,
                                           df$X3_empower==1 ~ 1,
                                           df$X3_collaboration==1 ~ 1,
                                           df$X3_lackgovernment==1 ~ 1,
                                           df$X3_thefts==1 ~ 1,
                                           df$X3_respect==1 ~ 1,
                                           df$X3_inequality==1 ~ 1,
                                           TRUE ~ 0))

# Unmanagable causes (extreme events, bad luck & general weather patterns)
df2 = df2 %>%
  mutate(issue_unmanagable_causes = case_when(df$X1_events==1 ~ 1,
                                         df$X3_noluck==1 ~ 1,
                                         df$X3_weather==1 ~ 1,
                                         TRUE ~ 0))

# No issue
df2 = df2 %>%
  mutate(issue_no = case_when(df$X1_no==1 ~ 1,
                              df$X3_no==1 ~ 1,
                              TRUE ~ 0))


## VALUES ----

# Livelihood, natural resources and money derived from them
df2 = df2 %>%
  mutate(value_livelihood = case_when(df$X2_livelihood==1 ~ 1,
                                         df$X2_natres==1 ~ 1,
                                         df$X2_fishermen==1 ~ 1,
                                         df$X2_money==1 ~ 1,
                                         df$X2_seaweeds==1 ~ 1,
                                         TRUE ~ 0))

# Food, proteins and benefits for health
df2 = df2 %>%
  mutate(value_food = case_when(df$X2_food==1 ~ 1,
                                         df$X2_protein==1 ~ 1,
                                         df$X2_health==1 ~ 1,
                                         df$X2_money==1 ~ 1,
                                         df$X2_seaweeds==1 ~ 1,
                                         TRUE ~ 0))

# Other services provided by the ocean
df2 = df2 %>%
  mutate(value_services = case_when(df$X2_biodiversity==1 ~ 1,
                                         df$X2_species==1 ~ 1,
                                         df$X2_nature==1 ~ 1,
                                         df$X2_beautiful==1 ~ 1,
                                         df$X2_services==1 ~ 1,
                                         df$X2_carbon==1 ~ 1,
                                         df$X2_coastalprot==1 ~ 1,
                                         df$X2_connectivity==1 ~ 1,
                                         df$X2_tourism==1 ~ 1,
                                         TRUE ~ 0))

# Traditional, religious and generational aspects
df2 = df2 %>%
  mutate(value_tradition = case_when(df$X2_nextgen==1 ~ 1,
                                         df$X2_godgift==1 ~ 1,
                                         df$X2_tradition==1 ~ 1,
                                         TRUE ~ 0))

## SOLUTIONS -----

# Eco-centric interventions
df2 = df2 %>%
  mutate(solution_ecocentric   = case_when(df$X4_mpa==1 ~ 1,
                                           df$X4_protection==1 ~ 1,
                                           df$X4_restoration==1 ~ 1,
                                           df$X4_wastem==1 ~ 1,
                                           TRUE ~ 0))

# Socio-centric coercive interventions (laws, patrolling, etc.)
df2 = df2 %>%
  mutate(solution_sociocentric_coercive = case_when(df$X4_law==1 ~ 1,
                                           df$X4_fishban==1 ~ 1,
                                           df$X4_diminishfish==1 ~ 1,
                                           df$X4_enforce==1 ~ 1,
                                           df$X4_guard==1 ~ 1,
                                           df$X4_arrest==1 ~ 1,
                                           TRUE ~ 0))

# Socio-centric non-coercive interventions (alternative livelihood, external help, inclusion)
df2 = df2 %>%
  mutate(solution_sociocentric_noncoercive = case_when(df$X4_stop==1 ~ 1,
                                                         df$X4_tourism==1 ~ 1,
                                                         df$X4_livelihood==1 ~ 1,
                                                         df$X4_education==1 ~ 1,
                                                         df$X4_capacity==1 ~ 1,
                                                         df$X4_funding==1 ~ 1,
                                                         df$X4_include==1 ~ 1,
                                                         df$X4_help==1 ~ 1,
                                                         df$X4_officials==1 ~ 1,
                                                         df$X4_capacity==1 ~ 1,
                                                         df$X4_aquaculture==1 ~ 1,
                                                         df$X4_bfarmc==1 ~ 1,
                                                         TRUE ~ 0))

# No solutions
df2 = df2 %>%
  mutate(solution_no = case_when(df$X4_no==1 ~ 1,
                                 TRUE ~ 0))

# PERCEPTION OF MPAs ----

# MPAs made for ecological sustainability (protection of fish and corals, restoration of stocks, fishery)
df2 = df2 %>%
  mutate(mpas_ecological_sustainability = case_when(df$X5a_protect==1 ~ 1,
                                                         df$X5a_improve==1 ~ 1,
                                                         df$X5a_restore==1 ~ 1,
                                                         df$X5a_corals==1 ~ 1,
                                                         df$X5a_fish==1 ~ 1,
                                                         df$X5a_shells==1 ~ 1,
                                                         df$X5a_connectivity==1 ~ 1,
                                                         df$X5a_nursery==1 ~ 1,
                                                         df$X5a_natres==1 ~ 1,
                                                         df$X5a_sea==1 ~ 1,
                                                         df$X5a_sustainability==1 ~ 1,
                                                         df$X5a_biodiversity==1 ~ 1,
                                                         df$X5b_ecosystems==1 ~ 1,
                                                         df$X5b_connectivity==1 ~ 1,
                                                         TRUE ~ 0))


# MPAs serving fishery objectives
df2 = df2 %>%
  mutate(mpas_fishery = case_when(df$X5a_spillover==1 ~ 1,
                                                    df$X5a_fishery==1 ~ 1,
                                                    df$X5a_food==1 ~ 1,
                                                    df$X5b_fishermen==1 ~ 1,
                                                    df$X5c_yield==1 ~ 1,
                                                    TRUE ~ 0))

# MPAs for local people
df2 = df2 %>%
  mutate(mpas_for_locals = case_when(df$X5b_locals==1 ~ 1,
                                     df$X5b_fishermen==1 ~ 1,
                                     TRUE ~ 0))

# MPAs as coercive instruments
df2 = df2 %>%
  mutate(mpas_coercive = case_when(df$X5a_guard==1 ~ 1,
                                     df$X5a_illegals==1 ~ 1,
                                     df$X5a_strangers==1 ~ 1,
                                     df$X5c_guard==1 ~ 1,
                                     df$X5c_nooutsiders==1 ~ 1,
                                     df$X5c_law==1 ~ 1,
                                     TRUE ~ 0))

## KNOWLEDGE OF NGO -----
df2 = df2 %>%
  mutate(ngo_knowledge = case_when(df$X6_sulu==1 ~ 1,
                                   TRUE ~ 0))

### EXPORTING CSV FILE
write.csv(df2, "output/general_matrix_synthesis.csv", row.names = FALSE)

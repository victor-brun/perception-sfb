### Victor BRUN
### Started 07-03-2021
### Gathering individual perceptions into larger themes on perception SFB study

rm(list=ls()) ; library(tidyr) ; library(dplyr)

df <- read.csv("data/df_quanti.csv", sep=",")

df <- df[,1:which(colnames(df)=="X6_sulubaai")] # Removing perceptions on the future and specific questions on NGO

## ADDING VARIABLES ----

# a) Adding a "Fisherman" variable for people that declared it as a main or secondary occupation
df = df %>%
  mutate(Fisher = case_when(Job_1 == "Fisherman" ~ "Yes",
                               Job_2 == "Fisherman" ~ "Yes", 
                               TRUE ~ "No"))

# b) Adding a "Group" variable for which 1 == villages where Sulubaai has been active 
# and 2 == villages for which Sulubaai has not
df = df %>%
  mutate(Involved_project = case_when(df$Residence == "Batas" ~ "No",
                                      df$Residence == "Mabini" ~ "No",
                                      df$Residence == "Silangga" ~ "No",
                                      df$Residence == "Depla" ~ "Yes",
                                      df$Residence == "Sandoval" ~ "Yes",
                                      TRUE ~ "Decision-makers and scientists"))

# c) Adding another variable to distinguish respondents that knew Suluubaai and respondents that did not

df = df %>%
  mutate(Knowledge_NGO = case_when(df$X6_sulubaai == "1" ~ "Yes",
                                      TRUE ~ "No"))
# d) Adding a variable to distinguish respondents that know what MPAs are
df = df %>%
  mutate(Knowledge_MPA = case_when(df$X5_mpa == "1" ~ "Yes",
                                   TRUE ~ "No"))

# e) Adding a variable to distinguish decision-makers and scientists
df = df %>%
  mutate(Decision_maker_scientist = case_when(df$Job_1 == "Bgy Captain" ~ "Yes",
                                    df$Residence == "Taytay" ~ "Yes",
                                    df$Residence == "Puerto Princesa" ~ "Yes",
                                    TRUE ~ "No"))

# Creating df2 that will serve as the dataframe to summarize themes
df2 = df %>%
  select(ID, Session, Gender, Residence, Age, Job_1, Job_2, Fisher, Involved_project,
         Knowledge_NGO, Knowledge_MPA, Decision_maker_scientist)

### ADDING VARIABLES BY GATHERING DIFFERENT INDIVIDUAL PERCEPTIONS

## 1) ENVIRONMENTAL STRESSORS  ----

# Destructive and illegal fishing practices
df2 = df2 %>%
  mutate(issue_destructive_practices = case_when(df$X1_illegal==1 ~ 1,
                                           df$X1_dynamite==1 ~ 1,
                                           df$X1_cyanide==1 ~ 1,
                                           df$X1_compressor==1 ~ 1,
                                           df$X1_comfishing==1 ~ 1,
                                           df$X3_illegal==1 ~ 1,
                                           df$X3_otherbarangay==1 ~ 1,
                                           df$X3_dynamite==1 ~ 1,
                                           df$X3_cyanide==1 ~ 1,
                                           df$X3_compressor==1 ~ 1,
                                           df$X3_trawl==1 ~ 1,
                                           df$X3_smallnet==1 ~ 1,
                                           TRUE ~ 0))

# Noticed a depletion of natural resources
df2 = df2 %>%
  mutate(issue_depletion_resources = case_when(df$X1_fewfish==1 ~ 1,
                                               df$X1_natres==1 ~ 1,
                                               df$X1_fauna==1 ~ 1,
                                               df$X3_overfishing==1 ~ 1,
                                               df$X3_smallfish==1 ~ 1,
                                               df$X3_fishyield==1 ~ 1,
                                               df$X3_farfish==1 ~ 1,
                                               df$X3_brokencorals==1 ~ 1,
                                               TRUE ~ 0))

# Deforestation
df2 <- df2 %>%
  mutate(issue_deforestation = case_when(df$X1_logging==1 ~ 1,
                                         df$X1_kaingin==1 ~ 1,
                                         df$X1_mangroves==1 ~ 1,
                                         df$X1_forest==1 ~ 1,
                                         df$X3_mangroves==1 ~ 1,
                                         TRUE ~ 0))

# Pollution
df2 = df2 %>%
  mutate(issue_pollution = case_when(
    df$X1_plastic==1 ~ 1,
    df$X1_wastew==1 ~ 1,
    df$X1_trash==1 ~ 1,
    df$X1_pesticides==1 ~ 1,
    df$X1_fertilizers==1 ~ 1,
    df$X3_pollution==1 ~ 1,
    TRUE ~ 0))

# Climate change & disasters
df2 = df2 %>%
  mutate(issue_climate_disasters = case_when(df$X1_climatechange==1 ~ 1,
                                             df$X1_disasters==1 ~ 1,
                                             df$X3_weather==1 ~ 1,
                                          TRUE ~ 0))

# Other landbased stressors (mining, tourism, infrastructures, pearl farms)
df2 = df2 %>%
  mutate(issue_landbased_stressors = case_when(df$X1_mining==1 ~ 1,
                                           df$X1_erosion==1 ~ 1,
                                           df$X1_infrastructure==1 ~ 1,
                                           df$X1_tourism==1 ~ 1,
                                           df$X3_pearlfarms==1 ~ 1,
                                           TRUE ~ 0))

# Agriculture and water
df2 = df2 %>%
  mutate(issue_agriculture_water = case_when(df$X1_water==1 ~ 1,
                                             df$X1_pests==1 ~ 1,
                                             df$X1_plantations==1 ~ 1,
                                               TRUE ~ 0))

# Root social causes (poverty, demography, lack of collaboration, thieves, etc.)
df2 = df2 %>%
  mutate(issue_social_causes = case_when(df$X1_demography==1 ~ 1,
                                           df$X1_dependance==1 ~ 1,
                                           df$X1_poverty==1 ~ 1,
                                           df$X1_education==1 ~ 1,
                                           df$X1_policy==1 ~ 1,
                                           df$X3_power==1 ~ 1,
                                           df$X3_collaboration==1 ~ 1,
                                           df$X3_lackgovernment==1 ~ 1,
                                           df$X3_thefts==1 ~ 1,
                                           df$X3_inequality==1 ~ 1,
                                           TRUE ~ 0))

# Marine conservation and fisheries management
df2 = df2 %>%
  mutate(issue_conservation = case_when(df$X3_closedseasons==1 ~ 1,
                                        df$X3_mpa==1 ~ 1,
                                              TRUE ~ 0))

# No issue identified
df2 = df2 %>%
  mutate(issue_no = case_when(df$X1_no==1 ~ 1,
                              df$X3_no==1 ~ 1,
                              TRUE ~ 0))

# NOTE: individual perceptions not classified in overarching themes => 
# 3_illegaldec (decrease in illegalfishing), 3_noluck (badluck)

## 2) ENVIRONMENTAL WELL-BEING ----

# Livelihood, natural resources and money derived from them
df2 = df2 %>%
  mutate(value_livelihood = case_when(df$X2_livelihood==1 ~ 1,
                                         df$X2_natres==1 ~ 1,
                                         df$X2_fishers==1 ~ 1,
                                         df$X2_money==1 ~ 1,
                                         df$X2_seaweeds==1 ~ 1,
                                         TRUE ~ 0))

# Food, proteins and benefits for health
df2 = df2 %>%
  mutate(value_food = case_when(df$X2_food==1 ~ 1,
                                         df$X2_protein==1 ~ 1,
                                         df$X2_health==1 ~ 1,
                                         TRUE ~ 0))

# Intrinsic value of nature
df2 = df2 %>%
  mutate(value_intrinsic = case_when(df$X2_biodiversity==1 ~ 1,
                                    df$X2_species==1 ~ 1,
                                    df$X2_nature==1 ~ 1,
                                    TRUE ~ 0))

# Spiritual, cultural reasons
df2 = df2 %>%
  mutate(value_cultural = case_when(df$X2_nextgen==1 ~ 1,
                                     df$X2_beautiful==1 ~ 1,
                                     df$X2_godgift==1 ~ 1,
                                     TRUE ~ 0))

# Other ecosystem services
df2 = df2 %>%
  mutate(value_services = case_when(df$X2_services==1 ~ 1,
                                    df$X2_carbon==1 ~ 1,
                                    df$X2_coastalprot==1 ~ 1,
                                    df$X2_tourism==1 ~ 1,
                                    TRUE ~ 0))

# No value recognized
df2 = df2 %>%
  mutate(value_no_value= case_when(df$X2_no==1 ~ 1,
                                   TRUE ~ 0))


## 3) POSSIBLE OPTIONS -----

# Eco-centric interventions
df2 = df2 %>%
  mutate(solution_ecocentric   = case_when(df$X4_mpa==1 ~ 1,
                                           df$X4_protection==1 ~ 1,
                                           df$X4_restoration==1 ~ 1,
                                           df$X4_fishclosure==1 ~ 1,
                                           df$X4_diminishfish==1 ~ 1,
                                           TRUE ~ 0))

# Legal instruments & enforcement
df2 = df2 %>%
  mutate(solution_sociocentric_coercive = case_when(df$X4_law==1 ~ 1,
                                           df$X4_enforce==1 ~ 1,
                                           df$X4_guard==1 ~ 1,
                                           df$X4_arrest==1 ~ 1,
                                           df$X4_officials==1 ~ 1,
                                           df$X4_bfarmc==1 ~ 1,
                                           df$X4_wastem==1 ~ 1,
                                           TRUE ~ 0))

# Capacity building, empowerment and alternative livelihoods
df2 = df2 %>%
  mutate(solution_capacity_alternative = case_when(df$X4_tourism==1 ~ 1,
                                                         df$X4_livelihood==1 ~ 1,
                                                         df$X4_education==1 ~ 1,
                                                         df$X4_capacity==1 ~ 1,
                                                         df$X4_funding==1 ~ 1,
                                                         df$X4_include==1 ~ 1,
                                                         df$X4_externalhelp==1 ~ 1,
                                                         df$X4_aquaculture==1 ~ 1,
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
                                                    df$X5c_otherthreats==1 ~ 1,
                                                    TRUE ~ 0))


# MPAs serve fishery and food objectives
df2 = df2 %>%
  mutate(mpas_fishery = case_when(df$X5a_spillover==1 ~ 1,
                                  df$X5a_fishery==1 ~ 1,
                                  df$X5a_food==1 ~ 1,
                                  df$X5b_fishers==1 ~ 1,
                                  df$X5c_yield==1 ~ 1,
                                  TRUE ~ 0))

# MPAs benefit local resource users
df2 = df2 %>%
  mutate(mpas_for_locals = case_when(df$X5b_locals==1 ~ 1,
                                     df$X5a_people==1 ~ 1,
                                     df$X5b_poors==1 ~ 1,
                                     df$X5a_economy==1 ~ 1,
                                     df$X5b_nextgen==1 ~ 1,
                                     TRUE ~ 0))

# MPAs benefit external actors (tourists, resorts, gov agencies)
df2 = df2 %>%
  mutate(mpas_for_external = case_when(df$X5b_tourists==1 ~ 1,
                                     df$X5a_tourism==1 ~ 1,
                                     df$X5b_resorts==1 ~ 1,
                                     df$X5b_mfarmc==1 ~ 1,
                                     df$X5b_government==1 ~ 1,
                                     TRUE ~ 0))

# MPAs as coercive instruments
df2 = df2 %>%
  mutate(mpas_coercive = case_when(df$X5a_guard==1 ~ 1,
                                   df$X5a_illegals==1 ~ 1,
                                   df$X5a_strangers==1 ~ 1,
                                   df$X5c_nooutsiders==1 ~ 1,
                                   df$X5c_law==1 ~ 1,
                                   TRUE ~ 0))


### EXPORTING CSV FILE
write.csv(df, "Data/general_matrix_allperceptions.csv", row.names = FALSE)
write.csv(df2, "Data/general_matrix_synthesis.csv", row.names = FALSE)

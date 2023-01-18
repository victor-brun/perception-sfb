### Victor BRUN
### Created 08-03-2021
### Analysis of synthesized themes from "perception MPAs" data

## Packages and data ----

rm(list=ls()) ; library(tidyverse) ; library(ggplot2) ; library(questionr)

df = read.csv("Data/general_matrix_synthesis.csv")


### CREATING DATAFRAMES ----

# Creating "df2" summing all the answers per group
df2 = df %>% 
  group_by(Knowledge_NGO) %>%
  select(10:31) %>%
  summarise_all(sum)

# Creating "df3" expressing "df2" in percentages of the whole group
group0 = df2 %>%
  filter(ngo_knowledge == "No previous knowledge of the NGO") %>%
  `/`(34)
group1 = df2 %>%
  filter(ngo_knowledge == "Previous knowledge of the NGO") %>%
  `/`(24)
df3 = rbind(group0, group1)
df3[1,1]=0
df3[2,1]=1
rm(group0, group1)

# Transposing df3
df3 = t(df3) %>% data.frame
colnames(df3) = c("No previous knowledge of the NGO", "Previous knowledge of the NGO")
df3 = df3[-1,]

## STATISTICAL ANALYSES ----

# Relevel factors
df3$ID = rownames(df3)
df3$ID <- factor(df3$ID, levels = c("value_livelihood",
                                    "value_food",
                                    "value_tradition",
                                    "value_services",
                                    "future_sustainability",
                                    "future_unsustainability",
                                    "future_alternatives",
                                    "issue_destructive_practices",
                                    "issue_depletion_resources",
                                    "issue_terrestrial",
                                    "issue_unmanagable_causes",
                                    "issue_other_pressures",
                                    "issue_social_causes",
                                    "issue_no",
                                    "solution_sociocentric_coercive",
                                    "solution_sociocentric_noncoercive",
                                    "solution_ecocentric",
                                    "solution_no",
                                    "mpas_ecological_sustainability",
                                    "mpas_for_locals",
                                    "mpas_coercive",
                                    "mpas_fishery"))
df3$ID <- fct_rev(df3$ID)

# Recode factor
df3$ID <- fct_recode(df3$ID,
                     "Livelihood" = "value_livelihood",
                     "Food and nutrition" = "value_food",
                     "Tradition" = "value_tradition",
                     "Other services" = "value_services",
                     "Paths to sustainability" = "future_sustainability",
                     "Paths to unsustainability" = "future_unsustainability",
                     "Alternative livelihoods" = "future_alternatives",
                     "Destructive fishing practices" = "issue_destructive_practices",
                     "Depletion of marine resources" = "issue_depletion_resources",
                     "Land-based pressures" = "issue_terrestrial",
                     "Unmanageable issues" = "issue_unmanagable_causes",
                     "Other pressures" = "issue_other_pressures",
                     "Social issues" = "issue_social_causes",
                     "No issue identified" = "issue_no",
                     "Coercive social interventions" = "solution_sociocentric_coercive",
                     "Non-coercive social interventions" = "solution_sociocentric_noncoercive",
                     "Ecosystem-based interventions" = "solution_ecocentric",
                     "No option identified" = "solution_no",
                     "MPAs for ecological sustainability" = "mpas_ecological_sustainability",
                     "MPAs for local people" = "mpas_for_locals",
                     "MPAs as coercive instruments" = "mpas_coercive",
                     "MPAs for fishery" = "mpas_fishery"
)

# Representing all proportions using a barplot
df4 = pivot_longer(df3, c(`No Knowledge of NGO`, `Knowledge of NGO`))
barplot = ggplot(df4) +
  aes(x = ID, y = value) +
  geom_col(aes(fill = name), position = "dodge") +
  labs(x = "", y = "% of respondents that identified this theme", fill = "Group of respondents") +
  scale_fill_manual(values = c("black", "light grey")) +
  theme_minimal()
barplot + coord_flip()




##### OTHER ANALYSES ------

# Putting back decision-makers and scientists 

df = read.csv("output/general_matrix_synthesis.csv")

# Number of respondents in each category (knowledge of NGO and no knowledge)
df %>%
  filter(ngo_knowledge == 0) %>%
  NROW() #38
df %>%
  filter(ngo_knowledge == 1) %>%
  NROW() #28

# Recoding

df$ngo_knowledge <- df$ngo_knowledge %>%
  as.character() %>%
  fct_recode(
    "No previous knowledge of the NGO" = "0",
    "Previous knowledge of the NGO" = "1"
  )

# Creating "df2" summing all the answers per group
df2 = df %>% 
  group_by(ngo_knowledge) %>%
  select(10:31) %>%
  summarise_all(sum)

# Getting N of respondents for each theme
df5 = as.data.frame(t(df2))
df5 = df5 %>%
  mutate(total = V1 + V2)


##### Statistical analyses

library(vegan)
library(nlme)

# Computing total biomass per species per transect

df_div <- df %>%
  select(10:32)

# Shannon diversity index for each transect

Div <- numeric(nrow(df_div))
for (i in 1:nrow(df_div)) {
  Div[i] <- diversity(df_div[i,1:22], index = "simpson", MARGIN = 1, base = exp(1))
}

df_div <- df_div %>%
  mutate(Diversity = Div)

rm(Div, i)

# Plot diversity
df_div$ngo_knowledge <-  as.factor(df_div$ngo_knowledge)

df_div %>%
  ggplot(., aes(x = ngo_knowledge, y = Diversity)) +
  geom_boxplot() +
  labs(title = "Diversity per group", x = "") +
  theme_minimal()

# ANOVA (p = 0.0471) => Higher diversity of responses among respondents that are aware of the NGO projects

res.aov <- aov(Diversity ~ ngo_knowledge, data = df_div)
summary(res.aov)

# ANOVA with the number of answers per respondent
df_anova <- df %>%
  select(-Session) %>%
  mutate(total_answers = rowSums(across(where(is.numeric))))

# PERMANOVA (p = 0.002)

dist <- vegdist(df_div[1:21],  method = "bray")

permRes <- adonis(dist ~ ngo_knowledge, data =  df_div, permutations=999, method="gower")

permRes

# Dispersion (permutest => p = 0.002)

dispersion <- betadisper(dist, group=df_div$ngo_knowledge)
TukeyHSD(dispersion)
permutest(dispersion)
plot(dispersion, hull=TRUE, ellipse=FALSE) 

## NMDS -------

mds=metaMDS(df_div[1:21], k = 3, trymax = 100)
stressplot(mds)

# Preparing data for ggplot

data.scores <- as.data.frame(scores(mds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- df_div$ngo_knowledge #  add the grp variable created earlier
head(data.scores)  #look at the data
sites.scores <- as.data.frame(scores(mds, "sites"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
sites.scores$sites <- rownames(sites.scores)  # create a column of species, from the rownames of species.scores
grp.a <- data.scores[data.scores$grp == "1", ][chull(data.scores[data.scores$grp == 
                                                                       "No previous knowledge of the NGO", c("NMDS1", "NMDS2")]), ]  # hull values for grp A
grp.b <- data.scores[data.scores$grp == "0", ][chull(data.scores[data.scores$grp == 
                                                                         "Previous knowledge of the NGO", c("NMDS1", "NMDS2")]), ]  # hull values for grp B
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

# Plotting NMDS

ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.4) + # add the convex hulls
  geom_text(data=sites.scores,aes(x=NMDS1,y=NMDS2,label=sites),alpha=0.8) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp), alpha=.2, size=5) + # add the point markers
  scale_colour_manual(values=c("1" = "grey", "2" = "blue")) +
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

# Other NMDS including the perceptions

ordiplot(mds)
ordihull(mds,groups=df_div$ngo_knowledge, col = c("red", "blue"))
orditorp(mds,display="species",col="black", air=1)

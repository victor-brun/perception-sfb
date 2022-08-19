## Multiple correspondence analysis
## Victor Brun
## 29/07/2020

rm(list=ls())

library(tidyr) ; library(tidyverse) ; library(FactoMineR) ; library(factoextra) ; 
library(fishualize) ; library(ggrepel)

# MCA WITH ORIGINAL DATASET -------

df <- read.csv("data/general_quanti.csv", sep=";") %>%
  mutate_all(as.character)

#
res.mca <- MCA(df[,11:240],  graph = FALSE)

# plot
fviz_mca_biplot(res.mca, 
                repel = TRUE, 
                ggtheme = theme_minimal())

meta <- df[,1:9]

## add other info
ind <- res.mca$ind$coord %>% 
  as.data.frame() %>%
  set_names(c("dim1", "dim2", "dim3", "dim4", "dim5")) %>%
  rownames_to_column(var = "Session") %>%
  left_join(meta)

questions_answers <- res.mca$var$coord %>% 
  as.data.frame() %>%
  set_names(c("dim1", "dim2", "dim3", "dim4", "dim5")) %>%
  rownames_to_column(var = "q_a") 

summary(res.mca)

## plot with color per "groupe" for example
ggplot(ind) +
  geom_point(aes(x = dim3, y = dim4, color = Residence), size = 3) +
  #geom_text_repel(aes(x = dim1, y = dim2, label = q_a), data = questions_answers) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Dim 1 (8.9%)", y = "Dim 2 (5.8%)") + 
  theme_minimal() 


# MCA WITH DATA FROM CLEANED DATASET ------

df2 <- read.csv("output/general_matrix.csv", sep=",") %>%
  mutate_all(as.character)

#
res.mca2 <- MCA(df2[,9:182],  graph = FALSE)

# plot
fviz_mca_biplot(res.mca2, 
                repel = TRUE, 
                ggtheme = theme_minimal())

meta2 <- df2[,1:9]

## add other info
ind2 <- res.mca2$ind$coord %>% 
  as.data.frame() %>%
  set_names(c("dim1", "dim2", "dim3", "dim4", "dim5")) %>%
  rownames_to_column(var = "Session") %>%
  left_join(meta2)

questions_answers2 <- res.mca2$var$coord %>% 
  as.data.frame() %>%
  set_names(c("dim1", "dim2", "dim3", "dim4", "dim5")) %>%
  rownames_to_column(var = "q_a") 

summary(res.mca2)

## plot with color per "group" for example
ggplot(ind2) +
  geom_point(aes(x = dim3, y = dim4, color = Residence), size = 3, alpha = 0.6) +
  #geom_text_repel(aes(x = dim1, y = dim2, label = q_a), data = questions_answers) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Dim 1 (8.7%)", y = "Dim 2 (6.5%)") + 
  theme_minimal() 

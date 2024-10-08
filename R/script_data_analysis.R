library(tidyverse); library(easystats); library(lme4); library(ggridges)
library(ggbeeswarm)

theme_set(theme_bw(14))

# 0. Import and check data ----
## 0.1 Data import ----
df_B = read.csv(file = "data/data-raw/Summary_Trajectories.csv", sep = ";") # Behavioral tracking data
df_M = read.csv(file = "data/data-raw/Summary_Data_morpho.csv", sep = ";") # Morphological data

## 0.2 Unique ID checks ----
length(unique(df_B$Video))
length(unique(df_B$Groupe))
length(unique(df_B$Individual)) # Problem with nb of individuals should be 16

length(unique(df_M$Video))
length(unique(df_M$Groupe))
length(unique(df_M$Individual)) # Problem with nb of individuals should be 16

## 0.3 Modify & save data into data-clean ----
## Change Mili3 to Mili4 in all "diplo_iso_opilio.avi" rows
## Change Isolpod2 to Isopod2 in all rows

df_B = df_B %>% 
  mutate(Individual = case_when(Video == "diplo_iso_opilio.avi" & 
                             Individual == "Mili3" ~ "Mili4", 
                             Individual == "Isolpod2" ~ "Isopod2",
                             .default = Individual))

df_M = df_M %>% 
  mutate(Individual = case_when(Video == "diplo_iso_opilio.avi" & 
                                  Individual == "Mili3" ~ "Mili4", 
                                Individual == "Isolpod2" ~ "Isopod2",
                                .default = Individual))
# Re-check
length(unique(df_B$Video))
length(unique(df_B$Groupe))
length(unique(df_B$Individual)) # Problem with nb of individuals should be 16

length(unique(df_M$Video))
length(unique(df_M$Groupe))
length(unique(df_M$Individual)) # Problem with nb of individuals should be 16

# data export into data-clean folder
write.csv(df_B, file = "data/data-clean/df_B.csv", row.names = FALSE)
write.csv(df_M, file = "data/data-clean/df_M.csv", row.names = FALSE)

# 1. Data viz (first pass) ---- 
df_B = read.csv(file = "data/data-clean/df_B.csv") # Behavioral tracking data
df_M = read.csv(file = "data/data-clean/df_M.csv") # Morphological data

## 1.1 Behavioral traits visualization ----
# Average speed, Distance traveled, % Time moving, % Arena explored
par(mfrow = c(1,2))
hist(df_B$Average_Speed); hist(log(df_B$Average_Speed))
hist(df_B$Traveled_Dist); hist(log(df_B$Traveled_Dist))
hist(df_B$Prop_time_moving); hist(sqrt(df_B$Prop_time_moving))
hist(df_B$Exploration_relative_value); hist(sqrt(df_B$Exploration_relative_value))

pairs(df_B[,])
## 1.2 Morphological traits visualization ----
# Area, Perimeter, Length, Width
par(mfrow = c(1,2))
hist(df_M$Area); hist(log(df_M$Area))
hist(df_M$Perimeter); hist(log(df_M$Perimeter))
hist(df_M$Length); hist(log(df_M$Length))
hist(df_M$Width); hist(log(df_M$Width))





# 2. Data viz by Observer ----
# TODO : Improvement: sort by highest average observer speed
df_B %>% 
  ggplot(aes(x = Average_Speed, y = Groupe)) +
  geom_point(size = 3, alpha = .5) +
  geom_density_ridges(alpha = .3) +
  labs(x = "Average speed", y = "Observer ID")
  
# 3. Data viz by Animal ----
# TODO : Improvement: sort by highest average individual speed
df_B %>% 
  ggplot(aes(x = Average_Speed, y = Individual)) +
  geom_beeswarm(size = 3, alpha = .5) +
  geom_density_ridges(alpha = .3) +
  labs(x = "Average speed", y = "Animal ID")

# 4. Visualizing Observer agreement ----
df_B %>% 
  ggplot(aes(x = Average_Speed, y = Video)) +
  geom_point(size = 3, alpha = .5) +
  geom_density_ridges(alpha = .3) +
  facet_wrap(~Groupe)

fig.repeat.Obs = df_B %>% 
  mutate(Obs.ID = rep(1:4, each = 16)) %>% 
  ggplot(aes(x = Obs.ID, y = Average_Speed, group = Individual)) +
  geom_beeswarm(size = 3, alpha = .15) +
  geom_line() + 
  ylim(0,20) +
  labs(x = "Observer ID", 
       y = "Average speed", 
       title = "Observer agreement", 
       subtitle = "Each line represents a different animal measured \n by 4 different observers") 

ggsave(filename = "outputs/figs/fig.repeat.Obs.jpeg", fig.repeat.Obs)

# 5. Observer repeatability calculation ----
lmm.1 = lmer(Average_Speed ~ 1 + (1|Groupe) + 
               (1|Video) + 
               (1|Individual), 
             df_B)
summary(lmm.1)
check_model(lmm.1)
r2_nakagawa(lmm.1, ci = .95) # Proportion of variation explained by all random effects (Conditional R2)
r2_nakagawa(lmm.1, by_group = T) # Proportion of variation explained by all terms
get_variance(lmm.1)

saveRDS(lmm.1, "outputs/mods/lmm.1.rds")

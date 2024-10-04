library(tidyverse); library(easystats); library(lme4)


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
write.csv(df_B, file = "data/data-clean/df_B.csv")
write.csv(df_M, file = "data/data-clean/df_M.csv")

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

## 1.2 Morphological traits visualization ----
# Area, Perimeter, Length, Width
par(mfrow = c(1,2))
hist(df_M$Area); hist(log(df_M$Area))
hist(df_M$Perimeter); hist(log(df_M$Perimeter))
hist(df_M$Length); hist(log(df_M$Length))
hist(df_M$Width); hist(log(df_M$Width))




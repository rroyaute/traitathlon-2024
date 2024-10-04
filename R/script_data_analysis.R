library(tidyverse); library(easystats); library(lme4)


# 0. Import and check data ----
df_B = read.csv(file = "data/Summary_Trajectories.csv", sep = ";") # Behavioral tracking data
df_M = read.csv(file = "data/Summary_Data_morpho.csv", sep = ";") # Morphological data

# Behavioral traits visualization
# Average speed, Distance traveled, % Time moving, % Arena explored
par(mfrow = c(1,2))
hist(df_B$Average_Speed); hist(log(df_B$Average_Speed))
hist(df_B$Traveled_Dist); hist(log(df_B$Traveled_Dist))
hist(df_B$Prop_time_moving); hist(sqrt(df_B$Prop_time_moving))
hist(df_B$Exploration_relative_value); hist(sqrt(df_B$Exploration_relative_value))

# Morphological traits visualization
# Area, Perimeter, Length, Width
par(mfrow = c(1,2))
hist(df_M$Area); hist(log(df_M$Area))
hist(df_M$Perimeter); hist(log(df_M$Perimeter))
hist(df_M$Length); hist(log(df_M$Length))
hist(df_M$Width); hist(log(df_M$Width))



library(tidyverse)
library(ggpubr)
library(rstatix)

# Read the CSV files
sopc5 <- read.csv("Data/20220111_RA_SOPC5.csv")
sopc3 <- read.csv("Data/20220513_RA_SOPC3.csv")
dppc5 <- read.csv("Data/20220111_RA_DPPC5.csv")
dppc3 <- read.csv("Data/20220722_RA_DPPC3.csv")

# Calculate the reduced area for each fluidity and concentration
fluid_5 = sopc5$Area[c(FALSE, TRUE)] / sopc5$Area[c(TRUE, FALSE)]
fluid_3 = sopc3$Area[c(FALSE, TRUE)] / sopc3$Area[c(TRUE, FALSE)]
fixed_5 = dppc5$Area[c(FALSE, TRUE)] / dppc5$Area[c(TRUE, FALSE)]
fixed_3 = dppc3$Area[c(FALSE, TRUE)] / dppc3$Area[c(TRUE, FALSE)]

# Define the data frame
df <- data.frame(
  reduced_area = c(fluid_5, fluid_3, fixed_5, fixed_3),
  slb = rep(c("Fluid 5", "Fluid 3", "Fixed 5", "Fixed 3"), 
            times = c(length(fluid_5), length(fluid_3), length(fixed_5), length(fixed_3))),
  Mobility = rep(c("Fluid", "Fluid", "Fixed", "Fixed"), 
            times = c(length(fluid_5), length(fluid_3), length(fixed_5), length(fixed_3))),
  Concentration = rep(c(5, 3, 5, 3), 
            times = c(length(fluid_5), length(fluid_3), length(fixed_5), length(fixed_3)))
)

# Statistical test
stat.test <- df %>%
  wilcox_test(reduced_area ~ slb) %>%
  add_significance() %>% 
  subset(substr(group1, 1, 5) == substr(group2, 1, 5) | substr(group1, 7, 7) == substr(group2, 7, 7))

violins <- ggviolin(df, x="slb", y="reduced_area", fill="Concentration", color="Mobility", add = "jitter", alpha = 0.07) +
  scale_x_discrete(limits = c("Fluid 5", "Fluid 3", "Fixed 5", "Fixed 3")) +
  scale_color_manual(values = c("#cf44cd","#4542cf")) +
  theme_bw() + 
  labs(x="SLB", y="Reduced Area")

# stat.test <- stat.test %>% add_xy_position(x = "slb")
    
violins +
  stat_pvalue_manual(stat.test, label = "P = {p}", y.position = c(1.65, 1.8, 2, 1.5)) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))

ggsave("violin_comparison.png", height = 5, width = 6.5)



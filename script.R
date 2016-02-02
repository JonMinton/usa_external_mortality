# USA Fall in violence paper

# The aim of this session is to produce various figures which show how mortality rates due to assault/homicide 
# have varied with age, sex, and ethnicity over time in the USA. THe aim of doing this is to produce some a series 
# of figures which show the trends for as long a period as possible. In particular it is important to see if the trends 
#  follow those observed more generally about convergence between Blacks and Non-Blacks in age specific mortality rates 




# Load packages -----------------------------------------------------------

rm(list=ls())

require(readr)
require(plyr)
require(tidyr)
require(dplyr)
require(stringr)
require(car)

require(ggplot2)
require(lattice)
require(latticeExtra)
require(RColorBrewer)
require(grid)




# Analyses of long-term trends using ICD8-10 ------------------------------


icd_8cat <- read_csv("data/tidied/icd_8_to_10_8fold.csv", col_types = "ciccccdd")

icd_8cat <- icd_8cat %>% mutate(
  age = factor(
    age, 
    levels = c(
      "< 1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-34", "35-44", 
      "45-54", "55-64", "64-74", "75-84"
               ),
    ordered = T
    )
  )

# dd at end works but ii doesn't, implying some non-integer values for death/population counts


# To dos - 
# 1) All cause mortality, overall and by race + sex
# 2) external mortality, overall and by race + sex
# 3) black/white ratio in all cause mortality 
# 4) black/white ratio in external mortality 
# 5) vehicle mortality trends, overall and by race + sex
# 6) assault mortality trends, overall and by race + sex
# 


# 1) All cause mortality, overall and by race + sex

icd_8cat %>% 
  filter(cause == "all_cause") %>% 
  select(-cause) %>% 
  group_by(icd_class, year, sex, race) %>% 
  summarise(
    death_count = sum(death_count), 
    population_count = sum(population_count)
            ) %>% 
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  ggplot(.) +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race)) + 
  facet_wrap(~ sex) + 
  labs(x = "Year", y = "All cause death rate per 100 000", title = "All Cause") + 
  theme_minimal()

ggsave(filename = "figures/all_cause_mortality.png", dpi = 300, width = 15, height = 10, units = "cm")



# 2) external mortality, overall and by race + sex

icd_8cat %>% 
  filter(cause == "all_external") %>% 
  select(-cause) %>% 
  group_by(icd_class, year, sex, race) %>% 
  summarise(
    death_count = sum(death_count), 
    population_count = sum(population_count)
  ) %>% 
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  ggplot(.) +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race)) + 
  facet_wrap(~ sex) + 
  labs(x = "Year", y = "External death rate per 100 000", title = "External") + 
  theme_minimal()

ggsave(filename = "figures/all_external_mortality.png", dpi = 300, width = 15, height = 10, units = "cm")

# 3) black/white ratio in all cause mortality 

icd_8cat %>% 
  filter(cause == "all_cause") %>% 
  filter(!is.na(age)) %>% 
  filter(race %in% c("white", "black")) %>%
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  select(year, age, sex, race, death_rate) %>% 
  spread(key = race, value = death_rate) %>% 
  mutate(bw_ratio = black / white) %>% 
  select(-black, -white) %>% 
  ggplot(.) + 
  geom_line(aes(x = year, y = bw_ratio, group = sex, colour = sex)) + 
  facet_wrap(~ age) + 
  labs(
    title = "Ratio of male/white all cause mortality rates at \ndifferent age groups",
    x = "Year", y= "Ratio of black to white mortality"
  )

ggsave("figures/ratio_of_bw_allcause_mort_by_age.png", width = 30, height = 30, units = "cm", dpi = 300)


# 4) black/white ratio in external mortality 

icd_8cat %>% 
  filter(cause == "all_external") %>% 
  filter(!is.na(age)) %>% 
  filter(race %in% c("white", "black")) %>%
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  select(year, age, sex, race, death_rate) %>% 
  spread(key = race, value = death_rate) %>% 
  mutate(bw_ratio = black / white) %>% 
  select(-black, -white) %>% 
  ggplot(.) + 
  geom_line(aes(x = year, y = bw_ratio, group = sex, colour = sex)) + 
  facet_wrap(~ age) + 
  labs(
    title = "Ratio of male/white external mortality rates at \ndifferent age groups",
    x = "Year", y= "Ratio of black to white mortality"
  )

ggsave("figures/ratio_of_bw_all_external_mort_by_age.png", width = 30, height = 30, units = "cm", dpi = 300)


# # 5) vehicle mortality trends, overall and by race + sex



icd_8cat %>% 
  filter(cause ==  "vehicle") %>% 
  select(-cause) %>% 
  group_by(icd_class, year, sex, race) %>% 
  summarise(
    death_count = sum(death_count), 
    population_count = sum(population_count)
  ) %>% 
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  ggplot(.) +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race)) + 
  facet_wrap(~ sex) + 
  labs(x = "Year", y = "RTA death rate per 100 000", title = "Road Traffic Accidents") + 
  theme_minimal()

ggsave(filename = "figures/vehicle_mortality.png", dpi = 300, width = 15, height = 10, units = "cm")

# 6) BW ratio in vehicle mortality

icd_8cat %>% 
  filter(cause == "vehicle") %>% 
  filter(!is.na(age)) %>% 
  filter(race %in% c("white", "black")) %>%
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  select(year, age, sex, race, death_rate) %>% 
  spread(key = race, value = death_rate) %>% 
  mutate(bw_ratio = black / white) %>% 
  select(-black, -white) %>% 
  ggplot(.) + 
  geom_line(aes(x = year, y = bw_ratio, group = sex, colour = sex)) + 
  facet_wrap(~ age) + 
  labs(
    title = "Ratio of male/white mortality due to assault/homicide at \ndifferent age groups",
    x = "Year", y= "Ratio of black to white mortality"
  )  + geom_hline(aes(yintercept = 1)) + 
  scale_y_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20))


ggsave(filename = "figures/bw_ratio_motor_mortality.png", dpi = 300, width = 15, height = 10, units = "cm")


# 6) assault mortality trends, overall and by race + sex
icd_8cat %>% 
  filter(cause ==  "assault") %>% 
  select(-cause) %>% 
  group_by(icd_class, year, sex, race) %>% 
  summarise(
    death_count = sum(death_count), 
    population_count = sum(population_count)
  ) %>% 
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  ggplot(.) +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race)) + 
  facet_wrap(~ sex) + 
  labs(x = "Year", y = "assault/homicide death rate per 100 000", title = "Assault/Homicide") + 
  theme_minimal()

ggsave(filename = "figures/assault_homicide_mortality.png", dpi = 300, width = 15, height = 10, units = "cm")


# 6) BW ratio in assault mortality trends

icd_8cat %>% 
  filter(cause == "assault") %>% 
  filter(!is.na(age)) %>% 
  filter(race %in% c("white", "black")) %>%
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  select(year, age, sex, race, death_rate) %>% 
  spread(key = race, value = death_rate) %>% 
  mutate(bw_ratio = black / white) %>% 
  select(-black, -white) %>% 
  ggplot(.) + 
  geom_line(aes(x = year, y = bw_ratio, group = sex, colour = sex)) + 
  facet_wrap(~ age) + 
  labs(
    title = "Ratio of male/white mortality due to assault/homicide at \ndifferent age groups",
    x = "Year", y= "Ratio of black to white mortality"
  ) + geom_hline(aes(yintercept = 1)) + 
  scale_y_log10(breaks = c(1, 2, 5, 10, 20))

ggsave(filename = "figures/bw_ratio_assault_homicide_mortality.png", dpi = 300, width = 15, height = 10, units = "cm")


# 6) undetermined, by age group, race + sex
icd_8cat %>% 
  filter(cause ==  "intentional_or_undetermined_self_harm") %>% 
  select(-cause) %>% 
  group_by(icd_class, year, sex, race) %>% 
  summarise(
    death_count = sum(death_count), 
    population_count = sum(population_count)
  ) %>% 
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  ggplot(.) +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race)) + 
  facet_wrap(~ sex) + 
  labs(x = "Year", y = "self-harm death rate per 100 000", title = "Self harm (intended or undetermined)") + 
  theme_minimal()

ggsave(filename = "figures/selfharm_mortality.png", dpi = 300, width = 15, height = 10, units = "cm")


# 6) BW ratio in self harm trends

icd_8cat %>% 
  filter(cause == "intentional_or_undetermined_self_harm") %>% 
  filter(!is.na(age)) %>% 
  filter(race %in% c("white", "black")) %>%
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  select(year, age, sex, race, death_rate) %>% 
  spread(key = race, value = death_rate) %>% 
  mutate(bw_ratio = black / white) %>% 
  select(-black, -white) %>% 
  ggplot(.) + 
  geom_line(aes(x = year, y = bw_ratio, group = sex, colour = sex)) + 
  facet_wrap(~ age) + 
  labs(
    title = "Ratio of male/white mortality due to assault/homicide at \ndifferent age groups",
    x = "Year", y= "Ratio of black to white mortality"
  ) + geom_hline(aes(yintercept = 1)) + 
  scale_y_log10()

ggsave(filename = "figures/bw_ratio_assault_homicide_mortality.png", dpi = 300, width = 15, height = 10, units = "cm")




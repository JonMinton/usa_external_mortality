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



# Jon,
# 
# Please run the trends (1968-2014) for homicide, suicide, and vehicle accident death rates 
# by race-sex groups (i.e., black females, black males, white females, and white males), 
# age group (set 3 groups: children/adolescents (ages 10-19), adults (20-54), and older adults (55-84)). 
# Mark the presidential years by party as follows:
#   

age_group_icd <- icd_8cat %>% 
  mutate(age_group = recode(
    age, 
    recodes = "
      c('< 1', '1-4', '5-9') = 'young children';
      c('10-14', '15-19') = 'children and adolescents';
      c('20-24', '25-34', '35-44', '45-54') = 'adults';
      c('55-64', '65-74', '75-84') = 'older adults'
    "                        
                            )) %>% 
  filter(!is.na(age_group)) %>% 
  filter(age_group != "young children") %>% 
  filter(cause %in% c("assault", "intentional_self_harm", "vehicle")) %>% 
  mutate(rep = ifelse(year %in% c(2010:2014, 1994:2001, 1978:1981, 1968:1969), 1, 0)) %>% 
  select(year, age_group, sex, race, cause, rep, death_count, population_count) %>% 
  group_by(year, age_group, sex, race, cause, rep) %>% 
  summarise(
    death_count = sum(death_count), 
    population_count = sum(population_count)        
            ) %>% 
  ungroup() %>% 
  mutate(death_rate = 100000 * death_count / population_count) 

write_csv(age_group_icd, path = "data/tidied/three_cause_agegrouped.csv")

age_group_icd %>% 
  filter(age_group == "adults") %>% 
  ggplot(.) + 
  geom_line(aes(x = year, y = death_rate, group = race, colour = race)) + 
  facet_grid(sex ~ cause) + 
  scale_y_log10() + 
  labs(title = "Adults")
ggsave("figures/three_cause_adults.png", dpi = 150, width = 20, height = 15, units = "cm")


age_group_icd %>% 
  filter(age_group == "children and adolescents") %>% 
  ggplot(.) + 
  geom_line(aes(x = year, y = death_rate, group = race, colour = race)) + 
  facet_grid(sex ~ cause) + 
  scale_y_log10() + 
  labs(title = "Children and adolescents")
ggsave("figures/three_cause_children.png", dpi = 150, width = 20, height = 15, units = "cm")

age_group_icd %>% 
  filter(age_group == "older adults") %>% 
  ggplot(.) + 
  geom_line(aes(x = year, y = death_rate, group = race, colour = race)) + 
  facet_grid(sex ~ cause) + 
  scale_y_log10() + 
  labs(title = "older_adults")
ggsave("figures/three_cause_older.png", dpi = 150, width = 20, height = 15, units = "cm")


# General comments:
# #   
# #   * Data for older adults are noisy and the trends seem 
# uninteresting for our purposes. Let's make them aside for now...
# # 
# # * Suicides data for females, in general, are also noisy, 
# adults being for the most part the exception.
# # 
# # * Trends seem to suggest that, to some degree, homicides 
# and suicides belong to different processes -- the trends are 
# somewhat different by race-sex and age-group combinations.  
# So let's keep an eye on this -- if we were thinking about 
# bringing them together under the single umbrella of gun 
# control policies and the respective issue-positions of 
# the parties, I think that's not going to be easy.
# # 
# # * The data for homicides, especially for black male and female adults, 
# seem to oscillate with the party of the president.  However, the trends 
# oscillate in such a manner that detrending the series is not obvious 
# -- I can anticipate a lot of variation from one method to another. 
# At first sight, interestingly, the relationship seems to be against 
# Democrats before Reagan and against Republicans once Reagan 
# got in office (?).  This means that, whatever is influencing black 
# adults' homicides, is split in two historical trends.  This means 
# we will need not one but two theoretical frameworks -- complicated.
# # 
# # * Needless to say -- the Reagan and Bush's administrations were disastrous.
# 
# * Jon, for now, let's focus on black and white male adults.  
# To keep things simple, let's run two quick exercises. Please:
# 
# Exercise 1:
# 
# Let's separate the linear trends by presidential regime. 
# This can be done running a model using linear splines with knots at the year 
# when there was a change of president.  In our case, the knots are the first 
# year for each Republican president.  The model looks like this:
#   
#   y = a + b1*t + b2*(t-y1)*D1 + b3*(t-y2)*D2 + b4*(t-y3)*D3... + e    [1]
# 
# where y1 is the year of the first knot (say, 1970) and D1 is  a 
# dummy equal to "1" if t is greater or equal to 1970, and "0" otherwise; 
# y2 is the year of the second knot (1977) and D2 a dummy 
# (1=greater or equal to 1977, 0=otherwise), and so on.  To simplify things 
# you can create a variable for each term (t-y_)*D_  where each of 
# them equals "0" before the knot and equal to "t" at and after the knot. 
# So, for example, for the term (t-y1)*D1, X1 will be equal to "0" for years 
# 1968-1969 and equal to 1970, 1971, 1972, 1973... (at and after the knot) --
#   the same for the other terms.
# 
# # From this regression (run for each race-sex (4), age-group (adults), 
# and mortality cause (3) -- for a total of 12 regressions), recover the fitted values, 
# and plot them using scatter and the fitted line.  Estimate the model using OLS. 
# The models will simply let us know if the linear trends are at least in the 
# right direction or if there is a patterns in the trends...
# 
# Exercise 2:
#   
#   Those trends are bumpy.  But to run at least a first approximation, let's do two detrending exercises.
# 
# 
# E1. Let's generate a 6th order polynomial function of time.  Create 6 variables, where t1=year-1968, t2=t1^2, t3=t1^3... t6=t1^6.  Then run the following model:
#   
#   y = a + b1*t + b2*t2 +... b6*t6 + e     [2]
# 
# recover the residuals of y (again, for each race-sex (4), age-group (adults), and mortality cause (3)). Then run this regression:
#   
#   res_y = a + b1*rep + e     [3]
# 
# where rep is the Republican dummy.  Recover the fitted values and plot them.
# 
# 
# E2. Detrend y using the Hodrick-Prescott (HP) filter (there should be a R package that does that). There are two conventional values for the smoothing parameter  for annual data: 6.25 and 100. Please use both since they usually do vary the detrending a lot.  Once you detrended y using HP, then run equation 3, where res_y will be the detrended values of y using HP, and plot the fitted values.
# 
# NOTE: We are, for now, not worrying about statistical significance. All of the above equations, therefore, do not correct the SEs and they can be estimated using OLS.  We just care for the fitted values... 
# 
# Thanks a lot.
  
#   Variable name: "rep" (1=Republican, 0=Democrat). This variable is lagged 1 year.
# 
# Republican years: 2010-2014, 1994-2001, 1978-1981, 1968-1969
# All others are Democrats.
# 
# That will get us started.
# 
# Thanks!

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




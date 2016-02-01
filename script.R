# USA Fall in violence paper

# The aim of this session is to produce various figures which show how mortality rates due to assault/homicide 
# have varied with age, sex, and ethnicity over time in the USA. THe aim of doing this is to produce some a series 
# of figures which show the trends for as long a period as possible. In particular it is important to see if the trends 
#  follow those observed more generally about convergence between Blacks and Non-Blacks in age specific mortality rates 


 
# To do:  -----------------------------------------------------------------

# 1) look on CDC for longer term data on assault homicide
# 2) Look for existing data relevant to this and bring the code into this file only 



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



# ICD 08 data -------------------------------------------------------------

# First I will look at the ICD 08 data I downloaded. This uses the 
# CDC Compressed Mortality data file. I have included a variable 
# giving different ICD codes so should be able to extract external, all cause, violent,
# suicide and undetermined from this 

icd_08 <- read_delim(
  file = "data/icd_08_age_gender_race_year_icd chapter.txt", 
  delim = "\t", 
  na = "Not Applicable", 
  col_types = paste(rep("c", 14), collapse = "")
)

# As the ICD codes here are the larger groups rather than subgroups, what they can provide is all cause and external
# but not the sub-types of external of interest. These will have to be extracted separately 

# Where there is no numerator, i.e. number of deaths, no denominator ie. population at risk is defined as well. 
# This will have to be checked, and replaced with the true values 

icd_08_external <- icd_08 %>% 
  select(
    race = Race, 
    sex = Gender, 
    year = Year,
    age = `Age Group Code`,
    icd = `ICD Chapter`, icd_code = `ICD Chapter Code`, 
    death_count = Deaths, 
    population_count = Population
  ) %>% 
  mutate(
    race = recode(
      race, 
      "
      'Black or African American' = 'black';
      'White' = 'white';
      'Other Race' = 'other'
      "          
                ),
    sex = tolower(sex)
  ) %>% 
  mutate(
    death_count = as.numeric(str_replace_all(death_count, ",", "")),
    population_count = as.numeric(str_replace_all(population_count, ",", ""))
  ) %>% 
  group_by(race, sex, year, age) %>% 
  mutate(population_count = max(population_count)) %>% # this changes population counts that are zere, because the death counts are zero, to the correct value
  filter(icd_code == "E800-E999") %>% 
  ungroup %>% 
  filter(age != "NS") %>% 
  mutate(
    age = factor(
      age, 
      levels = c("1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"), 
      ordered = T)
    ) %>% 
  ungroup %>% 
  mutate(cause = "all_external") %>% 
  select(race, sex, year, age, cause, death_count, population_count)
  

icd_08_allcause <- icd_08 %>% 
  select(
    race = Race, 
    sex = Gender, 
    year = Year,
    age = `Age Group Code`,
    icd = `ICD Chapter`, icd_code = `ICD Chapter Code`, 
    death_count = Deaths, 
    population_count = Population
  ) %>% 
  mutate(
    race = recode(
      race, 
      "
      'Black or African American' = 'black';
      'White' = 'white';
      'Other Race' = 'other'
      "          
    ),
    sex = tolower(sex)
    ) %>%  
  filter(age != "NS") %>% 
  mutate(
    age = factor(
      age, 
      levels = c("1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"), 
      ordered = T)
  ) %>% 
  group_by(race, sex, year, age) %>% 
  mutate(population_count = max(population_count)) %>% # this changes population counts that are zere, because the death counts are zero, to the correct value
  mutate(
    death_count = as.numeric(str_replace_all(death_count, ",", "")),
    population_count = as.numeric(str_replace_all(population_count, ",", ""))
  ) %>% 
  summarise(
    death_count = sum(death_count),
    population_count = population_count[1]
    ) %>% 
  ungroup %>% 
  mutate(cause = "all_cause") %>% 
  select(race, sex, year, age, cause, death_count, population_count)

# want non-external

tmp1 <- icd_08_allcause %>% 
  bind_rows(icd_08_external) %>% 
  select(-population_count) %>% 
  spread(cause, death_count) %>% 
  mutate(non_external = all_cause - all_external) %>% 
  gather(key = cause, value = death_count, -race, -sex, -year, -age) 

tmp2 <- icd_08_allcause %>% select(race, sex, year, age, population_count)

icd_08_3cause <- tmp1  %>% left_join(tmp2)

rm(icd_08_3groups, icd_08_allcause, icd_08_external)


# Now to do something similar with ICD 9 



icd_09 <- read_delim(
  file = "data/icd_09_age_gender_race_year_icd chapter.txt", 
  delim = "\t", 
  na = "Not Applicable", 
  col_types = paste(rep("c", 14), collapse = "")
)


icd_09_all_external <- icd_09 %>% select(
  notes = Notes, year = Year, age = `Age Group Code`, sex = Gender, race = Race,
  icd = `ICD Chapter`, icd_code = `ICD Chapter Code`, death_count = Deaths, population_count = Population
) %>% 
  mutate(
    race = recode(
      race, 
      "
      'Black or African American' = 'black';
      'White' = 'white';
      'Other Race' = 'other'
      "          
    ),
    sex = tolower(sex)
    ) %>% 
  mutate(
    death_count = as.numeric(str_replace_all(death_count, ",", "")),
    population_count = as.numeric(str_replace_all(population_count, ",", ""))
  ) %>% 
  group_by(race, sex, year, age) %>% 
  mutate(population_count = max(population_count)) %>% # this changes population counts that are zere, because the death counts are zero, to the correct value
  filter(icd_code == "E800-E999") %>% 
  ungroup %>% 
  filter(age != "NS") %>% 
  mutate(
    age = factor(
      age, 
      levels = c("1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"), 
      ordered = T)
  ) %>% 
  ungroup %>% 
  mutate(cause = "all_external") %>% 
  select(race, sex, year, age, cause, death_count, population_count)

# All cause - looking at total in notes

icd_09_all_cause <- icd_09 %>% select(
  notes = Notes, year = Year, age = `Age Group Code`, sex = Gender, race = Race,
  icd = `ICD Chapter`, icd_code = `ICD Chapter Code`, death_count = Deaths, population_count = Population
) %>% 
  filter(notes == "Total") %>% 
  mutate(
    race = recode(
      race, 
      "
      'Black or African American' = 'black';
      'White' = 'white';
      'Other Race' = 'other'
      "          
    ),
    sex = tolower(sex)
  ) %>% 
  mutate(
    death_count = as.numeric(str_replace_all(death_count, ",", "")),
    population_count = as.numeric(str_replace_all(population_count, ",", ""))
  ) %>% 
  filter(age != "NS") %>% 
  mutate(
    age = factor(
      age, 
      levels = c("1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+"), 
      ordered = T)
  ) %>% 
  mutate(cause = "all_cause") %>% 
  select(race, sex, year, age, cause, death_count, population_count)


tmp1 <- icd_09_all_cause %>% 
  bind_rows(icd_09_all_external) %>% 
  select(-population_count) %>% 
  spread(cause, death_count) %>% 
  mutate(non_external = all_cause - all_external) %>% 
  gather(key = cause, value = death_count, -race, -sex, -year, -age) 

tmp2 <- icd_09_all_cause %>% select(race, sex, year, age, population_count)

icd_09_3cause <- tmp1  %>% left_join(tmp2)

icd_09_3cause <- icd_09_3cause %>% filter(race != "" & sex != "" & year != "")

rm(tmp1, tmp2)


icd_08_3cause <- icd_08_3cause  %>% 
  mutate(icd_class = "icd_08")  %>% 
  select(icd_class, year, age, race, sex, cause, death_count, population_count) %>% 
  arrange(year, age, race, sex, cause)


icd_09_3cause <- icd_09_3cause %>% 
  mutate(icd_class = "icd_09") %>% 
  select(icd_class, year, age, race, sex, cause, death_count, population_count) %>% 
  arrange(year, age, race, sex, cause)


icd_08_09_3cause <- bind_rows(icd_08_3cause, icd_09_3cause)

write_csv(icd_08_09_3cause, path = "data/tidied/icd_08_09_3cause.csv")




# 8 causes , icd 8 , icd 9 and icd 10 -------------------------------------

# The 8 causes are 
# all cause 
# all external 
# asssault/homicide
# self harm (intentional or undetermined)
# intentional self harm
# undetermined self harm
# non-external
# vehicle-based




# 8 cause, icd 8 ----------------------------------------------------------




# 8 causes - icd 9  -------------------------------------------------------


icd_09_all_cause <- read_delim(
  "data/icd_09_year_age_gender_race_allcause.txt",
  delim = "\t",
  col_types = paste(rep("c", 12), collapse = "")
)

icd_09_all_cause <- icd_09_all_cause %>% 
  filter(Notes == "") %>% 
  select(
    year = `Year Code`, age = `Age Group Code`, sex = Gender, race = Race, 
    death_count = Deaths, population_count = Population
         ) %>% 
  mutate(
    sex = tolower(sex),
    r2 = recode(race, 
                recodes = "
                'Black or African American' = 'black';
                'Other Race' = 'other';
                'White' = 'white'
                "
                )
    ) %>% 
  mutate(cause = "all_cause") %>% 
  mutate(icd_class = "icd_09") %>% 
  select(icd_class, year, age, sex, race = r2, cause, death_count, population_count) %>% 
  filter(population_count != "Not Applicable") %>% 
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
         ) 


# external 

icd_09_external <- read_delim(
  "data/icd_09_year_age_gender_race_external.txt",
  delim = "\t",
  col_types = paste(rep("c", 12), collapse = "")
)

icd_09_external <- icd_09_external %>% 
  filter(Notes == "") %>% 
  select(
    year = `Year Code`, age = `Age Group Code`, sex = Gender, race = Race, 
    death_count = Deaths, population_count = Population
  ) %>% 
  mutate(
    sex = tolower(sex),
    r2 = recode(race, 
                recodes = "
                'Black or African American' = 'black';
                'Other Race' = 'other';
                'White' = 'white'
                "
    )
  ) %>% 
  mutate(cause = "all_external") %>% 
  mutate(icd_class = "icd_09") %>% 
  select(icd_class, year, age, sex, race = r2, cause, death_count, population_count) %>% 
  filter(population_count != "Not Applicable") %>% 
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
  )  


# the other categories can all be extracted from the following file 

icd_09_icdsub <- read_delim(
  "data/icd_09_year_age_gender_race_icd_subchapter.txt",
  delim = "\t",
  col_types = paste(rep("c", 14), collapse = "")
)

# Motor related 
icd_09_motor <- icd_09_icdsub %>% 
  filter(`ICD Sub-Chapter Code` %in% c(
    "E810-E819", "E820-E825", "E826-E829")) %>% 
  filter(Notes == "") %>% 
  select(year = Year, age = `Age Group Code`, sex = Gender, race = Race, 
         death_count = Deaths, population_count = Population) %>% 
  filter(population_count != "Not Applicable") %>% 
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
    ) %>% 
  group_by(year, age, sex, race) %>% 
  summarise(
    death_count = sum(death_count), 
    population_count = sum(population_count)
    ) %>% 
  ungroup %>% 
  mutate(
    sex = tolower(sex),
    r2 = recode(race, 
                recodes = "
                'Black or African American' = 'black';
                'Other Race' = 'other';
                'White' = 'white'
                "
    )
    ) %>% 
  mutate(cause = "motor") %>% 
  mutate(icd_class = "icd_09") %>% 
  select(icd_class, year, age, sex, race = r2, cause, death_count, population_count) 


  
# Assault and homicide
icd_09_assault <- icd_09_icdsub %>% 
  filter(`ICD Sub-Chapter Code` %in% c("E960-E969")) %>% 
  filter(Notes == "") %>% 
  select(year = Year, age = `Age Group Code`, sex = Gender, race = Race, 
         death_count = Deaths, population_count = Population) %>% 
  filter(population_count != "Not Applicable") %>% 
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
  ) %>% 
  mutate(
    sex = tolower(sex),
    r2 = recode(race, 
                recodes = "
                'Black or African American' = 'black';
                'Other Race' = 'other';
                'White' = 'white'
                "
    )
    ) %>% 
  mutate(cause = "assault") %>% 
  mutate(icd_class = "icd_09") %>% 
  select(icd_class, year, age, sex, race = r2, cause, death_count, population_count) 

# intentional self harm
icd_09_intent <- icd_09_icdsub %>% 
  filter(`ICD Sub-Chapter Code` %in% c("E950-E959")) %>% 
  filter(Notes == "") %>% 
  select(year = Year, age = `Age Group Code`, sex = Gender, race = Race, 
         death_count = Deaths, population_count = Population) %>% 
  filter(population_count != "Not Applicable") %>% 
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
  ) %>% 
  mutate(
    sex = tolower(sex),
    r2 = recode(race, 
                recodes = "
                'Black or African American' = 'black';
                'Other Race' = 'other';
                'White' = 'white'
                "
    )
    ) %>% 
  mutate(cause = "intentional_self_harm") %>% 
  mutate(icd_class = "icd_09") %>% 
  select(icd_class, year, age, sex, race = r2, cause, death_count, population_count) 

# undetermined intent
icd_09_undetermined <- icd_09_icdsub %>% 
  filter(`ICD Sub-Chapter Code` %in% c("E980-E989")) %>% 
  filter(Notes == "") %>% 
  select(year = Year, age = `Age Group Code`, sex = Gender, race = Race, 
         death_count = Deaths, population_count = Population) %>% 
  filter(population_count != "Not Applicable") %>% 
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
  ) %>% 
  mutate(
    sex = tolower(sex),
    r2 = recode(race, 
                recodes = "
                'Black or African American' = 'black';
                'Other Race' = 'other';
                'White' = 'white'
                "
    )
    ) %>% 
  mutate(cause = "undetermined") %>% 
  mutate(icd_class = "icd_09") %>% 
  select(icd_class, year, age, sex, race = r2, cause, death_count, population_count) 




# to dos 

# 1 ) calculate non external 
# 2) combine intentioanl self harm and undetermined into a single category
# 3) bind all together 



# 1 ) calculate non external 

tmp1 <- icd_09_all_cause %>% 
  select(year, age, race, sex, cause, death_count)

tmp2 <- icd_09_external %>% 
  select(year, age, race, sex, cause, death_count)

tmp3 <- bind_rows(tmp1, tmp2)
rm(tmp1, tmp2)


tmp4 <- tmp3 %>% 
  spread(key = cause, value = death_count) %>% 
  mutate(non_external = all_cause - all_external) %>%
  gather(key = "cause", value = "death_count", -year, -age, -race, -sex) %>% 
  filter(cause == "non_external")

rm(tmp3)

tmp5 <- icd_09_all_cause %>% 
  select(year, age, race, sex, population_count) %>% 
  ungroup

icd_09_nonexternal <- tmp4 %>% inner_join(tmp5)

rm(tmp4, tmp5)

icd_09_nonexternal <- icd_09_nonexternal %>% 
  mutate(icd_class = "icd_09") %>% 
  select(icd_class, year, age, race, sex, cause, death_count, population_count)

# 2) combine intentional self harm and undetermined into a single category

# bind intent and undetermined together

tmp1 <- icd_09_undetermined %>% bind_rows(icd_09_intent)

tmp2 <- tmp1 %>% 
  select(-population_count) %>% 
  spread(key = cause, value = death_count) %>% 
  mutate(intentional_or_undetermined_self_harm = intentional_self_harm + undetermined) %>% 
  gather(key = "cause", value = "death_count", -icd_class, -year, -age, -race, -sex) %>% 
  filter(cause == "intentional_or_undetermined_self_harm")

rm(tmp1)

tmp3 <- icd_09_undetermined %>% 
  select(icd_class, year, age, race, sex, population_count)

icd_09_self_harm <- tmp2 %>% inner_join(tmp3)

rm(tmp2, tmp3)

# 3) bind all together 

icd_09_8cause <- icd_09_intent %>% 
  bind_rows(icd_09_assault) %>% 
  bind_rows(icd_09_all_cause) %>% 
  bind_rows(icd_09_motor) %>% 
  bind_rows(icd_09_external) %>% 
  bind_rows(icd_09_nonexternal) %>% 
  bind_rows(icd_09_self_harm) %>% 
  bind_rows(icd_09_undetermined)

# 4 ) write to file

write_csv(icd_09_8cause, path = "data/tidied/icd_09_8cause.csv")




# 8 causes, icd 10 --------------------------------------------------------


# Now to look at what we can do with icd_10

icd_10_external <- read_delim(
  "data/icd_10_underlying_single_age_year_age_gender_race_hispanic_external.txt", 
  delim = "\t", col_types = paste(rep("c", 14), collapse = "")
  )

icd_10_external <- icd_10_external %>% 
    filter(Notes != "Total") %>% 
    select(-Notes) %>% 
    select(year = `Year Code`, age = `Single-Year Ages Code`, 
           sex = Gender, race =Race, hispanic = `Hispanic Origin`, 
           death_count = Deaths, population_count = Population
           ) %>% 
    filter(population_count != "Not Applicable") %>% 
    mutate(
      group = ifelse(race == "White" & hispanic == "Not Hispanic or Latino", "white", "other"),
      group = ifelse(race == "Black or African American" & hispanic == "Not Hispanic or Latino", "black", group), 
      group = ifelse(hispanic =="Hispanic or Latino", "hispanic", group)
    ) %>% 
  mutate(sex = tolower(sex)) %>% 
  mutate(icd_class = "icd_10") %>% 
  mutate(cause = "all_external") %>% 
  select(icd_class, year, age, race = group, sex, cause, death_count, population_count) %>% 
  mutate(
    death_count = as.numeric(str_replace_all(death_count, ",", "")),
    population_count = as.numeric(str_replace_all(population_count, ",", ""))
  ) %>% 
  group_by(icd_class, year, age, race, sex, cause) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) 



# ICD 10 - all cause 

icd_10_allcause <- read_delim(
  "data/icd_10_underlying_single_age_year_gender_hisp_race_allcause.txt", 
  delim = "\t", col_types = paste(rep("c", 14), collapse = "")
)

icd_10_allcause <- icd_10_allcause %>% 
  filter(Notes != "Total") %>% 
  select(-Notes) %>% 
  select(year = `Year Code`, age = `Single-Year Ages Code`, 
         sex = Gender, race =Race, hispanic = `Hispanic Origin`, 
         death_count = Deaths, population_count = Population
  ) %>% 
  filter(population_count != "Not Applicable") %>% 
  mutate(
    group = ifelse(race == "White" & hispanic == "Not Hispanic or Latino", "white", "other"),
    group = ifelse(race == "Black or African American" & hispanic == "Not Hispanic or Latino", "black", group), 
    group = ifelse(hispanic =="Hispanic or Latino", "hispanic", group)
  ) %>% 
  mutate(sex = tolower(sex)) %>% 
  mutate(icd_class = "icd_10") %>% 
  mutate(cause = "all_cause") %>% 
  select(icd_class, year, age, race = group, sex, cause, death_count, population_count) %>% 
  mutate(
    death_count = as.numeric(str_replace_all(death_count, ",", "")),
    population_count = as.numeric(str_replace_all(population_count, ",", ""))
  ) %>% 
  group_by(icd_class, year, age, race, sex, cause) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count))




# ICD 10 - vehicle

icd_10_vehicle <- read_delim(
  "data/icd_10_underlying_singleage_year_gender_hisp_race_motor.txt", 
  delim = "\t", col_types = paste(rep("c", 14), collapse = "")
)

icd_10_vehicle <- icd_10_vehicle %>% 
  filter(Notes != "Total") %>% 
  select(-Notes) %>% 
  select(year = `Year Code`, age = `Single-Year Ages Code`, 
         sex = Gender, race =Race, hispanic = `Hispanic Origin`, 
         death_count = Deaths, population_count = Population
  ) %>% 
  filter(population_count != "Not Applicable") %>% 
  mutate(
    group = ifelse(race == "White" & hispanic == "Not Hispanic or Latino", "white", "other"),
    group = ifelse(race == "Black or African American" & hispanic == "Not Hispanic or Latino", "black", group), 
    group = ifelse(hispanic =="Hispanic or Latino", "hispanic", group)
  ) %>% 
  mutate(sex = tolower(sex)) %>% 
  mutate(icd_class = "icd_10") %>% 
  mutate(cause = "vehicle") %>% 
  select(icd_class, year, age, race = group, sex, cause, death_count, population_count) %>% 
  mutate(
    death_count = as.numeric(str_replace_all(death_count, ",", "")),
    population_count = as.numeric(str_replace_all(population_count, ",", ""))
  ) %>% 
  group_by(icd_class, year, age, race, sex, cause) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count))




# ICD 10 - assault

icd_10_assault <- read_delim(
  "data/icd_10_underlying_single_age_year_race_hispanic_gender_assault.txt", 
  delim = "\t", col_types = paste(rep("c", 14), collapse = "")
)

icd_10_assault <- icd_10_assault %>% 
  filter(Notes != "Total") %>% 
  select(-Notes) %>% 
  select(year = `Year Code`, age = `Single-Year Ages Code`, 
         sex = Gender, race =Race, hispanic = `Hispanic Origin`, 
         death_count = Deaths, population_count = Population
  ) %>% 
  filter(population_count != "Not Applicable") %>% 
  mutate(
    group = ifelse(race == "White" & hispanic == "Not Hispanic or Latino", "white", "other"),
    group = ifelse(race == "Black or African American" & hispanic == "Not Hispanic or Latino", "black", group), 
    group = ifelse(hispanic =="Hispanic or Latino", "hispanic", group)
  ) %>% 
  mutate(sex = tolower(sex)) %>% 
  mutate(icd_class = "icd_10") %>% 
  mutate(cause = "assault") %>% 
  select(icd_class, year, age, race = group, sex, cause, death_count, population_count) %>% 
  mutate(
    death_count = as.numeric(str_replace_all(death_count, ",", "")),
    population_count = as.numeric(str_replace_all(population_count, ",", ""))
  ) %>% 
  group_by(icd_class, year, age, race, sex, cause) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count))



# ICD 10 - intentional self harm

icd_10_intent <- read_delim(
  "data/icd_10_underlying_singleage_year_gender_hisp_race_intent.txt", 
  delim = "\t", col_types = paste(rep("c", 14), collapse = "")
)

icd_10_intent <- icd_10_intent %>% 
  filter(Notes != "Total") %>% 
  select(-Notes) %>% 
  select(year = `Year Code`, age = `Single-Year Ages Code`, 
         sex = Gender, race =Race, hispanic = `Hispanic Origin`, 
         death_count = Deaths, population_count = Population
  ) %>% 
  filter(population_count != "Not Applicable") %>% 
  mutate(
    group = ifelse(race == "White" & hispanic == "Not Hispanic or Latino", "white", "other"),
    group = ifelse(race == "Black or African American" & hispanic == "Not Hispanic or Latino", "black", group), 
    group = ifelse(hispanic =="Hispanic or Latino", "hispanic", group)
  ) %>% 
  mutate(sex = tolower(sex)) %>% 
  mutate(icd_class = "icd_10") %>% 
  mutate(cause = "intentional_self_harm") %>% 
  select(icd_class, year, age, race = group, sex, cause, death_count, population_count) %>% 
  mutate(
    death_count = as.numeric(str_replace_all(death_count, ",", "")),
    population_count = as.numeric(str_replace_all(population_count, ",", ""))
  ) %>% 
  group_by(icd_class, year, age, race, sex, cause) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count))



# ICD 10, Undetermined intent
icd_10_undetermined <- read_delim(
  "data/icd_10_underlying_year_singleage_gender_hisp_race_undetermined.txt", 
  delim = "\t", col_types = paste(rep("c", 14), collapse = "")
)

icd_10_undetermined <- icd_10_undetermined %>% 
  filter(Notes != "Total") %>% 
  select(-Notes) %>% 
  select(year = `Year Code`, age = `Single-Year Ages Code`, 
         sex = Gender, race =Race, hispanic = `Hispanic Origin`, 
         death_count = Deaths, population_count = Population
  ) %>% 
  filter(population_count != "Not Applicable") %>% 
  mutate(
    group = ifelse(race == "White" & hispanic == "Not Hispanic or Latino", "white", "other"),
    group = ifelse(race == "Black or African American" & hispanic == "Not Hispanic or Latino", "black", group), 
    group = ifelse(hispanic =="Hispanic or Latino", "hispanic", group)
  ) %>% 
  mutate(sex = tolower(sex)) %>% 
  mutate(icd_class = "icd_10") %>% 
  mutate(cause = "undetermined") %>% 
  select(icd_class, year, age, race = group, sex, cause, death_count, population_count) %>% 
  mutate(
    death_count = as.numeric(str_replace_all(death_count, ",", "")),
    population_count = as.numeric(str_replace_all(population_count, ",", ""))
  ) %>% 
  group_by(icd_class, year, age, race, sex, cause) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count))



# to dos 

# 1 ) calculate non external 
# 2) combine intentioanl self harm and undetermined into a single category
# 3) bind all together 



# 1 ) calculate non external 

tmp1 <- icd_10_allcause %>% 
  select(year, age, race, sex, cause, death_count)

tmp2 <- icd_10_external %>% 
  select(year, age, race, sex, cause, death_count)

tmp3 <- bind_rows(tmp1, tmp2)
rm(tmp1, tmp2)


tmp4 <- tmp3 %>% 
  spread(key = cause, value = death_count) %>% 
  mutate(non_external = all_cause - all_external) %>%
  gather(key = "cause", value = "death_count", -icd_class, -year, -age, -race, -sex) %>% 
  filter(cause == "non_external")

rm(tmp3)

tmp5 <- icd_10_allcause %>% 
  select(year, age, race, sex, population_count) %>% 
  ungroup

icd_10_nonexternal <- tmp4 %>% inner_join(tmp5)

rm(tmp4, tmp5)
# 2) combine intentional self harm and undetermined into a single category

# bind intent and undetermined together

tmp1 <- icd_10_undetermined %>% bind_rows(icd_10_intent)

tmp2 <- tmp1 %>% 
  select(-population_count) %>% 
  spread(key = cause, value = death_count) %>% 
  mutate(intentional_or_undetermined_self_harm = intentional_self_harm + undetermined) %>% 
  gather(key = "cause", value = "death_count", -icd_class, -year, -age, -race, -sex) %>% 
  filter(cause == "intentional_or_undetermined_self_harm")

rm(tmp1)

tmp3 <- icd_10_undetermined %>% 
  select(icd_class, year, age, race, sex, population_count)

icd_10_self_harm <- tmp2 %>% inner_join(tmp3)

rm(tmp2, tmp3)

# 3) bind all together 

icd_10_8cause <- icd_10_intent %>% 
  bind_rows(icd_10_assault) %>% 
  bind_rows(icd_10_allcause) %>% 
  bind_rows(icd_10_vehicle) %>% 
  bind_rows(icd_10_external) %>% 
  bind_rows(icd_10_nonexternal) %>% 
  bind_rows(icd_10_self_harm) %>% 
  bind_rows(icd_10_undetermined)

# 4 ) write to file

write_csv(icd_10_8cause, path = "data/tidied/icd_10_8cause.csv")




# Progress : 
#   
#   Found and extracted data on legal intervention from 1968-1974
#     - located in data/long_term_external_causes


# Broader category - 1968 to 1978- all external causes of death 
 # Males and females extracted separately because of file size limitations 
#E800-899 - external causes of death

# To do - same for females using ICD 8
# to do - same for males and females using ICD 9 
# To do - same for males and females using ICD 10



#Let's try to access this data

dta <- read_delim(
  "data/legal_intervention_Compressed Mortality, 1968-1978.txt", 
  delim = "\t", 
  na = "Not Applicable", 
  col_types = paste(rep("c", 14), collapse = "")
  )


# "Notes"	
# "Age Group"	
# "Age Group Code"	
# "Gender"	
# "Gender Code"	
# "Race"	
# "Race Code"	
# "Year"	
# "Year Code"	
# "Cause of death"	
# "Cause of death Code"	
# Deaths	
# Population	
# Crude Rate

legal_intervention_1968_1978 <- dta %>% filter(Notes == "Total") %>% 
  select(race = Race, sex = Gender, year = Year, 
         age = `Age Group`, 
         death_count = Deaths, 
         population_count = Population
         ) %>%
  mutate(death_count = as.integer(death_count), 
         population_count = as.integer(population_count)
         ) %>%
  filter(race != "", sex != "", age != "", year != "", !is.na(death_count), !is.na(population_count)) %>%
  mutate(age = str_trim(str_replace(str_replace(age, "year", ""), "s", ""))) 


#Death rate by year 
legal_intervention_1968_1978 %>% group_by(race, sex, year) %>%
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
  mutate(death_rate = 100000 * death_count / population_count ) %>% 
  ggplot(.) + 
  geom_line(aes(x = year, y = death_rate, group = race, colour = race)) + 
              facet_wrap(~ sex, scales = "free_y")
  


dta <- read_delim("data/long_term_external_causes/external_males_Compressed Mortality, 1968-1978.txt", delim = "\t", na = "Not Applicable", col_types = paste(rep("c", 14), collapse = ""))


all_external_male_1968_1978 <- dta %>%  
  select(race = Race, sex = Gender, year = Year, age = `Age Group`, death_count = Deaths, population_count = Population) %>%
  mutate(death_count = as.numeric(death_count), population_count = as.numeric(population_count)) %>%
  filter(race != "", sex != "", age != "", year != "", !is.na(death_count), !is.na(population_count)) %>%
  mutate(age = str_trim(str_replace(str_replace(age, "year", ""), "s", ""))) %>%
  group_by(age, sex, race, year) %>%
  summarise(death_count = sum(death_count), population_count = sum(population_count))

all_external_male_1968_1978 %>% 
  group_by(year, sex, race) %>%
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
  mutate(death_rate = 100000 * death_count / population_count) %>% 
  ggplot()  + 
  geom_line(aes(y = death_rate, x = year, group = race, colour = race))


dta <- read_delim("data/long_term_external_causes/external_Compressed Mortality, 1979-1998.txt", delim = "\t")


# Homicide using ICD categories - homicide ICD 8 

dta_homicide_icd08 <- read_delim("data/long_term_external_causes/homicide_icd_8_Compressed Mortality, 1968-1978.txt", delim = "\t")

dta_hom_icd08_rate <- dta_homicide_icd08 %>% 
  filter(Notes == "Total") %>% 
  select(age = `Age Group`, sex = Gender, race = Race, year = Year, death_count = Deaths, population_count = Population) %>%
  select(age, year, race, sex, death_count, population_count) %>% 
  filter(!is.na(race), !is.na(sex), !is.na(year), !is.na(age), age != "Not Stated") %>%
  mutate(year = as.integer(year)) %>% 
  group_by(race, sex, year) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>%
  mutate(death_rate = 100000 * death_count / population_count) 

dta_hom_icd08_rate %>% 
  ggplot(.) +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  scale_y_log10(breaks = c(1, 5, 10, 50, 100))





# Homicide- ICD 9 

dta_homicide_icd09 <- read_delim("data/long_term_external_causes/homicide_icd9.txt", delim = "\t",
                                 col_types = "ccccciicccciid")

dta_hom_icd09_rate <- dta_homicide_icd09 %>% 
  filter(Notes == "Total") %>% 
  select(age = `Age Group`, sex = Gender, race = Race, year = Year, death_count = Deaths, population_count = Population) %>%
  select(age, year, race, sex, death_count, population_count) %>% 
  filter(!is.na(race), !is.na(sex), !is.na(year), !is.na(age), age != "Not Stated") %>%
  filter(race != "") %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(race, sex, year) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>%
  mutate(death_rate = 100000 * death_count / population_count)

dta_hom_icd09_rate %>% 
  ggplot(.) +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  scale_y_log10(breaks= c(1, 5, 10, 50, 100))




# Homicide - ICD 10 

#  Can't find a homicide category so looking at assault only 

dta_assault_icd10 <- read_delim("data/long_term_external_causes/assault_icd10_hisprace.txt", delim = "\t")

dta_assault_icd10_rate <- dta_assault_icd10 %>% 
  select(age = `Age Group`, year = Year, sex = Gender, race = Race, hispanic = `Hispanic Origin`, death_count = Deaths, population_count = Population) %>%
  filter(race = !is.na(race), age = !is.na(age), year = !is.na(year), sex = !is.na(sex)) %>%
  mutate(
    group = NA,
    group = ifelse(race == "White" & hispanic == "Not Hispanic or Latino", "White Non-Hispanic", group),
    group = ifelse(race == "Black or African American" & hispanic == "Not Hispanic or Latino", "Black Non-Hispanic", group),
    group = ifelse(hispanic == "Hispanic or Latino", "Hispanic", group)
  ) %>% 
  filter(!is.na(group)) %>% 
  select(sex, race = group, age, year, death_count, population_count) %>% 
  mutate(population_count = as.numeric(population_count)) %>% 
  filter(population_count = !is.na(population_count)) %>% 
  group_by(sex, race, year) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
  mutate(death_rate = 100000 * death_count / population_count) 

dta_assault_icd10_rate %>% 
  ggplot() +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  scale_y_log10(breaks = c(1, 5, 10, 50, 100))



# Group the above together 

dta_hom_icd08_rate
dta_hom_icd09_rate

dta_hom_icd08_09_rate <- rbind(dta_hom_icd08_rate, dta_hom_icd09_rate)

dta_hom_icd08_09_rate %>% 
  ggplot() +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y")


tmp_10 <- dta_assault_icd10_rate %>% 
  select(r1 = race, sex, year, death_count, population_count, death_rate) %>% 
  mutate(
    r2 = recode(
      r1, 
      "
      'Black Non-Hispanic' = 'Black';
      'White Non-Hispanic' = 'White';
      'Hispanic' = 'Hispanic/Other'
      "
    )
  ) %>% ungroup() %>% 
  select(race = r2, sex, year, death_count, population_count, death_rate) 




tmp_08_09 <- dta_hom_icd08_09_rate %>%
  select(r1 = race, sex, year, death_count, population_count, death_rate) %>% 
  mutate(
    r2 = recode(
      r1, 
      "
      'Black or African American' = 'Black';
      'White' = 'White';
      'Other Race' = 'Hispanic/Other'
      "
    )
    ) %>% ungroup() %>%
  select(race = r2, sex, year, death_count, population_count, death_rate) 
  

homassault_icd_08_09_10_rates <- rbind(tmp_08_09, tmp_10)


homassault_icd_08_09_10_rates %>% 
  ggplot() +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  geom_vline(xintercept = c(1979, 1999)) +
  scale_y_log10(breaks= c(1, 2, 5, 10, 20, 50, 100)) + 
  labs(y = "Death rate from assault/homicide per 100 000")


# Grouping together hispanic/other and non-black for more comparability with ICD-08 and ICD-09 groups
homassault_icd_08_09_10_rates %>% 
  select(race, sex, year, death_rate) %>%
  spread(key = race, value = death_rate) %>%
  mutate(`Non-Black` = (White * `Hispanic/Other`)^0.5) %>%
  gather(key = race, value = death_rate, -sex, -year) %>%
  mutate(
    death_rate = ifelse(race == "White" & year >= 1999, NA, death_rate),
    death_rate = ifelse(race == "Hispanic/Other" & year >= 1999, NA, death_rate),
    death_rate = ifelse(race == "Non-Black" & year < 1999, NA, death_rate)
    ) %>% 
  ggplot() +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  geom_vline(xintercept = c(1979, 1999)) +
  scale_y_log10(breaks= c(1, 2, 5, 10, 20, 50, 100)) + 
  labs(y = "Death rate from assault/homicide per 100 000")



# Now to do look at data on death from external causes  (The supercategory)

dta_ext_icd08 <- read_delim("data/long_term_external_causes/external_icd08.txt", delim = "\t")

dta_ext_icd09 <- read_delim("data/long_term_external_causes/external_icd09.txt", delim = "\t")

dta_ext_icd10 <- read_delim("data/long_term_external_causes/external_icd10.txt", delim = "\t")


ext_icd08_rate <- dta_ext_icd08 %>% 
  select(
    age = `Age Group`, sex = Gender, 
    race = Race, year = Year, 
    death_count = Deaths, population_count = Population) %>%
  mutate(population_count = as.numeric(population_count)) %>% 
  filter(!is.na(race), !is.na(sex), !is.na(year), !is.na(age), age != "Not Stated") %>%
  filter(race != "") %>% 
  mutate(year = as.integer(year)) %>% 
  group_by(race, sex, year) %>% 
  summarise(death_count = sum(death_count), population_count = sum(population_count)) %>%
  mutate(death_rate = 100000 * death_count / population_count)

ext_icd08_rate %>%
  ggplot() + 
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex) + 
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200))


ext_icd09_rate <- dta_ext_icd09 %>% 
select(
  age = `Age Group`, sex = Gender, 
  race = Race, year = Year, 
  death_count = Deaths, population_count = Population) %>%
mutate(population_count = as.numeric(population_count)) %>% 
filter(!is.na(race), !is.na(sex), !is.na(year), !is.na(age), age != "Not Stated") %>%
filter(race != "") %>% 
mutate(year = as.integer(year)) %>% 
group_by(race, sex, year) %>% 
summarise(death_count = sum(death_count), population_count = sum(population_count)) %>%
mutate(death_rate = 100000 * death_count / population_count)

ext_icd09_rate %>%
  ggplot() + 
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex) + 
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200))


ext_icd08_09_rate <- rbind(ext_icd08_rate, ext_icd09_rate)


ext_icd10_rate <- dta_ext_icd10 %>% 
select(age = `Age Group`, year = Year, sex = Gender, race = Race, hispanic = `Hispanic Origin`, death_count = Deaths, population_count = Population) %>%
filter(race = !is.na(race), age = !is.na(age), year = !is.na(year), sex = !is.na(sex)) %>%
mutate(
  group = NA,
  group = ifelse(race == "White" & hispanic == "Not Hispanic or Latino", "White Non-Hispanic", group),
  group = ifelse(race == "Black or African American" & hispanic == "Not Hispanic or Latino", "Black Non-Hispanic", group),
  group = ifelse(hispanic == "Hispanic or Latino", "Hispanic", group)
) %>% 
filter(!is.na(group)) %>% 
select(sex, race = group, age, year, death_count, population_count) %>% 
mutate(population_count = as.numeric(population_count)) %>% 
filter(population_count = !is.na(population_count)) %>% 
group_by(sex, race, year) %>% 
summarise(death_count = sum(death_count), population_count = sum(population_count)) %>% 
mutate(death_rate = 100000 * death_count / population_count) 

ext_icd10_rate %>% 
  ggplot() +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  scale_y_log10(breaks = c(1, 2, 5, 10, 20, 50, 100, 200))



tmp_10 <- ext_icd10_rate %>% 
  select(r1 = race, sex, year, death_count, population_count, death_rate) %>% 
  mutate(
    r2 = recode(
    r1, 
    "
    'Black Non-Hispanic' = 'Black';
    'White Non-Hispanic' = 'White';
    'Hispanic' = 'Hispanic/Other'
    "
    )
  ) %>% ungroup() %>% 
  select(race = r2, sex, year, death_count, population_count, death_rate) 




tmp_08_09 <- ext_icd08_09_rate %>%
  select(r1 = race, sex, year, death_count, population_count, death_rate) %>% 
  mutate(
    r2 = recode(
      r1, 
    "
    'Black or African American' = 'Black';
    'White' = 'White';
    'Other Race' = 'Hispanic/Other'
    "
  )
  ) %>% ungroup() %>%
select(race = r2, sex, year, death_count, population_count, death_rate) 

ext_icd_08_09_10_rates <- rbind(tmp_08_09, tmp_10)


ext_icd_08_09_10_rates %>% 
ggplot() +
  geom_line(aes(x = year, y = death_rate, group = race, colour = race, linetype = race)) + 
  facet_wrap(~sex, scales = "free_y") + 
  geom_vline(xintercept = c(1979, 1999)) +
  scale_y_log10(breaks= c(1, 2, 5, 10, 20, 50, 100, 200)) + 
  labs(y = "Death rate from all external causes per 100 000")




# Now to do the above but with suicides and undetermined 





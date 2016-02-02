


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

# all cause 

icd_08_allcause <- read_delim(
  "data/icd_08_year_age_race_gender_allcause.txt",
  delim = "\t",
  col_types = paste(rep("c", 12), collapse = "")
)

icd_08_allcause <- icd_08_allcause %>% 
  filter(Notes == "") %>% 
  select(
    year = Year, 
    age = `Age Group Code`, 
    race = Race,
    sex = Gender,
    death_count = Deaths,
    population_count = Population
  ) %>% 
  filter(population_count != "Not Applicable") %>% 
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
  mutate(icd_class = "icd_08") %>% 
  select(icd_class, year, age, sex, race = r2, cause, death_count, population_count) %>% 
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
  ) 


# all external 


icd_08_allexternal <- read_delim(
  "data/icd_08_year_age_race_gender_external.txt",
  delim = "\t",
  col_types = paste(rep("c", 12), collapse = "")
)

icd_08_allexternal <- icd_08_allexternal %>% 
  filter(Notes == "") %>% 
  select(
    year = Year, 
    age = `Age Group Code`, 
    race = Race,
    sex = Gender,
    death_count = Deaths,
    population_count = Population
  ) %>% 
  filter(population_count != "Not Applicable") %>% 
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
  mutate(icd_class = "icd_08") %>% 
  select(icd_class, year, age, sex, race = r2, cause, death_count, population_count) %>% 
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
  ) 


# Subsequent categories taken from icd_subclass file



icd_08_external <- read_delim(
  "data/icd_08_year_age_race_gender_icd_sub_external.txt",
  delim = "\t",
  col_types = paste(rep("c", 14), collapse = "")
)

# asssault/homicide

icd_08_assault <- icd_08_external %>% 
  filter(`ICD Sub-Chapter Code` == "E960-E969") %>% 
  filter(Notes == "") %>% 
  select(
    year = Year, 
    age = `Age Group Code`, 
    race = Race,
    sex = Gender,
    death_count = Deaths,
    population_count = Population
  ) %>% 
  filter(population_count != "Not Applicable") %>% 
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
  mutate(icd_class = "icd_08") %>% 
  select(icd_class, year, age, sex, race = r2, cause, death_count, population_count) %>% 
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
  ) 
# There's an issue with population counts being set at zero when the associated death count is zero# 
# this needs to be corrected with a merge from all cause mort

icd_08_assault <- icd_08_allcause  %>% 
  select(year, age, sex, race, p2 = population_count)  %>% 
  inner_join(icd_08_assault, by = c("year", "age" ,"sex", "race"))  %>% 
  select(icd_class, year, age, sex, race, cause, death_count, population_count = p2)

# motor 

icd_08_vehicle <- icd_08_external %>% 
  filter(`ICD Sub-Chapter Code` == "E810-E819") %>% 
  filter(Notes == "") %>% 
  select(
    year = Year, 
    age = `Age Group Code`, 
    race = Race,
    sex = Gender,
    death_count = Deaths,
    population_count = Population
  ) %>% 
  filter(population_count != "Not Applicable") %>% 
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
  mutate(cause = "vehicle") %>% 
  mutate(icd_class = "icd_08") %>% 
  select(icd_class, year, age, sex, race = r2, cause, death_count, population_count) %>% 
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
  ) 
# There's an issue with population counts being set at zero when the associated death count is zero# 
# this needs to be corrected with a merge from all cause mort

icd_08_vehicle <- icd_08_allcause  %>% 
  select(year, age, sex, race, p2 = population_count)  %>% 
  inner_join(icd_08_vehicle, by = c("year", "age" ,"sex", "race"))  %>% 
  select(icd_class, year, age, sex, race, cause, death_count, population_count = p2)


# intentional self harm
icd_08_intent <- icd_08_external %>% 
  filter(`ICD Sub-Chapter Code` == "E950-E959") %>% 
  filter(Notes == "") %>% 
  select(
    year = Year, 
    age = `Age Group Code`, 
    race = Race,
    sex = Gender,
    death_count = Deaths,
    population_count = Population
  ) %>% 
  filter(population_count != "Not Applicable") %>% 
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
  mutate(icd_class = "icd_08") %>% 
  select(icd_class, year, age, sex, race = r2, cause, death_count, population_count) %>% 
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
  ) 
# There's an issue with population counts being set at zero when the associated death count is zero# 
# this needs to be corrected with a merge from all cause mort

icd_08_intent <- icd_08_allcause  %>% 
  select(year, age, sex, race, p2 = population_count)  %>% 
  inner_join(icd_08_intent, by = c("year", "age" ,"sex", "race"))  %>% 
  select(icd_class, year, age, sex, race, cause, death_count, population_count = p2)



# undetermined self harm


icd_08_undetermined <- icd_08_external %>% 
  filter(`ICD Sub-Chapter Code` == "E980-E989") %>% 
  filter(Notes == "") %>% 
  select(
    year = Year, 
    age = `Age Group Code`, 
    race = Race,
    sex = Gender,
    death_count = Deaths,
    population_count = Population
  ) %>% 
  filter(population_count != "Not Applicable") %>% 
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
  mutate(icd_class = "icd_08") %>% 
  select(icd_class, year, age, sex, race = r2, cause, death_count, population_count) %>% 
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
  ) 


# There's an issue with population counts being set at zero when the associated death count is zero# 
# this needs to be corrected with a merge from all cause mort

icd_08_undetermined <- icd_08_allcause  %>% 
  select(year, age, sex, race, p2 = population_count)  %>% 
  inner_join(icd_08_undetermined, by = c("year", "age" ,"sex", "race"))  %>% 
  select(icd_class, year, age, sex, race, cause, death_count, population_count = p2)



# Derived ones: 

# None external 

tmp1 <- icd_08_allexternal %>% select(-population_count)
tmp2 <- icd_08_allcause %>% select(-population_count)
tmp4 <- icd_08_allcause %>% select(-cause, -death_count)
tmp3 <- bind_rows(tmp1, tmp2)

rm(tmp1, tmp2)

icd_08_external_nonexternal_allcause <- tmp3 %>% 
  spread(key = cause, value = death_count) %>% 
  mutate(non_external = all_cause - all_external) %>% 
  inner_join(tmp4, by = c("icd_class" ,"year", "age", "sex", "race")) %>% 
  gather(key = "cause", value = "death_count", all_cause, all_external, non_external) %>% 
  select(icd_class, year, age, sex, race, cause, death_count, population_count)

rm(tmp3, tmp4)

# undetermined and intentional 


tmp1 <- icd_08_undetermined %>% select(-population_count)
tmp2 <- icd_08_intent %>% select(-population_count)
tmp3 <- icd_08_allcause %>% select(-cause, -death_count)
tmp4 <- bind_rows(tmp1, tmp2)

rm(tmp1, tmp2)

icd_08_both_undetermined_intentional <- tmp4 %>% 
  spread(key = cause, value = death_count) %>% 
  mutate(intentional_or_undetermined_self_harm = intentional_self_harm + undetermined) %>% 
  inner_join(tmp3, by = c("icd_class" ,"year", "age", "sex", "race")) %>% 
  gather(key = "cause", value = "death_count", intentional_self_harm, undetermined, intentional_or_undetermined_self_harm) %>% 
  select(icd_class, year, age, sex, race, cause, death_count, population_count)

rm(tmp3, tmp4)



# binding

icd_08_8cause <- icd_08_both_undetermined_intentional %>% 
  bind_rows(icd_08_assault) %>% 
  bind_rows(icd_08_external_nonexternal_allcause) %>% 
  bind_rows(icd_08_vehicle) 

write_csv(icd_08_8cause, path = "data/tidied/icd_08_8cause.csv")

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
  mutate(cause = "vehicle") %>% 
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




# Binding of icd_08 and icd_09 together -----------------------------------

icd_09_8cause <- read_csv(file = "data/tidied/icd_09_8cause.csv", col_types = "cccccccc")
icd_08_8cause <- read_csv(file = "data/tidied/icd_08_8cause.csv", col_types = "cccccccc")

icd_89_8cause <- icd_08_8cause %>% bind_rows(icd_09_8cause)

write_csv(icd_89_8cause, path = "data/tidied_icd_08_09_joined_8cause.csv")

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



# icd 8, 9 and 10, 8 fold categorisation ----------------------------------


icd_09_8cause <- read_csv(file = "data/tidied/icd_09_8cause.csv", col_types = "cccccccc")
icd_08_8cause <- read_csv(file = "data/tidied/icd_08_8cause.csv", col_types = "cccccccc")

icd_89_8cause <- icd_08_8cause %>% bind_rows(icd_09_8cause)

icd_10_8cause <- read_csv(file = "data/tidied/icd_10_8cause.csv", col_types = "cccccccc")

icd_89_8cause <- icd_89_8cause %>% mutate(
  age = factor(
    age, 
    levels = c("1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-34", 
               "35-44", "45-54", "55-64", "65-74", "75-84", "85+"),
    labels = c("< 1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-34", 
               "35-44", "45-54", "55-64", "65-74", "75-84", "85+"),
    ordered = T)
) %>% 
  arrange(year, race, sex, age) 


icd_10_8cause_agegrouped <- icd_10_8cause %>% mutate(
  age2 = cut(
    as.numeric(age), right = F, 
    breaks = c(0, 1, 5, 10, 15, 20, 25, 35, 45, 55, 65, 75, 85, 99), 
    ordered_result = T,
    labels = 
      c( "< 1", "1-4", "5-9", "10-14", "15-19", "20-24", 
         "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+")         
    
  ),
  age = as.factor(age2)
) %>% 
  group_by(icd_class, year, race, sex, age, cause) %>%
  mutate(
    death_count = as.numeric(death_count), 
    population_count = as.numeric(population_count)
  ) %>% 
  summarise(
    death_count = sum(death_count),
    population_count = sum(population_count)
  ) %>% 
  mutate(
    death_count = as.character(death_count), 
    population_count = as.character(population_count)
  )  


icd_8910_8cause <- icd_89_8cause %>% bind_rows(icd_10_8cause_agegrouped) 

# 85 + not an available category for ICD 10 (using underlying causes) 
# so filtering it out

icd_8910_8cause <- icd_8910_8cause %>% 
  filter(age != "85+") %>% 
  mutate(death_count = as.numeric(death_count), population_count = as.numeric(population_count))




write_csv(icd_8910_8cause, path = "data/tidied/icd_8_to_10_8fold.csv") 


# Create a file containing a subset of the existing 8 fold data 

# Black, death from assault/homicide, age group 15 to 34

# 

# to assess the influence (or otherwise) of party in power on mortality trends 


rm(list=ls())

require(readr)
require(plyr)
require(tidyr)
require(dplyr)
require(stringr)
require(car)

require(xts)
require(zoo)
require(mFilter)
require(lubridate)

require(ggplot2)
require(lattice)
require(latticeExtra)
require(RColorBrewer)
require(grid)



dta <-  read_csv("data/tidied/icd_8_to_10_8fold.csv")


dta_subset <- dta %>% 
  filter(race =="black") %>% 
  filter(age %in% c("15-19", "20-24", "25-34")) %>% 
  filter(cause == "assault") %>%
  select(year, sex, death_count, population_count) %>% 
  group_by(year, sex) %>% 
  summarise(
    death_count = sum(death_count), 
    population_count = sum(population_count)
  ) %>% 
  mutate(rep = ifelse(year %in% c(2010:2014, 1994:2001, 1978:1981, 1968:1969), 1, 0)) 


dta_subset %>% mutate(
  k1970 = ifelse( year > 1970, year - 1970, 0), 
  k1978 = ifelse( year > 1978, year - 1978, 0),
  k1982 = ifelse( year > 1982, year - 1982, 0),
  k1994 = ifelse( year > 1994, year - 1994, 0), 
  k2002 = ifelse( year > 2002, year - 2002, 0),
  k2010 = ifelse( year > 2010, year - 2010, 0)
) %>% 
  mutate(
    death_rate = death_count / population_count,
    lg10mr = log(death_rate, 10) 
    ) -> knotted 



models <- dlply(
  knotted, 
  .(sex), 
  function(x) lm(lg10mr ~ year + k1970 + k1978 + k1982 + k1994+ k2002+ k2010, data = x)
)

# to summarise all
sink(file = "support/black_assault_subset.txt", split = T)
llply(models, summary)
sink()



# Exercise 2 --------------------------------------------------------------

# Those trends are bumpy.  But to run at least a first approximation, let's do two detrending exercises.
# 
# 
# E1. Let's generate a 6th order polynomial function of time.  
# Create 6 variables, where t1=year-1968, t2=t1^2, t3=t1^3... t6=t1^6.  Then run the following model:
#   
#   y = a + b1*t + b2*t2 +... b6*t6 + e     [2]
# 
# recover the residuals of y (again, for each race-sex (4), age-group (adults), and mortality cause (3)). 
# Then run this regression:
#   
#   res_y = a + b1*rep + e     [3]
# 
# where rep is the Republican dummy.  Recover the fitted values and plot them.
# 

dta_subset %>% 
  mutate(death_rate = death_count / population_count, 
         lg10mr = log(death_rate, 10)) %>% 
  dlply(
    ., 
    .(sex), 
    function(x) lm(lg10mr ~ poly(year, 6), data = x)
  ) -> models_poly


  do_both_stages <- function(x){
    stage1 <- lm(lg10mr ~ poly(year, 6), data =x)
    
    residuals <- stage1$residuals
    
    df <- data.frame(res_y = residuals, rep = x$rep)
    
    stage2 <- lm(res_y ~ rep, data = df)
    
    output <- list(stage1 = stage1, stage2 = stage2)
  }

  both_stages  <- dta_subset %>% 
    mutate(death_rate = death_count / population_count, 
           lg10mr = log(death_rate, 10)) %>% 
    dlply(., .(sex), do_both_stages)
  
  

ldply(both_stages, function(x) {x[["stage2"]][["fitted.values"]]}) %>% 
  tbl_df %>% 
  gather(key = obs, value = fitted, -sex) %>% 
  mutate(obs = as.numeric(obs)) %>% 
  arrange(sex) %>% 
  ggplot(.) + 
  geom_point(aes(x = obs, y = fitted, colour = sex, group = sex))





# Exercise 2.2 ------------------------------------------------------------

# E2. Detrend y using the Hodrick-Prescott (HP) filter (there should be a R package that does that). 
# There are two conventional values for the smoothing parameter  for annual data: 6.25 and 100. 
# Please use both since they usually do vary the detrending a lot.  
# Once you detrended y using HP, then run equation 3, where res_y will be the 
# detrended values of y using HP, and plot the fitted values.

# Need to first convert the data to a timeseries format

# actually two separate dataseries - one for males and one for females

fn <- function(x){
  y <- data.frame(year = as.Date(as.character(x$year), format = "%Y"), value = x$death_rate)
  output <- xts(y$value, y$year)
  return(output)
}

dta_list <- dta_subset %>% mutate(death_rate = death_count / population_count) %>%  
  mutate(death_rate = log(death_rate, 10)) %>% 
  dlply(., .(sex), fn)


dta_list_detrended_100 <- llply(dta_list, hpfilter, freq = 100)
dta_list_detrended_6_25 <- llply(dta_list, hpfilter, freq = 6.25)

fn2 <- function(x){
  md_part <- paste0("-", str_split(today(), "\\-", n = 2)[[1]][[2]]) # the filter above adds the month and the day to years,
  # this needs to be removed using the lines above (from lubridate) and below
  year <- dimnames(x$trend)[1][[1]]  %>% str_replace_all(., md_part, "")  %>% as.numeric()
  output <- data.frame(year = year, cycle = x$cycle, trend = x$trend[,1])
  return(output)
}


dta_df_detrended_100 <- ldply(dta_list_detrended_100, fn2) %>% 
  tbl_df %>% rename(sex = .id)

dta_df_detrended_6_25 <- ldply(dta_list_detrended_6_25, fn2) %>% 
  tbl_df %>% rename(sex = .id)

dta_df_detrended_100 <- dta_subset  %>% 
  select(year, rep)  %>% 
  distinct %>% 
  right_join(dta_df_detrended_100) 

dta_df_detrended_6_25 <- dta_subset  %>% 
  select(year, rep)  %>% 
  distinct %>% 
  right_join(dta_df_detrended_6_25) 

write_csv(x = dta_df_detrended_100, path = "data/filtered/black_assault_subset_detrended_100.csv")
write_csv(x = dta_df_detrended_6_25, path = "data/filtered/black_assault_subset_detrended_6_25.csv")



# display model summaries too 

sink(file = "support/hp_100_outputs.txt", split = T)
llply(mdl_hp_100, summary)
sink()

sink(file = "support/hp_6_25_outputs.txt", split = T)
llply(mdl_hp_6_25, summary)
sink()



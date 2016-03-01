
# Exercise 1:

# Let's separate the linear trends by presidential regime. 
# This can be done running a model using linear splines with knots at the year 
# when there was a change of president.  In our case, the knots are the first 
# year for each Republican president.  The model looks like this:
# 
# >   y = a + b1*t + b2*(t-y1)*D1 + b3*(t-y2)*D2 + b4*(t-y3)*D3... + e    [1]
# 
# where y1 is the year of the first knot (say, 1970) and D1 is  a 
# dummy equal to "1" if t is greater or equal to 1970, and "0" otherwise; 
# y2 is the year of the second knot (1977) and D2 a dummy 
# (1=greater or equal to 1977, 0=otherwise), and so on.  To simplify things 
# you can create a variable for each term (t-y_)*D_  where each of 
# them equals "0" before the knot and equal to "t" at and after the knot. 
# So, for example, for the term (t-y1)*D1, X1 will be equal to "0" for years 
# 1968-1969 and equal to 1970, 1971, 1972, 1973... (at and after the knot) --
# the same for the other terms.
# 
# From this regression (run for each race-sex (4), age-group (adults), 
# and mortality cause (3) -- for a total of 12 regressions), recover the fitted values, 
# and plot them using scatter and the fitted line.  Estimate the model using OLS. 
# The models will simply let us know if the linear trends are at least in the 
# right direction or if there is a patterns in the trends...


rm(list=ls())

require(readr)
require(plyr)
require(tidyr)
require(dplyr)
require(stringr)
require(car)

require(mFilter)

require(ggplot2)
require(lattice)
require(latticeExtra)
require(RColorBrewer)
require(grid)



dta <- read_csv("data/tidied/three_cause_agegrouped.csv")

# exclude older adults

dta <- dta %>% filter(age_group != "older adults")

# knots: 1970 1978 1982 1994 2002 2010
#mutate(rep = ifelse(year %in% c(, , , ), 1, 0)) %>% 
#1968:1969  -> 1970
#1978:1981 -> 1982
#1994:2001 -> 2002
#2010:2014 

dta %>% mutate(
  k1970 = ifelse(year < 1970, 0 , year), 
  k1978 = ifelse(year < 1978, 0, year),
  k1982 = ifelse(year < 1982, 0, year),
  k1994 = ifelse(year < 1994, 0 , year), 
  k2002 = ifelse(year < 2002, 0, year),
  k2010 = ifelse(year < 2010, 0, year)
) %>% 
filter(race %in% c("black", "white")) %>% 
mutate(
lg10mr = log(death_rate, 10) 
) -> knotted 

models <- dlply(knotted, .(age_group, race, sex, cause), 
function(x) lm(lg10mr ~ year + k1970 + k1978 + k1982 + k1994+ k2002+ k2010, data = x))

# to summarise all


llply(models, summary)

# To display for each model

write_pdf <- function(x){
nms <- x
dta <- models[[nms]]

pdf(
file = paste0("figures/diagnostic/", nms, ".pdf"),
width = 10, height = 10
)

par(mfrow = c(2,2))

plot(dta)

dev.off()

}

nms <- names(models)
l_ply(nms, write_pdf)


# Exercise 2

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


dta %>%  
  filter(race %in% c("black", "white")) %>% 
  filter(age_group != "older adults") %>% 
  mutate(
    lg10mr = log(death_rate, 10) 
  ) -> dta2 

models_poly <- dlply(dta2, .(age_group, race, sex, cause), 
                function(x) lm(lg10mr ~ poly(year, 6), data = x))

llply(models_poly, summary)


do_both_stages <- function(x){
  stage1 <- lm(lg10mr ~ poly(year, 6), data =x)
  
  residuals <- stage1$residuals
  
  df <- data.frame(res_y = residuals, rep = x$rep)
  
  stage2 <- lm(res_y ~ rep, data =df)
  
  output <- list(stage1 = stage1, stage2 = stage2)
}

both_stages  <- dlply(dta2, .(age_group, race, sex, cause), do_both_stages)

ldply(both_stages, function(x) {x[["stage2"]][["fitted.values"]]}) %>% 
  tbl_df %>% 
  gather(key = obs, value = residual, -age_group, -race, -sex, -cause) %>% 
  mutate(obs = as.numeric(obs)) %>% 
  filter(age_group == "adults") %>% 
  arrange(race, sex, cause) %>% 
  ggplot(.) + 
  geom_point(aes(x = obs, y = residual, group = cause, colour = cause)) + 
  facet_grid(race ~ sex) + 
  geom_hline(aes(y = 0)) + 
  labs(title = "adults")


ldply(both_stages, function(x) {x[["stage2"]][["fitted.values"]]}) %>% 
  tbl_df %>% 
  gather(key = obs, value = residual, -age_group, -race, -sex, -cause) %>% 
  mutate(obs = as.numeric(obs)) %>% 
  filter(age_group == "children and adolescents") %>% 
  arrange(race, sex, cause) %>% 
  ggplot(.) + 
  geom_point(aes(x = obs, y = residual, group = cause, colour = cause)) + 
  facet_grid(race ~ sex) + 
  geom_hline(aes(y = 0)) + 
  labs(title = "children and adolescents")


# E2. Detrend y using the Hodrick-Prescott (HP) filter (there should be a R package that does that). 
# There are two conventional values for the smoothing parameter  for annual data: 6.25 and 100. 
# Please use both since they usually do vary the detrending a lot.  
# Once you detrended y using HP, then run equation 3, where res_y will be the 
# detrended values of y using HP, and plot the fitted values.






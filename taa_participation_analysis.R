# File Name: 	  	taa_participation_analysis.R
# File Purpose: 	Analysis of TAA participation data from DOL
# Author: 	    	Nicholas Bell (nicky.bell@gmail.com)

#### Initial commands ####

# Are you here?
here::i_am("taa_participation_data_wrangling.R")

# Set options
options(tibble.width = Inf)

# Load packages
library(dplyr)
library(ggplot2)

# Load custom functions
source("R/plot_coefficients.R")

# Load data
partic <- read.csv(here::here("data/final/partic.csv"))
pets <- read.csv(here::here("data/final/petitions.csv"))
dta <- left_join(pets, partic, by = c("TAW" = "petition_number")) %>%
  filter(!is.na(county_fips)) %>%
  mutate(median_household_income_1000s = median_household_income/1000,
         Rate = Rate/100,
         petitioner_type = factor(petitioner_type,levels=c("Workers","Company","Union","State"))) %>%
  fastDummies::dummy_cols("petitioner_type", ignore_na = TRUE)

###################
#### FIGURE 1 #####
###################

dta %>%
  count(state_fips, county_fips) %>%
  ggplot(aes(x=n)) + 
  geom_histogram(binwidth=1, colour="black", fill="white") +
  geom_density(alpha=.1, fill="#808080") +
  geom_vline(aes(xintercept = mean(n)), color="black", linetype="dashed", size=1) +
  coord_cartesian(xlim=c(0,50)) +
  labs(x="Number of Petitions",
       y="Count") +
  theme_bw()

###################
#### FIGURE 2 #####
###################

dta %>%
  group_by(state_fips, county_fips) %>%
  summarize(n_workers = sum(Est..No..Workers, na.rm = T)) %>%
  ggplot(aes(x=n_workers)) + 
  geom_histogram(binwidth=100, colour="black", fill="white") +
  geom_vline(aes(xintercept = mean(n_workers)), color="black", linetype="dashed", size=1) +
  coord_cartesian(xlim=c(0,7500)) +
  labs(x="Number of Eligible Workers",
       y="Count") +
  theme_bw()

###################
#### FIGURE 3 #####
###################

dta %>%
  ggplot(aes(x=partic_rate*100)) + 
  geom_histogram(binwidth=2, colour="black", fill="white") +
  geom_vline(aes(xintercept = mean(partic_rate*100, na.rm = T)), color="black", linetype="dashed", size=1) +
  labs(x="Participation Rate (%)",
       y="Count") +
  theme_bw()

##################
#### TABLE 1 #####
##################

tab1 <- purrr::map_dfr(
  list(
    list(
      var = "petitioner_type_Company",
      name = "Filed by Firm"
    ),
    list(
      var = "petitioner_type_Union",
      name = "Filed by Union"
    ),
    list(
      var = "petitioner_type_State",
      name = "Filed by State"
    ),
    list(
      var = "petitioner_type_Workers",
      name = "Filed by Three Workers"
    ),
    list(
      var = "Rate",
      name = "Unemployment Rate"
    ),
    list(
      var = "female",
      name = "Female, Civilian Employed, 16 years +"
    ),
    list(
      var = "black",
      name = "Black, Full-Time Workers"
    ),
    list(
      var = "hispanic",
      name = "Hispanic or Latino, Full-Time Workers"
    ),
    list(
      var = "median_age",
      name = "Median Age, Workers 16-64 Years"
    ),
    list(
      var = "less_hs",
      name = "Less than High School, Civilian Labor Force, 25-64 Years"
    ),
    list(
      var = "hs",
      name = "High School or GED, Civilian Labor Force, 25-64 Years"
    ),
    list(
      var = "some_col",
      name = "Some College, Civilian Labor Force, 25-64 Years"
    ),
    list(
      var = "col",
      name = "College, Civilian Labor Force, 25-64 Years"
    ),
    list(
      var = "median_household_income_1000s",
      name = "Median Household Income ($1000s)"
    ),
    list(
      var = "industry_density",
      name = "Proportion of TAA-Eligible Workers in Industry"
    ),
    list(
      var = "total_density",
      name = "Proportion of TAA-Eligible Workers in County"
    )
  ), \(x) {
    if (!x$var %in% c("median_age", "median_household_income_1000s")) {
      list(
        name = x$name,
        mean = paste0(round(mean(dta[[x$var]]*100, na.rm = T), 2), "%"),
        sd = round(sd(dta[[x$var]]*100, na.rm = T), 2)
      )
    } else {
      list(
        name = x$name,
        mean = as.character(round(mean(dta[[x$var]], na.rm = T), 2)),
        sd = round(sd(dta[[x$var]], na.rm = T), 2)
      )
    }
  })

withr::with_options(list(xtable.include.rownames=FALSE,
                         xtable.table.placement="h",
                         xtable.caption.placement="top"), {
  print(xtable::xtable(tab1,
                       caption = "Means and Standard Deviations of Key Variables",
                       align = c("l", "l", "c", "c")))
})


##################
#### TABLE 3 #####
##################

#  Regressions
dta_reg <- mutate(dta, across(c(Rate, partic_rate, less_hs, hs, some_col, col, female, black, hispanic, industry_density, total_density), ~ .x*100)) |>
  filter(partic_rate < Inf)

econ.push <- lm(partic_rate ~ Rate + less_hs + hs + some_col + col + median_household_income_1000s + median_age, data=dta_reg)

prog <- lm(partic_rate ~ petitioner_type, data=dta_reg)

stigma <- lm(partic_rate ~ female + black + hispanic + industry_density + total_density, data=dta_reg)

full.partic <- lm(partic_rate ~ Rate + less_hs + hs + some_col + col + median_household_income_1000s + median_age + petitioner_type + female + black + hispanic + industry_density + total_density, data=dta_reg)

full.partic.industryfe <- lm(partic_rate ~ Rate + less_hs + hs + some_col + col + median_household_income_1000s + median_age + petitioner_type + female + black + hispanic + industry_density + total_density + factor(NAICS.Description ), data=dta_reg)

full.partic.yearfe <- lm(partic_rate ~ Rate + less_hs + hs + some_col + col + median_household_income_1000s + median_age + petitioner_type + female + black + hispanic + industry_density + total_density + factor(acs_year), data=dta_reg)

full.partic.allfe <- lm(partic_rate ~ Rate + less_hs + hs + some_col + col + median_household_income_1000s + median_age + petitioner_type + female + black + hispanic + industry_density + total_density + factor(NAICS.Description ) + factor(acs_year), data=dta_reg)

# Ready to make it a latex table!
stargazer::stargazer(econ.push, prog, stigma, full.partic, full.partic.industryfe, full.partic.yearfe, full.partic.allfe,
                     title="OLS Regression of TAA-Eligible Worker Characteristics on Participation Rate",
                     header=FALSE,
                     model.names=FALSE,
                     model.numbers=FALSE,
                     column.labels=c("Economic Push","Programmatic","Stigma","Full Model"),
                     column.separate=c(1,1,1,4),
                     star.cutoffs=0.05,
                     table.placement="H",
                     keep.stat=c("adj.rsq","n"),
                     dep.var.caption="",
                     dep.var.labels.include=FALSE,
                     notes.align="l",
                     omit=c("Description","acs.year"),
                     omit.labels=c("Industry FEs","Year FEs"),
                     omit.yes.no=c("",""),
                     covariate.labels = c("Unemployment Rate","\\% Less than HS","\\% HS or GED","\\% Some College","\\% College","Median Household Income (\\$1000s)","Median Age","\\% Filed by Firm","\\% Filed by Union","\\% Filed by State","\\% Female","\\% Black","\\% Hispanic or Latino","\\% Workers in Industry Eligible for TAA","\\% All Workers Eligible for TAA"))

# And coefficients plot
plot_coefficients(full.partic.industryfe,
                  rename_covariates = 
                    list("Rate" = "Unemployment Rate",
                         "less_hs" = "% Less than HS",
                         "hs" = "% HS or GED",
                         "some_col" = "% Some College",
                         "col" = "% College",
                         "median_household_income_1000s" = "Median Household Income (\\$1000s)",
                         "median_age" = "Median Age",
                         "petitioner_typeCompany" = "Filed by Firm",
                         "petitioner_typeUnion" = "Filed by Union",
                         "petitioner_typeState" = "Filed by State",
                         "female" = "% Female",
                         "black" = "% Black",
                         "hispanic" = "% Hispanic or Latino",
                         "industry_density" = "% Workers in Industry Eligible for TAA",
                         "total_density" = "% All Workers Eligible for TAA"),
                  dv_name = "TAA Participation Rate") +
  theme(axis.text.y = element_text(size = 14),
        plot.title = element_text(hjust = .5, size = 20)) +
  ggtitle("Who Participates in TAA?")

# Save
ggsave(here::here("images/taa_participation_coefficients.png"))

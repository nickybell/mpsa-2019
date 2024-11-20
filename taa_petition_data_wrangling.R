# File Name: 	  	taa_petition_data_wrangling.R
# File Purpose: 	Wrangling TAA petition data from DOL
# Author: 	    	Nicholas Bell (nicky.bell@gmail.com)

#### Initial commands ####

# Are you here?
here::i_am("taa_petition_data_wrangling.R")

# Set options
options(tibble.width = Inf)

# Load packages
# library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(blsAPI)
library(acs)

# Set up parallel processing (for BLS API)
# options(parallelly.fork.enable = TRUE) # to fork in RStudio
future::plan("multisession")
progressr::handlers(global = TRUE)

# Petitions Data ----------------------------------------------------------

## This data was provided by FOIA request to the Department of Labor

# Load data
dta_raw <- read.csv("PetitionData31819.csv") #82648 obs.

dta <- 
  dta_raw %>%
  mutate(
    # Format determination date
    Determination.Date = as.Date(Determination.Date, format="%d-%b-%y"),
    # Format ZIP codes
    Zip = str_pad(Zip, 5, pad = "0"),
    # Create a variable for TAW+Suffix
    TAW.Suffix = paste0(trimws(TAW), trimws(Suffix)),
    # Remove commas from est. no. workers
    Est..No..Workers = gsub(",", "", Est..No..Workers),
    Est..No..Workers = as.numeric(Est..No..Workers)) %>%
  filter(
    # Keep only certified petitions
    grepl("C|P", Determ.Code) &
      # Limit to petitions with NAICS codes (this is a quick and dirty way to limit the time frame of the data)
      !is.na(NAICS) & NAICS != 0
  ) %>%
  select(TAW.Suffix:Zip, Worker.Group:Petitioner, NAICS:Determ.Code, Est..No..Workers)# 13599 obs.

# There is one outlying petition from 2015 - not sure what is going on with that, so going to eliminate it
# table(format(dta$Determination.Date, "%Y"))
dta <- dta[dta$Determination.Date != "2005-07-18",] # 13598 obs.

## Drop PR, and VI; correct the "State" variable
dta <- dta[-which(dta$State %in% c("0","PR","VI")),] # 13573 obs.
dta$State[dta$State=="Il"] <- "IL"
dta$State[dta$State=="In"] <- "IN"
dta$State[dta$State=="Az"] <- "AZ"
# length(unique(dta$State)) #51

# Clean up the petitioner type
# Where the petitioner value is ambiguous (e.g. a number), I looked at the actual petition (available as PDFs on DOL website) and made a determination
dta <-
  mutate(dta, petitioner_type = case_when(
    Petitioner %in% c("AFL",
                      "AFLCIO",
                      "AWPPW",
                      "CIC",
                      "CICUBC",
                      "CWA",
                      "GMP",
                      "GMPU",
                      "IAM",
                      "IAMAW",
                      "IAWAW",
                      "IBB",
                      "IBDW",
                      "IBEW",
                      "IBT",
                      "ICWU",
                      "ILWU",
                      "IUAW",
                      "IUE",
                      "IUE-CW",
                      "IUECWA",
                      "IUPAT",
                      "LSW",
                      "SMWIA",
                      "TLC",
                      "UAW",
                      "UBCJA",
                      "UE",
                      "UFCW",
                      "UFCWIU",
                      "union",
                      "Union",
                      "UNION",
                      "UNITE",
                      "USW",
                      "USWA",
                      "UWA",
                      "WU",
                      "AFSCME",
                      "BCTGM",
                      "IAM&AW",
                      "IUCWA",
                      "LIUNA",
                      "SEIU",
                      "UMWA",
                      "45810",
                      "CA") ~ "Union",
    Petitioner %in% c("1571",
                      "18014",
                      "comp",
                      "Comp",
                      "COMP",
                      "Comp.",
                      "Compan",
                      "Company",
                      "OH",
                      "REP",
                      "29628",
                      "40356") | TAW %in% c(62737,
                                            62786,
                                            63204,
                                            63395,
                                            63500,
                                            63760,
                                            64410,
                                            64438,
                                            64666,
                                            64898,
                                            64901,
                                            62247,
                                            62922,
                                            64625,
                                            64726,
                                            65243,
                                            65248) ~ "Company",
    Petitioner %in% c("ONE-ST",
                      "state",
                      "State",
                      "STATE",
                      "State/One-Stop",
                      "48647",
                      "One St",
                      "State/",
                      "94520") | TAW %in% c(62663,
                                            63694,
                                            63717,
                                            65696) ~ "State",
    Petitioner %in% c("wkrs",
                      "Wkrs",
                      "Wks",
                      "Work",
                      "Worker",
                      "Workers",
                      "Wrkrs",
                      "wrks",
                      "Wrks",
                      "WKRS",
                      "wrkrs",
                      "21742",
                      "29574") ~ "Workers",
    TRUE ~ NA_character_))

# Two-digit NAICS code descriptions
naics <- read.csv("naics_2_digit_codes.csv")
dta$NAICS.2.Digit.Code <- as.numeric(substr(dta$NAICS,1,2))
dta_naics <- left_join(dta, naics, by = "NAICS.2.Digit.Code") # 13573 obs.
# sum(is.na(dta_naics$NAICS.Description)) # 24 non-matchers


# Geocoding ---------------------------------------------------------------

# This is v1.74 of SimpleMaps.com US Cities Database - PRO (paid version) and therefore cannot be distributed
geo <- read.csv("uscities.csv")

# Separate out the combined FIPS into separate county and state FIPS
geo$county_fips_orig <- geo$county_fips
geo$county_fips <- str_sub(geo$county_fips_orig,-3,-1)
geo$state_fips <- str_sub(geo$county_fips_orig,-5,-4)

# Has a potentially useful "zips" field that needs to be made long
# max(str_count(geo$zips, "\\s")) #309 meaning 310 zips
geo_zips <- 
  geo %>%
  select(city, state_id, zips, county_fips, state_fips) %>%
  separate(zips, sep = "\\s", into = paste0("zips", 1:310), fill = "right") %>%
  pivot_longer(zips1:zips310, values_to = "zip") |>
  filter(!is.na(zip))

# Merge with petitions data
dta_geo <- left_join(dta_naics, geo_zips, by = c("City" = "city", "State" = "state_id", "Zip" = "zip")) # 13573 obs.
# sum(is.na(dta_geo$county_fips)) # 2182 non-matchers

# Some of the petitions lack a zip code. So, if they have only ONE match in geo, we can use that for geocoding.
geo_one <-
  semi_join(geo,
            geo %>%
              count(city, state_id) %>%
              filter(n == 1))

dta_geo <- left_join(dta_geo, geo_one, by = c("City" = "city", "State" = "state_id"), suffix = c("", "_")) # 13573 obs.

# Adopt the geocoded value if appropriate
dta_geo <- mutate(dta_geo, county_fips = if_else(!is.na(county_fips_) & is.na(county_fips), as.character(county_fips_), county_fips),
                  state_fips = if_else(!is.na(state_fips_) & is.na(state_fips), as.character(state_fips_), state_fips))
# sum(is.na(dta_geo$county_fips)) # 662 non-matchers

# I previously geocoded some unmatchable petitions using Texas A&M geocoder
geo_tam <- purrr::map_dfr(c("field_site_corr_done.csv", "geo.tam2.csv"), read.csv)
# Restrict the geocoded file to only those places with a city match
geo_tam <- geo_tam[geo_tam$naaccrQualType == "CityCentroid" & geo_tam$FeatureMatchingResultType == "Success",]

# Merge
dta_tam <- left_join(dta_geo, select(geo_tam, TAWSuffix, CensusCountyFips, CensusStateFips), by = c("TAW.Suffix" = "TAWSuffix")) # 13573 obs.

# Adopt the geocoded value if appropriate
dta_tam <- mutate(dta_tam, county_fips = if_else(!is.na(CensusCountyFips) & is.na(county_fips), as.character(CensusCountyFips), county_fips),
                  state_fips = if_else(!is.na(CensusStateFips) & is.na(state_fips), as.character(CensusStateFips), state_fips))
# sum(is.na(dta_tam$county_fips)) # 475 non-matchers

# I previously manually geocoded some unmatchable petitions (keep in mind, only geocoding certified petitions)
geo_man <- purrr::map_dfr(c("","2"), \(x) read.csv(paste0("field_site_unmatched", x, ".csv")))

# Merge
dta_man <- left_join(dta_tam, select(geo_man, -City, -State), by = c("TAW.Suffix" = "TAWSuffix")) # 13573 obs.

# Adopt the geocoded value if appropriate
dta_man <- mutate(dta_man, county_fips = if_else(!is.na(County.FIPS) & is.na(county_fips), as.character(County.FIPS), county_fips),
                  state_fips = if_else(!is.na(State.FIPS) & is.na(state_fips), as.character(State.FIPS), state_fips))
# sum(is.na(dta_man$county_fips)) # 361 non-matchers

# Also have to correct some state abbreviations
for (i in dta_man$TAW.Suffix[which(dta_man$Correction == 1)]) {
  # print(dta$State[dta_man$TAW.Suffix == i])
  dta_man$State[dta_man$TAW.Suffix == i] <- geo_man$State[match(i, geo_man$TAWSuffix)]
  # print(dta$State[dta_man$TAW.Suffix == i])
}

# Pad the fips codes
dta_man$county_fips <- str_pad(dta_man$county_fips, 3, pad = "0")
dta_man$state_fips <- str_pad(dta_man$state_fips, 2, pad = "0")

# Remove unneeded cols
dta_man <- select(dta_man, -c(name:Correction))

# Covariates --------------------------------------------------------------

#### Unemployment ####

# Create year-month field
dta_man$yearmon <- format(dta_man$Determination.Date, "%Y-%m")

# To pull from BLS API, need full series number for Local Area Unemployment Statistics
dta_man$bls_series <- paste("LAUCN",dta_man$state_fips, dta_man$county_fips,"0000000003",sep="")

# Limited to 500 queries @ 50 series per day
# Limited to 50 requests per 10 seconds
# https://www.bls.gov/developers/api_faqs.htm#register1
bls_reg_key <- Sys.getenv("BLS_REG_KEY") # set in .Renviron
# Create lists of 50 series (32 queries total)
series_list <- lapply(seq(1, length(unique(dta_man$bls_series)), by = 50), \(i) na.omit(unique(dta_man$bls_series)[i:(i+49)]))

# Parallel calls to API
get_bls <- TRUE
if (get_bls) {
  progressr::with_progress({
    p <- progressr::progressor(steps = length(series_list))
    unempl <- furrr::future_map_dfr(series_list, \(x) {
      p()
      tryCatch({
        blsAPI(payload = list(
          seriesid = x,
          startyear = 2008,
          endyear = 2016,
          registrationKey = bls_reg_key),
          api_version = 2,
          return_data_frame = T)
      },
      error = \(e) {
        message(paste0("Error with BLS API for series ID: ", x))
        message(conditionMessage(e))
        data.frame()
      })
    })
  })
  # This can be a bit of a pain to re-run every time, so saving the output
  write.csv(unempl, "unemployment_rates.csv", row.names = FALSE)
} else {
  message("Skipping BLS API calls (using unemployment_rates.csv instead). Set `get_bls` to TRUE to use API.")
  unempl <- read.csv("unemployment_rates.csv")
}

# Merge
dta_unempl <- 
  unempl %>%
  mutate(yearmon = format(my(paste(substr(unempl$period,2,3), unempl$year, sep = "-")), "%Y-%m")) %>%
  rename(Rate = value,
         bls_series = seriesID) %>%
  select(bls_series, yearmon, Rate) %>%
  right_join(dta_man, by = c("bls_series", "yearmon")) # 13573 obs.

# sum(is.na(dta_unempl$Rate)) #2186 either out of date range or missing FIPS

#### American Community Survey ####

# Set counties as the census geography of interest
county <- geo.make(state="*", county="*")

# No ACS data for < 2009, use 2009 data
dta_unempl$acs_year <- if_else(year(dta_unempl$Determination.Date) <= 2008, 2009, year(dta_unempl$Determination.Date))

# Load ACS key
api.key.install(Sys.getenv("ACS_KEY"))

# Parallel calls to US Census Bureau API
get_acs <- TRUE
if (get_acs) {
  progressr::with_progress({
    p <- progressr::progressor(steps = length(2009:2016))
    acs <- furrr::future_map_dfr(2009:2016, \(yr) {
      p()
      tryCatch({
        out <- acs.fetch(
          endyear = yr,
          span = 5,
          geography = county,
          variable = c("C24030_001","C24030_029","B20005_003","B20005_050","B20005B_003","B20005B_050","B20005I_003","B20005I_050","B23013_001","B23006_005","B23006_012","B23006_019","B23006_026","B19013_001"))
        geo <- data.frame(geography(out))
        geo$county_fips <- str_pad(geo$county, 3, pad = "0")
        geo$state_fips <- str_pad(geo$state, 2, pad = "0")
        acs_df <- cbind(
          geo,
          data.frame(estimate(out)),
          year = yr)
        rownames(acs_df) <- NULL
        return(acs_df)
      },
      error = \(e) {
        message(paste0("Error calling Census Bureau API for ", yr))
        message(conditionMessage(e))
        data.frame()
      })
    })
  })
  write.csv(acs, "acs.csv", row.names = FALSE)
} else {
  message("Skipping ACS API calls (using acs.csv instead). Set `get_acs` to TRUE to use API.")
  acs <- read.csv("acs.csv", colClasses = c("county_fips" = "character", "state_fips" = "character"))
}

acs <- 
  acs %>%
  mutate(
    #Sex for the Civilian Employed Population 16 Years and Over
    female = C24030_029/C24030_001, 
    # % Race of Total Worked full-time, year-round in the past 12 months
    black = (B20005B_003+B20005B_050)/(B20005_003+B20005_050), 
    hispanic = (B20005I_003+B20005I_050)/(B20005_003+B20005_050),
    # Median Age for Workers 16 to 64
    median_age = B23013_001,
    # Educational attainment of civilian labor force 25-64 years old
    less_hs = B23006_005/(B23006_005+B23006_012+B23006_019+B23006_026),
    hs = B23006_012/(B23006_005+B23006_012+B23006_019+B23006_026),
    some_col = B23006_019/(B23006_005+B23006_012+B23006_019+B23006_026),
    col = B23006_026/(B23006_005+B23006_012+B23006_019+B23006_026),
    # Median Household Income in the Past 12 Months (inflation-adjusted for that year, i.e. March and August 2017, but not 2017 to 2016)
    median_household_income = B19013_001) %>%
  select(county_fips, state_fips, year:median_household_income) 

# Merge
dta_acs <- left_join(dta_unempl, acs, by = c("county_fips", "state_fips", "acs_year"= "year"))

# Derived variables -------------------------------------------------------

# 2009 County Business Patterns data (workers per NAICS code)
cbp <- read.csv("2009cbp.csv")

# FIPS codes
cbp <-
  mutate(cbp,
         Combo.FIPS = str_pad(Combo.FIPS, 5, pad = "0"),
         county_fips = str_sub(Combo.FIPS,-3,-1),
         state_fips = str_sub(Combo.FIPS,-5,-4)) %>%
  select(-Combo.FIPS)

# Merge
dta_cbp <- left_join(dta_acs, cbp, by = c("county_fips", "state_fips", "Combo.2.Digit.NAICS" = "CBP.2.Digit.NAICS")) # 13573 obs.

# sum(is.na(dta_cbp$employees)) # 390 missing (most missing FIPS codes)

# Density of TAA-eligible workers - total and in industry
dta_dens <- 
  dta_cbp %>%
  group_by(county_fips, state_fips) %>%
  summarize(total_workers = sum(employees, na.rm = T),
            elig_workers = sum(Est..No..Workers, na.rm=T),
            total_density = min(elig_workers/total_workers, 1)) %>%
  right_join(dta_cbp) %>%
  group_by(county_fips, state_fips, total_density, Combo.2.Digit.NAICS, employees) %>%
  summarize(elig_workers = sum(Est..No..Workers, na.rm=T),
            industry_density = min(elig_workers/employees, 1)) %>%
  right_join(dta_cbp) # 13573 obs.

# Save
write.csv(dta_dens, "data/final/petitions.csv", row.names = FALSE)

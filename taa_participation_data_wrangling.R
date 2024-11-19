# File Name: 	  	taa_participation_data_wrangling.R
# File Purpose: 	Wrangling TAA participation data from DOL
# Author: 	    	Nicholas Bell (nicky.bell@gmail.com)

#### Initial commands ####

# Are you here?
here::i_am("taa_participation_data_wrangling.R")

# Set options
options(tibble.width = Inf)

# Participation Data ----------------------------------------------------------

# This data was provided by FOIA request to the Department of Labor.
# NOTE: participation data is missing if # participating workers is <= 2. This could potentially bias estimates of participation up. Should check to see if those petitions missing participation data have large numbers of affected workers.

# Load data
dol <- read.csv("dol_participants_foia_data.csv") #6044 obs.

# Remove commas from participation numbers and make numeric
dol$partic_pet <- sub(",","",dol$partic_pet)
dol$partic_pet <- as.numeric(dol$partic_pet)

# How many of the participants data eligible worker #s do not match the petitions data?
pet <- read.csv("PetitionData31819.csv") %>%
  mutate(
    Est..No..Workers = gsub(",", "", Est..No..Workers),
    Est..No..Workers = as.numeric(Est..No..Workers)) %>%
  filter(
    # Keep only certified petitions
    grepl("C|P", Determ.Code) &
      # Limit to petitions with NAICS codes (this is a quick and dirty way to limit the time frame of the data)
      !is.na(NAICS) & NAICS != 0
  ) %>%
  group_by(TAW) %>%
  summarize(petitions_eligible_workers = sum(Est..No..Workers, na.rm = T))

# inner_join(dol, pet, by = c("petition_number" = "TAW")) %>%
#   filter(cert_wrkrs_pet != petitions_eligible_workers) # 11 do not match because petitions data has number of eligible workers as 0; this is good because we can simply estimate the participation rate from the participation data.

# Participation rate
dol$partic_rate <- pmin(dol$partic_pet/dol$cert_wrkrs_pet, 1)

# Save
write.csv(dol, "data/final/partic.csv")

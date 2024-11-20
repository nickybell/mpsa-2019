# File Name: 	  	participation_rates.R
# File Purpose: 	Viz of TAA participation over time
# Author: 	    	Nicholas Bell (nicky.bell@gmail.com)

#### Initial commands ####

# Are you here?
here::i_am("taa_participation_data_wrangling.R")

# Set options
options(tibble.width = Inf)

# Load packages
library(dplyr)
library(lubridate)
library(runner)
library(readxl)
library(ggplot2)


# Petition Data -----------------------------------------------------------

# This data comes from the DOL website (more updated than data from FOIA request): https://www.dol.gov/agencies/eta/tradeact/data/petitions-determinations
pets_raw <- read.csv(here::here("PetitionData_20210314.csv")) #85189 obs.

pets <- 
  pets_raw %>%
  mutate(
    # Format determination date and federal fiscal quarter
    Det.Date = mdy(Det.Date),
    quarter = as.character(quarter(Det.Date, with_year = T, fiscal_start = 10)),
   ) %>%
  filter(
    # Keep only certified petitions
    grepl("C|P", Det.Code) &
    Det.Date %within% interval(ymd("2014-01-01"), ymd("2019-12-31")) &
    !(State %in% c(0, "PR"))
  ) # 4761 obs.


# Rolling 3 quarter count of eligible participants (national)
US_by_Quarter <-
  pets %>% 
  group_by(quarter) %>% 
  summarize(eligible = sum(Est..Workers..Obj., na.rm = T))

US_by_Quarter$eligible_run_sum <- 
  runner(
    x = US_by_Quarter$eligible,
    f = function(x) sum(x, na.rm=T),
    k = "3 quarters",
    lag = "1 quarter",
    idx = yq(US_by_Quarter$quarter)
  )


# Participation Data ------------------------------------------------------

# Collate data from https://www.dol.gov/agencies/eta/tradeact/data/participants
partic <- purrr::map_dfr(list.files(here::here("2021_participation_data/"), full.names = TRUE), \(x) {
  suppressMessages(
    read_excel(x, .name_repair = "universal") %>%
      select(matches("...(1|2)$"), matches("State"), matches("TAA\\.*Participants\\.*New"), matches("Training\\.*Participants\\.*New")) %>%
      mutate(quarter = paste(stringr::str_match(x, "_q\\d{1}_(\\d{4})")[1,2], stringr::str_match(x, "_q(\\d{1})_\\d{4}")[1,2], sep = "."))
  )
}) %>%
  filter((State.Abbrv != "PR" | is.na(State.Abbrv)) & (!(...2 %in% c("PR", "Total")) | is.na(...2)))

# Rolling 1 year count of NEW participants (national)
NewTAA_US_by_Quarter <-
  partic %>% 
  group_by(quarter) %>% 
  summarize(newTAA = sum(TAA.Participants..New., na.rm = T),
            newTrain = sum(Training.Participants.New., Training.Participants..New., na.rm = T))

NewTAA_US_by_Quarter$newTAA_run_sum <- 
  runner(
    x = NewTAA_US_by_Quarter$newTAA,
    f = function(x) sum(x, na.rm=T),
    k = "4 quarters",
    idx = yq(NewTAA_US_by_Quarter$quarter)
  )

NewTAA_US_by_Quarter$newTrain_run_sum <- 
  runner(
    x = NewTAA_US_by_Quarter$newTrain,
    f = function(x) sum(x, na.rm=T),
    k = "4 quarters",
    idx = yq(NewTAA_US_by_Quarter$quarter)
  )

# Merge
national <- full_join(US_by_Quarter, NewTAA_US_by_Quarter, by = "quarter")


# Plot --------------------------------------------------------------------

# Convert quarter field to date field (subtract quarter from yq() bc federal fiscal year starts in October)
national$quarter_start <- floor_date(yq(national$quarter)-1, "quarter")

ggplot(data = national[-c(1:3),]) +
  geom_line(mapping = aes(y = eligible_run_sum,
                          x = quarter_start)) +
  geom_line(mapping = aes(y = newTAA_run_sum,
                          x = quarter_start),
            color = "blue") +
  geom_line(mapping = aes(y = newTrain_run_sum,
                          x = quarter_start),
            color = "red3") +
  annotate("text",
           label = "Eligible Workers",
           x = as.Date("2017-08-01"),
           y = 59000,
           size = 3.5) +
  annotate("text",
           label = "New Participants",
           x = as.Date("2017-08-01"),
           y = 23500,
           color = "blue",
           size = 3.5) +
  annotate("text",
           label = "New Trainees",
           x = as.Date("2017-08-01"),
           y = 4000,
           color = "red3",
           size = 3.5) +
  annotate("rect",
           xmin=as.Date("2016-01-01"),
           xmax=as.Date("2017-01-01"),
           ymin=0,
           ymax=130000,
           fill = "lightgoldenrod3",
           alpha = 0.4) +
  annotate("text",
           x = as.Date("2016-07-01"),
           y = 40000,
           label = "\"Reversion 2014\"\nReconsideration Window",
           size = 2.5) +
  scale_y_continuous(limits = c(0,130000), breaks = seq(0,125000,25000), labels = scales::comma) +
  scale_x_date(limits = c(as.Date("2014-10-01"),as.Date("2020-01-01")), date_breaks = "1 year", date_labels = "%Y") +
  labs(y = "Number of Workers",
       title = "TAA Eligible Workers, New Participants, and New Trainees, 2015-2019",
       caption = "Source: U.S. Department of Labor\nData by quarter; eligible workers is over previous three quarters (one quarter lag);\nnew participants and trainees is over previous four quarters (no lag)") +
  theme_bw(base_size = 10) +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey90"),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust=.5),
        plot.subtitle = element_text(hjust=.5))

library(tidyverse)
library(readtext)
library(jsonlite)
library(lubridate)

source("scripts/helpers.R")

parliamentarians <- as_tibble(readtext::readtext("data/members/", verbosity = 0)) %>%
  mutate(doc_id = str_split(doc_id, fixed("?callback=jQuery33107266187344061623_1569968990479&_=1569968990480"))) %>%
  unnest(cols = c(doc_id)) %>%
  filter(doc_id != "") %>%
  mutate(text = str_split(text, fixed("/**/ typeof jQuery33107266187344061623_1569968990479 === 'function' && jQuery33107266187344061623_1569968990479("))) %>%
  unnest(cols = c(text)) %>%
  mutate(text = str_split(text, fixed("});"))) %>%
  unnest(cols = c(text)) %>%
  filter(text != "") %>%
  mutate(text = paste0("[", text, "}]")) %>% # TODO: use only up to this point to export JSON versions of the files
  mutate(contents = vectorize_json(text, flatten = TRUE)) %>%
  select(contents) %>%
  bind_rows(.$contents) %>%
  filter(! is.na(Person.PersonId)) %>%
  select(-one_of(identify_empty_columns(.)))

## more nuanced capture for ministers
ministers <- roles %>%
  filter(GroupingTitleEn == "Cabinet") %>%
  filter(
    ! OrganizationTypeEn %in% 
      c("Province",
        "Municipal Government",
        "Regional Government",
        "Party"
      )
  ) %>%
  remove_extra_columns(.) %>%
  mutate(
    StartDate = date(StartDate),
    EndDate = date(EndDate),
    period_in_office = interval(StartDate, EndDate)
  )

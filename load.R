library(tidyverse)
library(readtext)
library(jsonlite)
library(lubridate)

source("scripts/helpers.R")

parliamentarians_unmodified <- as_tibble(readtext::readtext("data/members/", verbosity = 0)) %>%
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

## Extract any subsetted data frames for easier analysis
roles <- parliamentarians_unmodified %>%
  select(Person.PersonId, Person.Roles) %>%
  unnest(cols = c(Person.Roles)) %>%
  mutate_at(
    c(
      "StartDate",
      "EndDate",
      "PartyStartDate",
      "PartyEndDate",
      "Senator.NominationEndDate",
      "GovernorGeneral.AppointedDate",
      "GovernorGeneral.PublishedDate"
    ),
    date
  ) %>%
  mutate(
    IsCurrent = as.logical(IsCurrent),
    EndDate = case_when(
      is.na(EndDate) & IsCurrent ~ today(),
      TRUE ~ EndDate
    )
  )

professions <- parliamentarians_unmodified %>%
  select(Person.PersonId, Person.Professions) %>%
  unnest(cols = c(Person.Professions))

election_candidates <- parliamentarians_unmodified %>%
  select(Person.PersonId, Person.ElectionCandidates) %>%
  unnest(cols = c(Person.ElectionCandidates)) %>%
  mutate_at(
    c(
      "ElectionDate"
    ),
    date
  )

## Create nested versions of modified extracts to recombine
roles_nested <- roles %>%
  nest(-Person.PersonId) %>%
  rename(roles_cleaned = data)
election_candidates_nested <- election_candidates %>%
  nest(-Person.PersonId) %>%
  rename(election_candidates_cleaned = data)
  

## Create the `parliamentarians` object for analysis
parliamentarians <- parliamentarians_unmodified %>%
  mutate_if(is.list, ~ map(., as_tibble)) %>%
  mutate_at(
    c(
      "Person.DateOfBirth",
      "Person.Death.DateOfDeath",
      "Person.Death.FuneralDate",
      "Person.Death.StateLayFuneralStartDate",
      "Person.Death.StateLayFuneralEndDate"
    ),
    date
  ) %>%
  mutate(
    Person.LifeInterval = interval(Person.DateOfBirth, Person.Death.DateOfDeath),
    Person.Death.StateLayFuneralInterval = interval(Person.Death.StateLayFuneralStartDate, Person.Death.StateLayFuneralEndDate)
  ) %>%
  left_join(roles_nested, by = c("Person.PersonId" = "Person.PersonId")) %>%
  select(-Person.Roles) %>%
  rename(Person.Roles = roles_cleaned) %>%
  left_join(election_candidates_nested, by = c("Person.PersonId" = "Person.PersonId")) %>%
  select(-Person.ElectionCandidates) %>%
  rename(Person.ElectionCandidates = election_candidates_cleaned)

## Clean up a bit (we don't need these variables anymore)
rm(parliamentarians_unmodified)
rm(roles_nested)
rm(election_candidates_nested)

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
    period_in_office = interval(StartDate, EndDate)
  )


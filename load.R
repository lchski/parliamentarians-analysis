library(tidyverse)
library(readtext)
library(jsonlite)
library(lubridate)
library(janitor)

source("scripts/helpers.R")

## map party names to party groupings
## NB: This is contestable! fork and change as necessary for your analysis
simplified_party_mappings = tribble(
  ~party, ~party_simple,
  #--|--|----
  "Liberal Party of Canada","liberal",
  "Liberal","liberal",
  "Liberal Party","liberal",
  "Liberal Progressive","liberal",
  "Conservative Party of Canada","conservative",
  "Conservative (1867-1942)","conservative",
  "Conservative (Historical)","conservative",
  "Liberal-Conservative","conservative",
  "Progressive Conservative","conservative",
  "Progressive Conservative Party","conservative",
  "Conservative","conservative",
  "Nat'l Liberal & Conservative","conservative",
  "Reform Party of Canada","conservative",
  "Canadian Reform Conservative Alliance","conservative",
  "National Government","conservative",
  "Unionist","conservative",
  "New Democratic Party","ccf/ndp",
  "Co-operative Commonwealth Federation","ccf/ndp",
  "Bloc Québécois","bq",
  "Social Credit Party of Canada","socred",
  "Ralliement des créditistes","socred",
  "Progressive","progressive",
  "United Farmers of Alberta","progressive",
  "Green Party of Canada","green",
  "Independent","independent",
  "Independent Liberal","independent",
  "Independent Conservative","independent",
  "Independent Progressive Conservative","independent",
)

parliamentarians_raw <- as_tibble(readtext::readtext("data/source/lop/parliamentarians/", verbosity = 0)) %>%
  mutate(doc_id = str_split(doc_id, fixed("?callback=1"))) %>%
  unnest(cols = c(doc_id)) %>%
  filter(doc_id != "") %>%
  mutate(text = str_split(text, fixed("/**/ typeof 1 === 'function' && 1("))) %>%
  unnest(cols = c(text)) %>%
  mutate(text = str_split(text, fixed("});"))) %>%
  unnest(cols = c(text)) %>%
  filter(text != "") %>%
  mutate(text = paste0("[", text, "}]")) %>% # TODO: use only up to this point to export JSON versions of the files
  mutate(contents = vectorize_json(text, flatten = TRUE)) %>%
  unnest(contents) %>%
  clean_names %>%
  filter(! is.na(person_person_id)) %>%
  select(-one_of(identify_empty_columns(.)))

## Extract any subsetted data frames for easier analysis
roles <- parliamentarians_raw %>%
  select(person_person_id, person_roles) %>%
  unnest(cols = c(person_roles)) %>%
  clean_names %>%
  mutate_at(
    c(
      "start_date",
      "end_date",
      "party_start_date",
      "party_end_date",
      "senator_nomination_end_date",
      "governor_general_appointed_date",
      "governor_general_published_date"
    ),
    date
  ) %>%
  mutate(
    is_current = as.logical(is_current),
    end_date = case_when(
      is.na(end_date) & is_current ~ today(),
      TRUE ~ end_date
    ),
    period_in_role = interval(start_date, end_date)
  ) %>%
  mutate_at(
    c(
      "port_folio_en"
    ),
    trimws
  ) %>%
  left_join(simplified_party_mappings, by = c("party_en" = "party"))

professions <- parliamentarians_raw %>%
  select(person_person_id, person_professions) %>%
  unnest(cols = c(person_professions)) %>%
  clean_names

election_candidates <- parliamentarians_raw %>%
  select(person_person_id, person_election_candidates) %>%
  unnest(cols = c(person_election_candidates)) %>%
  clean_names %>%
  mutate_at(
    c(
      "election_date"
    ),
    date
  ) %>%
  mutate(
    is_win = result_long_en == "Elected"
  ) %>%
  left_join(simplified_party_mappings, by = c("party_name_en" = "party"))
  

## Create nested versions of modified extracts to recombine
roles_nested <- roles %>%
  nest(-person_person_id) %>%
  rename(roles_cleaned = data)
election_candidates_nested <- election_candidates %>%
  nest(-person_person_id) %>%
  rename(election_candidates_cleaned = data)
  

## Create the `parliamentarians` object for analysis
parliamentarians <- parliamentarians_raw %>%
  mutate_if(is.list, ~ map(., as_tibble)) %>%
  mutate_at(
    c(
      "person_date_of_birth",
      "person_death_date_of_death",
      "person_death_funeral_date",
      "person_death_state_lay_funeral_start_date",
      "person_death_state_lay_funeral_end_date"
    ),
    date
  ) %>%
  mutate(
    person_life_interval = interval(person_date_of_birth, person_death_date_of_death),
    person_death_state_lay_funeral_interval = interval(person_death_state_lay_funeral_start_date, person_death_state_lay_funeral_end_date)
  ) %>%
  left_join(roles_nested, by = c("person_person_id" = "person_person_id")) %>%
  select(-person_roles) %>%
  rename(person_roles = roles_cleaned) %>%
  left_join(election_candidates_nested, by = c("person_person_id" = "person_person_id")) %>%
  select(-person_election_candidates) %>%
  rename(person_election_candidates = election_candidates_cleaned)

## Clean up a bit (we don't need these variables anymore)
rm(parliamentarians_raw)
rm(roles_nested)
rm(election_candidates_nested)

## more nuanced capture for ministers
ministers <- roles %>%
  filter(grouping_title_en %in% c("Cabinet", "House of Commons Roles")) %>%
  filter(
    ! organization_type_en %in% 
      c("Province",
        "Municipal Government",
        "Regional Government",
        "Party"
      )
  ) %>%
  filter(
    str_detect(name_en, paste0(c(
      "Minister",
      "Minister of State",
      "Associate Minister",
      "Secretary of State",
      "Parliamentary Secretary",
      "Parliamentary Assistant"
    ), collapse = "|"))
  ) %>%
  filter(
    ! str_detect(name_en, "Shadow")
  ) %>%
  remove_extra_columns(.) %>%
  mutate(
    period_in_office = period_in_role,
    in_cabinet =
      str_detect(name_en, "Minister") &
      ! str_detect(name_en, "Secretary") &
      ! str_detect(name_en, "of State"),
    in_ministry =
      str_detect(name_en, "Minister") &
      ! str_detect(name_en, "Secretary")
  ) %>%
  left_join(
    parliamentarians %>%
      select(person_person_id, person_display_name, person_gender)
  )

## Cabinet Size! Replicating: https://lop.parl.ca/sites/ParlInfo/default/en_CA/People/primeMinisters/Cabinet
cabinet_size_by_lop_shuffle <- read_csv("data/lop-primeministers-cabinet.csv") %>%
  clean_names %>%
  rename(
    shuffle_date = cabinet_shuffle_date
  ) %>%
  filter(! is.na(cabinet_size)) %>% ## TODO: instead, extract the ministry # and parliament # and dates from these rows, then `fill` down`
  mutate(
    shuffle_date = date(shuffle_date)
  )

ministries <- read_tsv("data/wikipedia-ministries.tsv", skip = 1) %>%
  mutate(
    ministry = as.numeric(gsub("([0-9]+).*$", "\\1", ministry)),
    start_date = as_date(parse_date_time(start_date, c("%B %d, %Y"))),
    end_date = as_date(parse_date_time(end_date, c("%B %d, %Y"))),
    end_date = if_else(is.na(end_date), today(), end_date)
  ) %>%
  select(-duration) %>%
  left_join(simplified_party_mappings) %>%
  left_join(
    (
      roles %>%
        filter(name_en == "Prime Minister" & organization_type_en == "Ministry") %>%
        select(organization_acronym_en, person_person_id, person_role_id, notes_en, period_in_role) %>%
        mutate(organization_acronym_en = as.numeric(organization_acronym_en))
    ),
    by = c("ministry" = "organization_acronym_en")
  )

members <- roles %>%
  filter(name_en == "Constituency Member") %>%
  filter(organization_type_en == "Constituency") %>%
  remove_extra_columns() %>%
  left_join(
    parliamentarians %>%
      select(person_person_id, person_display_name, person_gender)
  ) %>%
  select(-party_organization_id:-party_end_date, -party_simple)



parliaments <- read_csv("data/lop-parliament-key-dates.csv") %>%
  clean_names %>%
  mutate(parliament = lag(term)) %>%
  select(parliament, everything()) %>%
  filter(! is.na(duration)) %>% ## get rid of the parliament header rows from the Excel
  mutate(parliament = as.integer(str_remove(parliament, "Parliament: "))) %>%
  separate(term, c("term_start", "term_end"), " - ") %>%
  mutate(
    first_sitting = term_start,
    dissolution = ifelse(is.na(term_end), today(), term_end)
  ) %>%
  mutate(
    interval_from_election_to_first_budget = interval(general_election_date, date_of_first_budget),
    days_to_budget_from_election = time_length(interval_from_election_to_first_budget, "days"),
    interval_from_first_sitting_to_first_budget = interval(first_sitting, date_of_first_budget),
    days_to_budget_from_first_sitting = time_length(interval_from_first_sitting_to_first_budget, "days"),
    interval_from_returns_to_dissolution = interval(return_of_the_writs, dissolution),
    days_to_dissolution_from_returns = time_length(interval_from_returns_to_dissolution, "days")
  )

library(tidyverse)
library(readtext)
library(jsonlite)
library(lubridate)

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
    ),
    period_in_role = interval(StartDate, EndDate)
  ) %>%
  mutate_at(
    c(
      "PortFolioEn"
    ),
    trimws
  ) %>%
  left_join(simplified_party_mappings, by = c("PartyEn" = "party"))

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
  ) %>%
  mutate(
    IsWin = ResultLongEn == "Elected"
  ) %>%
  left_join(simplified_party_mappings, by = c("PartyNameEn" = "party"))
  

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
  filter(GroupingTitleEn %in% c("Cabinet", "House of Commons Roles")) %>%
  filter(
    ! OrganizationTypeEn %in% 
      c("Province",
        "Municipal Government",
        "Regional Government",
        "Party"
      )
  ) %>%
  filter(
    str_detect(NameEn, paste0(c(
      "Minister",
      "Minister of State",
      "Associate Minister",
      "Secretary of State",
      "Parliamentary Secretary",
      "Parliamentary Assistant"
    ), collapse = "|"))
  ) %>%
  filter(
    ! str_detect(NameEn, "Shadow")
  ) %>%
  remove_extra_columns(.) %>%
  mutate(
    period_in_office = period_in_role,
    in_cabinet =
      str_detect(NameEn, "Minister") &
      ! str_detect(NameEn, "Secretary") &
      ! str_detect(NameEn, "of State"),
    in_ministry =
      str_detect(NameEn, "Minister") &
      ! str_detect(NameEn, "Secretary")
  ) %>%
  left_join(
    parliamentarians %>%
      select(Person.PersonId, Person.DisplayName, Person.Gender)
  )

## Cabinet Size! Replicating: https://lop.parl.ca/sites/ParlInfo/default/en_CA/People/primeMinisters/Cabinet
cabinet_size_by_lop_shuffle <- read_csv("data/lop-primeministers-cabinet.csv") %>%
  rename(
    shuffle_date = `Cabinet Shuffle Date`,
    prime_minister = `Prime Minister`,
    political_affiliation = `Political Affiliation`,
    cabinet_size = `Cabinet Size`,
    ministry_size = `Ministry Size`
  ) %>%
  filter(! is.na(cabinet_size)) %>%
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
        filter(NameEn == "Prime Minister" & OrganizationTypeEn == "Ministry") %>%
        select(OrganizationAcronymEn, Person.PersonId, PersonRoleId, NotesEn, period_in_role) %>%
        mutate(OrganizationAcronymEn = as.numeric(OrganizationAcronymEn))
    ),
    by = c("ministry" = "OrganizationAcronymEn")
  )

members <- roles %>%
  filter(NameEn == "Constituency Member") %>%
  filter(OrganizationTypeEn == "Constituency") %>%
  remove_extra_columns() %>%
  left_join(
    parliamentarians %>%
      select(Person.PersonId, Person.DisplayName, Person.Gender)
  )



parliaments <- read_csv("data/lop-parliament-key-dates.csv") %>%
  select(
    parliament = `Parliament`,
    key_dates = `Key Dates`
  ) %>%
  mutate(
    parliament = as.numeric(gsub("([0-9]+).*$", "\\1", parliament)),
    key_dates = str_replace(key_dates, "1st", "")
  ) %>%
  separate(
    key_dates,
    c(NA, "writs_issued", "general_election", "writs_returned", "first_sitting", "first_budget", "dissolution"),
    "[A-Za-z ]*: "
  ) %>%
  mutate(
    dissolution = ifelse(parliament == 25, first_budget, dissolution),
    first_budget = ifelse(parliament == 25, NA, first_budget)
  ) %>%
  mutate_at(
    c(
      "writs_issued", "general_election", "writs_returned", "first_sitting", "first_budget", "dissolution"
    ),
    date
  ) %>%
  mutate(
    interval_from_election_to_first_budget = interval(general_election, first_budget),
    days_to_budget_from_election = time_length(interval_from_election_to_first_budget, "days"),
    interval_from_first_sitting_to_first_budget = interval(first_sitting, first_budget),
    days_to_budget_from_first_sitting = time_length(interval_from_first_sitting_to_first_budget, "days"),
    interval_from_returns_to_dissolution = interval(writs_returned, dissolution),
    days_to_dissolution_from_returns = time_length(interval_from_returns_to_dissolution, "days")
  )
